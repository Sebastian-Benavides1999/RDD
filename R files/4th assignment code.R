rm(list= ls())

#Instalar y cargar paquetes relevantes
load.lib <- c('PerformanceAnalytics','xtable','stargazer', 'ggpubr','xts',"readxl","data.table","lubridate",'tidyr','rddensity','estimatr','rdrobust',
              'dplyr','ggplot2','forecast','RColorBrewer','quadprog','NMOF','fBonds','cvar','mFilter',"x13binary","seasonal","lmtest","RCurl") 
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib,require,character=TRUE)

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
#Convencion de notacion cientifica
options(scipen=10000)
col<-brewer.pal(6,"Blues")

#Seleccion de directorio
dir <- 'C:/Users/sebit/Documents/GitHub/RDD/Data'
setwd(dir)


#Importacion de datos.
Datos=as.data.table(read.table("hansen_dwi.csv",header = T,sep=","))

#Point 3: Dummy variables
Datos$D <- as.numeric(Datos$bac1 >= 0.08)
Datos$bac1 = Datos$bac1

Datos1<- Datos[bac1>=0.03 & bac1<=0.13]
Datos2<- Datos[bac1>=0.055 & bac1<=0.105]

#Point 4: Manipulation

mccrary=rddensity(Datos$bac1, c = 0.08,all=T)
rdplotdensity(mccrary, Datos$bac1)


hist1<-ggplot(Datos,aes(x=Datos$bac1))+
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue",bins = 100)+
  geom_density(col=col[6],size=1)+
  geom_vline(xintercept =c(0.08) ,
             colour = c('indianred1'), linetype ="longdash", size = .6)+
  xlab('Blood alcohol level') +ylab('Density (%)')+
  theme(axis.title.x =element_text(size=13,  family="serif"),
        panel.background = element_blank(),
        panel.border = element_rect( fill=NA,colour = "black"),
        axis.text.x = element_text(angle = 90))
hist1

#Point 5: Covariate balance
balance_1 <- rdrobust(y = Datos1$male,
                      x = Datos1$bac1, c = 0.08,kernel = 'uniform',all = T)
balance_2 <- rdrobust(y = Datos1$white,
                      x = Datos1$bac1, c = 0.08,kernel = 'uniform',all = T)
balance_3 <- rdrobust(y = Datos1$aged,
                      x = Datos1$bac1, c = 0.08,kernel = 'uniform',all = T)
balance_4 <- rdrobust(y = Datos1$acc,
                      x = Datos1$bac1, c = 0.08,kernel = 'uniform',all= T)


balance_1_1 <- lm_robust(male ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                  data = Datos)
balance_2_1 <- lm_robust(white ~ D + bac1 + D*bac1+ male+ aged+ acc, 
                         data = Datos)
balance_3_1 <- lm_robust(aged ~ D + bac1 + D*bac1+ white+ male +acc, 
                         data = Datos)
balance_4_1 <- lm_robust(acc ~ D + bac1 + D*bac1+ white+ aged+ male, 
                         data = Datos)

balance_1_2 <- lm(male ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                         data = Datos)
balance_2_2 <- lm(white ~ D + bac1 + D*bac1+ male+ aged+ acc, 
                         data = Datos)
balance_3_2 <- lm(aged ~ D + bac1 + D*bac1+ white+ male +acc, 
                         data = Datos)
balance_4_2 <- lm(acc ~ D + bac1 + D*bac1+ white+ aged+ male, 
                         data = Datos)



#Point 6: Graphs

categories = Datos[bac1<=0.16]

malemeans <- split(categories$male, cut(categories$bac1, 100)) %>% 
  lapply(mean) %>% 
  unlist()

whitemeans <- split(categories$white, cut(categories$bac1, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agedmeans <- split(categories$aged, cut(categories$bac1, 100)) %>% 
  lapply(mean) %>% 
  unlist()

accmeans <- split(categories$acc, cut(categories$bac1, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agg_male_data <- data.frame(male = malemeans, bac1 = seq(0.0016,0.16, by = 0.0016))

agg_white_data <- data.frame(white = whitemeans, bac1 = seq(0.0016,0.16, by = 0.0016))

agg_aged_data <- data.frame(aged = agedmeans, bac1 = seq(0.0016,0.16, by = 0.0016))

agg_acc_data <- data.frame(acc = accmeans, bac1 = seq(0.0016,0.16, by = 0.0016))

#plotting

#LINEAR

male_l=ggplot(categories, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = agg_male_data) +
  stat_smooth(aes(bac1, male, group = D), method = "lm") +
  geom_vline(xintercept = 0.08)

white_l=ggplot(categories, aes(bac1, white)) +
  geom_point(aes(x = bac1, y = white), data = agg_white_data) +
  stat_smooth(aes(bac1, white, group = D), method = "lm") +
  geom_vline(xintercept = 0.08)

aged_l=ggplot(categories, aes(bac1, aged)) +
  geom_point(aes(x = bac1, y = aged), data = agg_aged_data) +
  stat_smooth(aes(bac1, aged, group = D), method = "lm") +
  geom_vline(xintercept = 0.08)

acc_l=ggplot(categories, aes(bac1, acc)) +
  geom_point(aes(x = bac1, y = acc), data = agg_acc_data) +
  stat_smooth(aes(bac1, acc, group = D), method = "lm") +
  geom_vline(xintercept = 0.08)

ggarrange(male_l,white_l,aged_l,acc_l,ncol=2,nrow=2)


#QUADRATIC
male_q=ggplot(categories, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = agg_male_data) +
  stat_smooth(aes(bac1, male, group = D), formula = y ~ x + I(x^2),method = "lm") +
  geom_vline(xintercept = 0.08)

white_q=ggplot(categories, aes(bac1, white)) +
  geom_point(aes(x = bac1, y = white), data = agg_white_data) +
  stat_smooth(aes(bac1, white, group = D), formula = y ~ x + I(x^2),method = "lm") +
  geom_vline(xintercept = 0.08)


aged_q=ggplot(categories, aes(bac1, aged)) +
  geom_point(aes(x = bac1, y = aged), data = agg_aged_data) +
  stat_smooth(aes(bac1, aged, group = D), formula = y ~ x + I(x^2), method = "lm") +
  geom_vline(xintercept = 0.08)

acc_q=ggplot(categories, aes(bac1, acc)) +
  geom_point(aes(x = bac1, y = acc), data = agg_acc_data) +
  stat_smooth(aes(bac1, acc, group = D), formula = y ~ x + I(x^2),method = "lm") +
  geom_vline(xintercept = 0.08)

ggarrange(male_q,white_q,aged_q,acc_q,ncol=2,nrow=2)

#Point 7 RD model:

Datos1$bac1_2 = Datos1$bac1^2
Datos2$bac1_2 = Datos2$bac1^2


rd_1 <- rdrobust(y = Datos$recidivism, x = Datos$bac1, c = 0.08, h=0.5,all=T)
rd_2 <- rdrobust(y = Datos$recividism, x = Datos$bac1, c = 0.08, all=T)


rd_1_1 <- lm_robust(recidivism ~ D + bac1 + white+ aged+ acc, 
                         data = Datos1)

rd_1_2 <- lm_robust(recidivism ~ D + bac1 + white+ aged+ acc, 
                    data = Datos2)
rd_2_1 <- lm_robust(recidivism ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                    data = Datos1)

rd_2_2 <- lm_robust(recidivism ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                    data = Datos2)

rd_3_1 <- lm_robust(recidivism ~ D + bac1 + bac1_2+D*bac1+ D*bac1_2+ white+ aged+ acc, 
                    data = Datos1)

rd_3_2 <- lm_robust(recidivism ~ D + bac1 + bac1_2+D*bac1+ D*bac1_2+white+ aged+ acc, 
                    data = Datos2)

rd_1_1l <- lm(recidivism ~ D + bac1 + white+ aged+ acc, 
                    data = Datos1)

rd_1_2l <- lm(recidivism ~ D + bac1 + white+ aged+ acc, 
                    data = Datos2)
rd_2_1l <- lm(recidivism ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                    data = Datos1)

rd_2_2l <- lm(recidivism ~ D + bac1 + D*bac1+ white+ aged+ acc, 
                    data = Datos2)

rd_3_1l <- lm(recidivism ~ D + bac1 + bac1_2+D*bac1+ D*bac1_2+ white+ aged+ acc, 
                    data = Datos1)

rd_3_2l <- lm(recidivism ~ D + bac1 + bac1_2+D*bac1+ D*bac1_2+white+ aged+ acc, 
                    data = Datos2)



#Point 8:

recidivismmeans <- split(categories$recidivism, cut(categories$bac1, 100)) %>% 
  lapply(mean) %>% 
  unlist()

agg_recidivism_data <- data.frame(recidivism = recidivismmeans, bac1 = seq(0.0016,0.16, by = 0.0016))

rec_l= ggplot(categories, aes(bac1, recidivism)) +
  geom_point(aes(x = bac1, y = recidivism), data = agg_recidivism_data) +
  stat_smooth(aes(bac1, recidivism, group = D), formula = y ~ x ,method = "lm") +
  geom_vline(xintercept = 0.08)

rec_q =ggplot(categories, aes(bac1, recidivism)) +
  geom_point(aes(x = bac1, y = recidivism), data = agg_recidivism_data) +
  stat_smooth(aes(bac1, recidivism, group = D), formula = y ~ x + I(x^2),method = "lm") +
  geom_vline(xintercept = 0.08)

ggarrange(rec_l,rec_q,ncol=2,nrow=1)



