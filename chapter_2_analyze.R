library(cowplot)
library(descr)
library(readr)
library(magick)
library(dplyr)
library(magrittr)
library(grid)
library(kableExtra)
library(stringr)
library(gridExtra)
library(knitr)
library(ggplot2)
library(gtable)
library(labelled)
library(haven)
library(gmodels)
library(png)
library(tidyverse)
library(cjoint)
library(stargazer)
library(Hmisc)
library(sjPlot)
library(labelled)
library(haven)
library(png)
library(magicfor)    
library(magicfor)  
library(fastDummies)
library(lme4)
library(coefplot)
library(pBrackets)
library(estimatr)
library(ggtext)
library(texreg)
local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/Prospectus 2022/'

dat1 <- read.csv(paste0(local,'data/tass_misinfo/8557_WUSTL-Weidenbaum10Main.csv'), header = TRUE, stringsAsFactors = T)
# MISINF_PRE P_MISINF MISINF_POST
#GROUPS_RACE.
# Which of the following race or ethnic groups do you identify with?
#   
#   CAWI/CATI RESPONSE OPTIONS
# 1. 	White people
# 2. 	Black people

dat1$treatment <- NA


dat1$treatment[dat1$P_MISINF ==1 ] <- "Control"
dat1$treatment[dat1$P_MISINF ==2 ] <- "Black correction"
dat1$treatment[dat1$P_MISINF ==3 ] <- "White correction"
dat1$treatment <- factor(dat1$treatment, levels = c('Control' ,'Black correction','White correction'))
dat1$MISINF_PRE[dat1$MISINF_PRE >5 ] <- NA
dat1$MISINF_POST[dat1$MISINF_POST >5 ] <- NA

dat1$pre <- 5- dat1$MISINF_PRE
dat1$post <- 5- dat1$MISINF_POST


table(dat1$pre, dat1$treatment)/nrow(dat1)
table(dat1$post, dat1$treatment)/nrow(dat1)


table(dat1$pre[dat1$GROUPS_RACE==1]
, dat1$treatment[dat1$GROUPS_RACE==1]
)/nrow(subset(dat1, dat1$GROUPS_RACE==1))


table(dat1$post[dat1$GROUPS_RACE==1]
, dat1$treatment[dat1$GROUPS_RACE==1]
)/nrow(subset(dat1, dat1$GROUPS_RACE==1))



table(dat1$pre[dat1$GROUPS_RACE==2], dat1$treatment[dat1$GROUPS_RACE==2])/nrow(subset(dat1, dat1$GROUPS_RACE==2))



table(dat1$post[dat1$GROUPS_RACE==2], dat1$treatment[dat1$GROUPS_RACE==2])/nrow(subset(dat1, dat1$GROUPS_RACE==2))






dat1$black <- ifelse(dat1$GROUPS_RACE==2,1,0)
dat1$dif <- dat1$post - dat1$pre


f1 <- lm_robust(post~treatment, data=dat1)
screenreg(f1, include.ci=F)
f2 <- lm_robust(post~treatment*black+pre+PartyID5, data=dat1)
screenreg(f2, include.ci=F)
f3 <- lm_robust(dif~treatment*black, data=dat1)
screenreg(f3, include.ci=F)

texreg(list(f1, f2, f3),  
           custom.model.names =  c( 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))
















































