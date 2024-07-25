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
library(fastDummies)
library(lme4)
library(coefplot)
library(pBrackets)
library(estimatr)
library(ggtext)
library(readxl)
library(texreg)
local <- 'C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation'

dat1 <- read_excel(paste0(local,'/data/chapter_2.2_data.xlsx'))
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







dat1$black <- ifelse(dat1$GROUPS_RACE==2,1,0)
dat1$latino <- ifelse(dat1$GROUPS_RACE==6,1,0)

dat1$dif <- dat1$post - dat1$pre
study2 <- lm_robust(post~treatment*latino+pre+PartyID5, data=dat1)


f1 <- lm_robust(post~treatment*black+pre+PartyID5, data=dat1)
screenreg(f1, include.ci=F)
screenreg(study1.1, include.ci=F)
f3 <- lm_robust(post~treatment*black, data=dat1)
screenreg(f3, include.ci=F)

