
library(descr)
library(readr)
#library(RnBeads)
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
library(ggtext)
setwd("C:/Users/Path/to/Data/Grants/Race and Misinfo")
misin<-read.csv('pilot2.csv')
misin <- misin[8:nrow(misin),]

misin$lat <- ifelse(misin$hisp =='Yes',1,0)
sum(misin$lat)/nrow(misin)#50%

misin$black <- ifelse(misin$race=='Black or African American', 1,0)
sum(misin$black)/nrow(misin)#46%
misin$race[misin$lat ==0 & misin$black==0]

misin$bl <- c()
for(i in 1:nrow(misin)){
  if(misin$black[i] == 1 | misin$race[i] ==  "White,Black or African American"  |misin$race[i]=="Black or African American,American Indian or Alaska Native"  | misin$race[i]== "Black or African American,Native Hawaiian, or other Pacific islander"| misin$race[i]== "White,Black or African American,Native Hawaiian, or other Pacific islander"){
    misin$bl[i]<- 1
  }else(misin$bl[i] <- 0)
}

sum(misin$bl)/nrow(misin)#62

misin$both <- ifelse(misin$lat==1&misin$bl ==1,1,0) 
sum(misin$both)/nrow(misin)#3%

misin$neither <- ifelse(misin$lat ==0 & misin$bl ==0,1,0)
sum(misin$neither)/nrow(misin)#4%
misin$race[misin$neither==1]


table(misin$gender)/nrow(misin)

misin$group <-c()

for(i in 1:nrow(misin)){
  if(misin$bl[i] ==1 ){
    misin$group[i] <- 'Black'
  }else if(misin$lat[i] ==1){
    misin$group[i] <- 'Latino'
  }else if(misin$bl[i] !=1 & misin$lat[i] !=1){
    misin$group[i] <- 'Other'
  }
}

misin$PID <- as.numeric(as.factor(misin$pid))
table(misin$pid)/nrow(misin)

table(misin$pid,misin$bl)/nrow(misin)
table(misin$pid,misin$lat)/nrow(misin)


misin$treat_b <- misin$FL_21_DO
misin$treat_l <- misin$FL_22_DO

misin$commID_b <- c()

for(i in 1:nrow(misin)){
  if(misin$treat_b[i] == 'bl_treat' & misin$bl_comm[i] == 'NAACP'){
    misin$commID_b[i] <- 1
  }else if (misin$treat_b[i] == 'bl_control' & misin$bl_comm[i] =='ACLU'){
    misin$commID_b[i] <- 1
  }else(misin$commID_b[i] <- 0)
}

misin$commID_l <- c()
for(i in 1:nrow(misin)){
  if(misin$treat_l[i] == 'lat_treat' & misin$lat_comm[i] == 'UnidosUS'){
    misin$commID_l[i] <- 1
  }else if (misin$treat_l[i] == 'lat_control' & misin$lat_comm[i] =='ACLU'){
    misin$commID_l[i] <- 1
  }else(misin$commID_l[i] <- 0)
}

sum(misin$commID_b)/nrow(misin)
sum(misin$commID_l)/nrow(misin)


# Responses among those who got it wrong in politifact latino targeting condition
table(misin$lat_comm[misin$commID_l==0 & misin$treat_l=='lat_control'])

# Responses among those who got it wrong in UnidosUS  latino targeting condition
table(misin$lat_comm[misin$commID_l==0 & misin$treat_l=='lat_treat'])

# Responses among those who got it wrong in poitifact  black targeting condition
table(misin$bl_comm[misin$commID_b==0 & misin$treat_b=='bl_control'])

# Responses among those who got it wrong in NAACP  black targeting condition
table(misin$bl_comm[misin$commID_b==0 & misin$treat_b=='bl_treat'])


bl_target <- data.frame(matrix(nrow=2, ncol=2))
rownames(bl_target)<- c('Black', 'Latino')
colnames(bl_target)<- c('ACLU','NAACP')

overall <- data.frame(matrix(nrow=2, ncol=1))
rownames(overall)<- c('Black targeting condition', 'Latino targeting condition')
colnames(overall)<- c('Overall means of 2 conditions')
overall[1,1] <- ci(as.numeric(as.factor(misin$bl_acc))[ misin$treat_b=="bl_control" ], na.rm=T)[1]
overall[2,1]<- ci(as.numeric(as.factor(misin$bl_acc))[ misin$treat_b=="bl_treat" ], na.rm=T)[1]


bl_target[1,1] <- ci(as.numeric(as.factor(misin$bl_acc))[misin$group =='Black' & misin$FL_21_DO=="bl_control" ], na.rm=T)[1]
bl_target[1,2] <- ci(as.numeric(as.factor(misin$bl_acc))[misin$group =='Black' & misin$FL_21_DO=="bl_treat"], na.rm=T)[1]
bl_target[2,1] <- ci(as.numeric(as.factor(misin$bl_acc))[misin$group =='Latino' &misin$FL_21_DO=="bl_control" ], na.rm=T)[1]
bl_target[2,2] <- ci(as.numeric(as.factor(misin$bl_acc))[misin$group =='Latino' &misin$FL_21_DO=="bl_treat"], na.rm=T)[1]


lat_target <- data.frame(matrix(nrow=2, ncol=2))
rownames(lat_target)<- c('Black', 'Latino')
colnames(lat_target)<- c('ACLU','UnidosUS')
lat_target[1,1] <- ci(as.numeric(as.factor(misin$lat_acc))[misin$group =='Black' & misin$FL_22_DO=="lat_control"], na.rm=T)[1]
lat_target[1,2] <- ci(as.numeric(as.factor(misin$lat_acc))[misin$group =='Black' & misin$FL_22_DO=="lat_treat"], na.rm=T)[1]
lat_target[2,1] <- ci(as.numeric(as.factor(misin$lat_acc))[misin$group =='Latino' &misin$FL_22_DO=="lat_control"], na.rm=T)[1]
lat_target[2,2] <- ci(as.numeric(as.factor(misin$lat_acc))[misin$group =='Latino' &misin$FL_22_DO=="lat_treat"], na.rm=T)[1]

bl_target
lat_target

summary(lm(as.numeric(as.factor(misin$bl_acc))~as.factor(misin$FL_21_DO)))
summary(lm(as.numeric(as.factor(misin$lat_acc))~as.factor(misin$FL_22_DO)))

misin$famil_1#aclu
misin$famil_2#naacp
misin$famil_3#unidos

table(misin$famil_1, misin$group)/#aclu
table(misin$famil_2,misin$group)#naacp
table(misin$famil_3,misin$hisp)#unidos

