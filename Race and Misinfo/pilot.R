
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

misin<-read.csv('pilot.csv')
misin <- misin[10:nrow(misin),]



misin$lat <- ifelse(misin$hisp =='Yes',1,0)
sum(misin$lat)/nrow(misin)#36%

misin$black <- ifelse(misin$race=='Black or African American', 1,0)
sum(misin$black)/nrow(misin)
misin$race[misin$l ==0 & misin$black==0]




misin$bl <- c()
for(i in 1:nrow(misin)){
  if(misin$black[i] == 1 | misin$race[i] ==  "White,Black or African American"  |misin$race[i]=="Black or African American,American Indian or Alaska Native"  | misin$race[i]== "Black or African American,Native Hawaiian, or other Pacific islander" ){
    misin$bl[i]<- 1
  }else(misin$bl[i] <- 0)
}
sum(misin$bl)/nrow(misin)#62


misin$both <- ifelse(misin$lat==1&misin$bl ==1,1,0) 
sum(misin$both)/nrow(misin)#3%

misin$neither <- ifelse(misin$l ==0 & misin$bl ==0,1,0)
sum(misin$neither)/nrow(misin)#4%
misin$race[misin$neither==1]

misin$male <- ifelse(misin$gender =='Male',1,0)
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
#dem = 58 & in = 19 & rep = 16 $ something else = 5
#black - 35      10         13                    1
#latino -21      9           1                    4



misin$idB <- misin$CBDV4
misin$idL <- misin$TLDV4


misin$treat_b[misin$FL_21_DO =="exp1_b_treatment"] <- 'NAACP'
misin$treat_b[misin$FL_21_DO =="exp1_b_control"] <- 'PolitiFact_b'
misin$treat_l[misin$FL_22_DO =="exp2_l_treatment"] <- 'UnidosUS'
misin$treat_l[misin$FL_22_DO =="exp2_l_control"] <- 'PolitiFact_l'

misin$commID_b <- c()

for(i in 1:nrow(misin)){
  if(misin$treat_b[i] == 'NAACP' & misin$idB[i] == 'NAACP'){
    misin$commID_b[i] <- 1
  }else if (misin$treat_b[i] == 'PolitiFact_b' & misin$idB[i] =='Politifact'){
    misin$commID_b[i] <- 1
  }else(misin$commID_b[i] <- 0)
}

misin$commID_l <- c()
for(i in 1:nrow(misin)){
  if(misin$treat_l[i] == 'UnidosUS' & misin$idL[i] == 'UnidosUS'){
    misin$commID_l[i] <- 1
  }else if (misin$treat_l[i] == 'PolitiFact_l' & misin$idL[i] =='Politifact'){
    misin$commID_l[i] <- 1
  }else(misin$commID_l[i] <- 0)
}

sum(misin$commID_b)/nrow(misin)
sum(misin$commID_l)/nrow(misin)
# black treatment 74% correct ID
# latino treatment 73% correct ID
 
# Responses among those who got it wrong in politifact latino targeting condition
table(misin$idL[misin$commID_l==0 & misin$treat_l=='PolitiFact_l'&misin$group =='Black'])


# Responses among those who got it wrong in UnidosUS  latino targeting condition
table(misin$idL[misin$commID_l==0 & misin$treat_l=='UnidosUS'&misin$group =='Black'])

# Responses among those who got it wrong in poitifact  black targeting condition
table(misin$idB[misin$commID_b==0 & misin$treat_b=='PolitiFact_b'&misin$group =='Black'])

# Responses among those who got it wrong in NAACP  black targeting condition
table(misin$idB[misin$commID_b==0 & misin$treat_b=='NAACP'&misin$group =='Black'])


table(misin$idL[misin$commID_l==0 & misin$treat_l=='PolitiFact_l'])


# Responses among those who got it wrong in UnidosUS  latino targeting condition
table(misin$idL[misin$commID_l==0 & misin$treat_l=='UnidosUS'])

# Responses among those who got it wrong in poitifact  black targeting condition
table(misin$idB[misin$commID_b==0 & misin$treat_b=='PolitiFact_b'])

# Responses among those who got it wrong in NAACP  black targeting condition
table(misin$idB[misin$commID_b==0 & misin$treat_b=='NAACP'])


bl_target <- data.frame(matrix(nrow=2, ncol=2))
rownames(bl_target)<- c('Black', 'Latino')
colnames(bl_target)<- c('PolitiFact','NAACP')

overall <- data.frame(matrix(nrow=2, ncol=1))
rownames(overall)<- c('Black targeting condition', 'Latino targeting condition')
colnames(overall)<- c('Overall means of 2 conditions')
overall[1,1] <- ci(as.numeric(as.factor(misin$CBDV1))[ misin$FL_21_DO=="exp1_b_control" ], na.rm=T)[1]
overall[2,1]<- ci(as.numeric(as.factor(misin$TLDV1))[ misin$FL_22_DO=="exp2_l_treatment" ], na.rm=T)[1]

bl_target[1,1] <- ci(as.numeric(as.factor(misin$CBDV1))[misin$group =='Black' & misin$FL_21_DO=="exp1_b_control" ], na.rm=T)[1]
bl_target[2,1] <- ci(as.numeric(as.factor(misin$CBDV1))[misin$group =='Latino' &misin$FL_21_DO=="exp1_b_control" ], na.rm=T)[1]
bl_target[1,2] <- ci(as.numeric(as.factor(misin$CBDV1))[misin$group =='Black' & misin$FL_21_DO=="exp1_b_treatment"], na.rm=T)[1]
bl_target[2,2] <- ci(as.numeric(as.factor(misin$CBDV1))[misin$group =='Latino' &misin$FL_21_DO=="exp1_b_treatment"], na.rm=T)[1]
b_treat
bl_target

lat_target <- data.frame(matrix(nrow=2, ncol=2))
rownames(lat_target)<- c('Black', 'Latino')
colnames(lat_target)<- c('PolitiFact','UnidosUS')
lat_target[1,1] <- ci(as.numeric(as.factor(misin$TLDV1))[misin$group =='Black' & misin$FL_22_DO=="exp2_l_control"], na.rm=T)[1]
lat_target[1,2] <- ci(as.numeric(as.factor(misin$TLDV1))[misin$group =='Black' & misin$FL_22_DO=="exp2_l_treatment"], na.rm=T)[1]
lat_target[2,1] <- ci(as.numeric(as.factor(misin$TLDV1))[misin$group =='Latino' &misin$FL_22_DO=="exp2_l_control"], na.rm=T)[1]
lat_target[2,2] <- ci(as.numeric(as.factor(misin$TLDV1))[misin$group =='Latino' &misin$FL_22_DO=="exp2_l_treatment"], na.rm=T)[1]

bl_target
lat_target


t.test(as.numeric(as.factor(misin$TLDV1)),as.numeric(as.factor(misin$FL_22_DO)))

table(misin$TLDV1)
lm(as.numeric(as.factor(misin$TLDV1))~as.factor(misin$FL_22_DOl))


jacob.dv<-as.numeric(as.factor(misin$TLDV1))
treatment<-as.factor(misin$FL_22_DO)
summary(lm(jacob.dv~treatment))

jacob.dv<-as.numeric(as.factor(misin$CBDV1))
treatment<-as.factor(misin$FL_21_DO)
summary(lm(jacob.dv~treatment*as.factor(misin$group)))

table(misin$FL_21_DO)

table(treatment)



