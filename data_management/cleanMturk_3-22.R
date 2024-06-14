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
local <- 'C:/Users/Path/to/Data/Prospectus 2022'
###Data
dat1 <- read.csv(paste0(local,'/data/dat-2022.csv'), header = TRUE, stringsAsFactors = T)
dat1 <- dat1[-1:-2,]
dat2 <- read.csv(paste0(local,'/data/dat-2022.csv'), header = TRUE, stringsAsFactors = T)
dat2 <- dat2[-1:-2,]
dat1$Birth_Year <- 2021 - as.numeric(dat1$Birth_Year)



##########################
#dependent variables
##########################

###higher values more racist
###FearInstRacismRare

##Flip 2 and 4


fire_range <- which(colnames(dat1)=='FIRE1.1'): which(colnames(dat1)=='FIRE4.1')

dat1$FIRE1.1_ <- dat1$FIRE1.1
dat1$FIRE2.1_ <- dat1$FIRE2.1
dat1$FIRE3.1_ <- dat1$FIRE3.1
dat1$FIRE4.1_ <- dat1$FIRE4.1


for(i in fire_range){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'  ){
         dat1[j,i] <- NA}
      }
  }  
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- as.numeric(factor(dat1[,i], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree')))
  
}

for(i in c(which(colnames(dat1)=='FIRE2.1'),which(colnames(dat1)=='FIRE4.1'))){
  dat1[,i] <- 6-dat1[,i]
}

dat1$Fear_1 <- dat1$FIRE1.1
dat1$Inst_1 <- dat1$FIRE2.1
dat1$Rare_1 <- dat1$FIRE3.1
dat1$Racism_1 <- dat1$FIRE4.1

##Flip 2 and 4


fire_range2 <- which(colnames(dat1)=='FIRE1.2'): which(colnames(dat1)=='FIRE4.2')


dat1$FIRE1.2_ <- dat1$FIRE1.2
dat1$FIRE2.2_ <- dat1$FIRE2.2
dat1$FIRE3.2_ <- dat1$FIRE3.2
dat1$FIRE4.2_ <- dat1$FIRE4.2

for(i in fire_range2){
  for(j in 1:nrow(dat1)){
    if( is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'){
        dat1[j,i] <- NA}
      }
  }
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- as.numeric(factor(dat1[,i], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree')))
  
}


for(i in c(which(colnames(dat1)=='FIRE2.2'),which(colnames(dat1)=='FIRE4.2'))){
  dat1[,i] <- 6-dat1[,i]
}

dat1$Fear_2 <- dat1$FIRE1.2
dat1$Inst_2 <- dat1$FIRE2.2
dat1$Rare_2 <- dat1$FIRE3.2
dat1$Racism_2 <- dat1$FIRE4.2


###Opinions
### Transforming so that HIGHER VALUES == MORE COUNTERNORMATIVE 
### --flip normative opinions

opinion_range <- which(colnames(dat1)=='NORM.1'): which(colnames(dat1)=='COUNTER.3')

for(i in opinion_range){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'){
      dat1[j,i] <- NA}
    }
  }  
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- factor(dat1[,i], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree'))
  dat1[,i] <- as.numeric(dat1[,i])
  
}


for(i in which(colnames(dat1)=='NORM.1'):which(colnames(dat1)=='NORM.3')){
  dat1[,i] <- 6-dat1[,i]
}
dat1$COUNTER.1 <- 6-dat1$COUNTER.1
dat1$COUNTER.2 <-  6-dat1$COUNTER.2



dat1$sumOpinion<- 0
for(i in 1:nrow(dat1)){
  for(j in opinion_range){
    if(is.na(dat1[i,j])==F){
      dat1$sumOpinion[i] <- dat1$sumOpinion[i] + dat1[i,j]
      dat1$opin <- dat1$sumOpinion/24

    }
  }
}

dat1$sumVALIDOpinion <- 0

for(i in 1:nrow(dat1)){
  for(j in opinion_range){
    dat1$sumVALIDOpinion[i] <- dat1$sumVALIDOpinion[i] + (is.na(dat1[i,j])==F)
  }
}

dat1$opinion<-NA
dat1$opinionV <- (dat1$sumOpinion/(dat1$sumVALIDOpinion/6))/24
for(i in 1:length(dat1$opinionV)){
  if(dat1$opinionV[i] !='NaN'){
    if(dat1$opinionV[i] < .5){
      dat1$opinion[i] <- 'Normative'
    }else if( dat1$opinionV[i] >= .5){
      dat1$opinion[i] <- 'Counternormative'
  }  
}
}
dat1$opinion <- factor(dat1$opinion, levels = c("Normative", "Counternormative"))



###Dehumanization

#######################Add race colname dehuman
colnames(dat1)[colnames(dat1) == 'DEHUMAN_1'] <- 'DEHUMAN_W'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_2'] <- 'DEHUMAN_B'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_3'] <- 'DEHUMAN_L'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_4'] <- 'DEHUMAN_A'


human_range <- c(which(colnames(dat1)=='DEHUMAN_W'),which(colnames(dat1)=='DEHUMAN_B') ,which(colnames(dat1)=='DEHUMAN_L') , which(colnames(dat1)=='DEHUMAN_A'))

for(i in human_range){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'){
      dat1[j,i] <- NA}
      }
  }
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- as.numeric(factor(dat1[,i],levels=1:100))
}

dat1$dehuman_wb <- (dat1$DEHUMAN_W - dat1$DEHUMAN_B)




###Racialresentment
###to make higher more racists
###recode rr2, rr4, rr6
###
colnames(dat1)[colnames(dat1) == 'RR6.1'] <- 'RR5.1' 
colnames(dat1)[colnames(dat1) == 'RR6.1.1'] <- 'RR6.1' 


rr_range <- which(colnames(dat1)=='RR1.1'): which(colnames(dat1)=='RR6.1')


for(i in rr_range){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'){
        dat1[j,i] <- NA}
    }
  }
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- as.numeric(factor(dat1[,i], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree')))
}


for(i in c(which(colnames(dat1)=='RR2.1'),which(colnames(dat1)=='RR3.1'),which(colnames(dat1)=='RR5.1'))){
  dat1[,i] <- 6-dat1[,i]
}

##
##

rr_range2 <- which(colnames(dat1)=='RR1.2'): which(colnames(dat1)=='RR6.2')

for(i in rr_range2){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,i])==F){
      if(dat1[j,i] == "" & dat1[j,i] == '-99'){
        dat1[j,i] <- NA}
    }
  }
  dat1[,i] <- factor(dat1[,i])
  dat1[,i] <- as.numeric(factor(dat1[,i], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree')))
}


for(i in c(which(colnames(dat1)=='RR2.2'),which(colnames(dat1)=='RR3.2'),which(colnames(dat1)=='RR5.2'))){
  dat1[,i] <- 6-dat1[,i]
}










rr_range <- which(colnames(dat1)=='RR1.1'): which(colnames(dat1)=='RR6.1')

dat1$sumRR<- 0
for(i in 1:nrow(dat1)){
  for(j in rr_range){
    if(is.na(dat1[i,j])==F){
      dat1$sumRR[i] <- dat1$sumRR[i] + dat1[i,j]
      dat1$rr1 <- dat1$sumRR/30
      
    }
  }
}


dat1$sumVALIDRR <- 0

for(i in 1:nrow(dat1)){
  for(j in rr_range){
    dat1$sumVALIDRR[i] <- dat1$sumVALIDRR[i] + (is.na(dat1[i,j])==F)
  }
}



dat1$RRV <- (dat1$sumRR/(dat1$sumVALIDRR/6))/30
median(dat1$RRV,na.rm=T)



rr_range2 <- which(colnames(dat1)=='RR1.2'): which(colnames(dat1)=='RR6.2')


dat1$sumRR2<- 0
for(i in 1:nrow(dat1)){
  for(j in rr_range2){
    if(is.na(dat1[i,j])==F){
      dat1$sumRR2[i] <- dat1$sumRR2[i] + dat1[i,j]
      dat1$rr2 <- dat1$sumRR2/24
      
    }
  }
}


dat1$sumVALIDRR2 <- 0

for(i in 1:nrow(dat1)){
  for(j in rr_range2){
    dat1$sumVALIDRR2[i] <- dat1$sumVALIDRR2[i] + (is.na(dat1[i,j])==F)
  }
}


dat1$RRV_2 <- (dat1$sumRR2/(dat1$sumVALIDRR2/6))/30
median(dat1$RRV_2,na.rm=T)

dat1$ideo <- dat1$Ideology






dat1$RR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.58){
      dat1$RR_pre[i] <- 'Low Racial Resentment'
    }else if( dat1$RRV[i] >=.58){
      dat1$RR_pre[i] <- 'High Racial Resentment'
    }  
  }
}

dat1$hiRR_pre  <- factor(dat1$RR_pre, levels = c("Low Racial Resentment", "High Racial Resentment"))
dat1$loRR_pre <- factor(dat1$RR_pre, levels = c("High Racial Resentment","Low Racial Resentment"))

dat1$age <- dat1$Birth_Year


##I think it is all right for blacks and whites to date each other
##higher values more racists



colnames(dat1)[colnames(dat1) == 'ER1.1'] <- 'date1'
colnames(dat1)[colnames(dat1) == 'ER1.2'] <- 'date2'

dat1$date1[dat1$date1 =="" | dat1$date1 =='-99'] <- NA
dat1$date2[dat1$date2 =="" | dat1$date2 =='-99'] <- NA
dat1$date1 <- as.numeric(factor(dat1$date1, levels =c('Strongly agree','Somewhat agree',"Neither agree nor disagree",'Somewhat disagree','Strongly disagree')))
dat1$date2 <- as.numeric(factor(dat1$date2, levels =c('Strongly agree','Somewhat agree',"Neither agree nor disagree",'Somewhat disagree','Strongly disagree')))


dat1$date_1 <-dat1$date1
dat1$date_2 <- dat1$date2

###Marriage
###Add race colname ExplRacism
#scale 0 100 how comfortable
#recoded 100 very uncomfortable
for(i in 1:4){
  for(j in 1:nrow(dat1)){
    if(is.na(dat1[j,which(colnames(dat1) == paste0('ER2_',i))])==F){
      if(dat1[j,which(colnames(dat1) == paste0('ER2_',i))]==""|dat1[j,which(colnames(dat1) == paste0('ER2_',i))]=='-99'  ){
      dat1[j,which(colnames(dat1) == paste0('ER2_',i))] <-NA
      }
    }
  }
}


dat1$marry_w <- as.numeric(factor(dat1$ER2_1, levels =as.character(0:100)))
dat1$marry_l <- as.numeric(factor(dat1$ER2_2, levels =as.character(0:100)))
dat1$marry_a <- as.numeric(factor(dat1$ER2_3, levels =as.character(0:100)))
dat1$marry_b <- as.numeric(factor(dat1$ER2_4, levels =as.character(0:100)))


dat1$marrybw <- dat1$marry_w - dat1$marry_b

##########################
#independent variables
##########################


###Treat
#Fix Column Name
colnames(dat1)[colnames(dat1) == 'TIME.T1_First.Click.1'] <- 'TIME.T2_First.Click'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Last.Click.1'] <- 'TIME.T2_Last.Click'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Page.Submit.1'] <- 'TIME.T2_Page.Submit'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Click.Count.1'] <- 'TIME.T2_Click.Count'

dat1$Treat[dat1$TIME.T1_Page.Submit==""&dat1$TIME.T2_Page.Submit==""&dat1$TIME.T3_Page.Submit==""&dat1$TIME.T4_Page.Submit==""] <- "Control"

dat1$Treat[dat1$TIME.T1_Page.Submit!=""] <- 'Tweet only'
dat1$Treat[dat1$TIME.T2_Page.Submit!=""] <- 'Counternormative comments'
dat1$Treat[dat1$TIME.T3_Page.Submit!=""] <- 'Normative comments'
dat1$Treat[dat1$TIME.T4_Page.Submit!=""] <- 'Mixed comments'

dat1$Treatment <- factor(dat1$Treat, levels=c('Control','Tweet only','Counternormative comments','Normative comments','Mixed comments' ))

###Race
dat1$latino <- ifelse(dat1$LATINO=='Yes',1,0)

dat1$Race <- NA
dat1$Race[dat1$RACE.LATINON =="White"]<-0
dat1$Race[dat1$RACE.LATINOY =="White"]<-0
dat1$Race[dat1$RACE.LATINOSKIP =="White"]<-0
dat1$Race[dat1$RACE.LATINON =="Black or African American" ]<-1
dat1$Race[dat1$RACE.LATINOY =="Black or African American" ]<-1
dat1$Race[dat1$RACE.LATINOSKIP =="Black or African American" ]<-1
dat1$Race[dat1$RACE.LATINON =="Asian"]<-2
dat1$Race[dat1$RACE.LATINOY =="Asian"]<-2
dat1$Race[dat1$RACE.LATINOSKIP =="Asian"]<-2
dat1$Race[dat1$RACE.LATINON =="American Indian or Alaska Native"]<-3
dat1$Race[dat1$RACE.LATINOY =="American Indian or Alaska Native"]<-3
dat1$Race[dat1$RACE.LATINOSKIP =="American Indian or Alaska Native"]<-3
dat1$Race[dat1$RACE.LATINON =="Native Hawaiian, or other Pacific islander" ]<-4
dat1$Race[dat1$RACE.LATINOY =="Native Hawaiian, or other Pacific islander" ]<-4
dat1$Race[dat1$RACE.LATINOSKIP =="Native Hawaiian, or other Pacific islander"]<- 4
dat1$Race <- as.factor(dat1$Race)
###Party id

dat1$PID.ALT[dat1$PID.ALT != ""]
dat1$PID.ALT
dat1$PID.ALT[dat1$PID.ALT=='none' | dat1$PID.ALT =='None' |dat1$PID.ALT =='N/A'|dat1$PID.ALT =='na' |dat1$PID.ALT =='Apolitical' |dat1$PID.ALT =='Nonpartisan' |dat1$PID.ALT =='NONE'| dat1$PID.ALT =="I don't know"| dat1$PID.ALT == "don't really have a side of either I'm very neutral "| dat1$PID.ALT == 'I dont'| dat1$PID.ALT == 'nothing'| dat1$PID.ALT == 'Unaffiliated with any party'| dat1$PID.ALT == 'independent of political party'| dat1$PID.ALT == 'Unaffiliated'| dat1$PID.ALT == 'no us party'| dat1$PID.ALT == 'Undecided'| dat1$PID.ALT == "Don't know"| dat1$PID.ALT == 'none of the pigs really'| dat1$PID.ALT == 'Non-Party'| dat1$PID.ALT == 'None of them.'| dat1$PID.ALT == 'Im not sure. ' | dat1$PID.ALT == "I don't."] <- 'None'

dat1$PID.ALT[dat1$PID.ALT == 'none - anarchist'| dat1$PID.ALT == 'No party, anarchy' |dat1$PID.ALT == 'anarchist/socialist'] <- 'Anarchist'

dat1$PID.ALT[dat1$PID.ALT == 'Libertarian'| dat1$PID.ALT == 'Libertarian Party' | dat1$PID.ALT == 'libertarian'| dat1$PID.ALT == 'Libertarian Party ' | dat1$PID.ALT == 'Libertarian '] <- 'Libertarian'

dat1$PID.ALT[dat1$PID.ALT == 'Liberal'| dat1$PID.ALT == 'Liberal '| dat1$PID.ALT == 'liberal' ] <- 'Liberal'

dat1$PID.ALT[dat1$PID.ALT == 'Independent '| dat1$PID.ALT == 'INDEPENDANT' | dat1$PID.ALT == 'Independent'| dat1$PID.ALT == 'independent '] <- 'Independent'

dat1$PID.ALT[dat1$PID.ALT == 'Democratic'] <- 'Democrat'

dat1$PID.ALT[dat1$PID.ALT == 'republican'] <- 'Republican'

sort(dat1$PID.ALT[dat1$PID.ALT != ""])
dat1$Party <- dat1$PID



#for(i in 1:length(dat1$Party)){
 # if(dat1$Party[i]=='Something else' & is.na(dat1$Party[i])==F){
 #   dat1$Party[i] <- dat1$PID.ALT[i]
#  }
#}
#############################
#######################1==REPUBLICAN; 2==DEMOCRAT; 3==SOMETHINGELSE
dat1$PID_ <- as.numeric(dat1$PID)

dat1$PID <- NA
dat1$PID_STRENGTH.D <- as.numeric(dat1$PID_STRENGTH.D)
dat1$PID_STRENGTH.R <- as.numeric(dat1$PID_STRENGTH.R)

for(i in 1:length(dat1$PID)){
  if(dat1$PID_STRENGTH.D[i]==1 & is.na(dat1$PID_STRENGTH.D[i])==F){
    dat1$PID[i] <- 1
  }else if(dat1$PID_STRENGTH.D[i]==0& is.na(dat1$PID_STRENGTH.D[i])==F){
    dat1$PID[i] <- 2
  }else if(dat1$PID_STRENGTH.R[i]==0& is.na(dat1$PID_STRENGTH.R[i])==F){
    dat1$PID[i] <- 3
  }else if(dat1$PID_STRENGTH.R[i]==1& is.na(dat1$PID_STRENGTH.R[i])==F){
    dat1$PID[i] <- 4
  }
}

for(i in 1:length(dat1$PID)){
  if(is.na(dat1$PID[i])==T & dat1$PID_LEAN.OTHER[i]=='LEAN_REP'){
    dat1$PID[i] <- 3
  }else if(is.na(dat1$PID[i])==T & dat1$PID_LEAN.OTHER[i]=='LEAN_DEM'){
    dat1$PID[i] <- 2
  }
  
}

dat1$pid <- ifelse(dat1$Party != 'Republican' & dat1$Party != 'Democrat', 'Other',NA)
dat1$pid[dat1$Party == 'Republican'] <- 'Republican'
dat1$pid[dat1$Party == 'Democrat'] <- 'Democrat'




###Age

for (i in 1:nrow(dat1)){
  if (dat1$Birth_Year[i] < 30 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '18-- 29 years old'
  } else if (dat1$Birth_Year[i] > 30 & dat1$Birth_Year[i] < 45 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '30-- 44 years old'
  } else if (dat1$Birth_Year[i] > 44 & dat1$Birth_Year[i] < 60 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '46-- 49 years old'
  } else if (dat1$Birth_Year[i] > 59 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '60+ years old'
  }}
dat1$ageCat <- as.factor(dat1$ageCat)

###Income

dat1$income <- as.numeric(dat1$INCOME)

###Ideology

dat1$Ideology[dat1$IDEOLOGY=="Very liberal"|dat1$IDEOLOGY=="Somewhat liberal" |dat1$IDEOLOGY=="Slightly liberal"] <- 'Liberal'
dat1$Ideology[dat1$IDEOLOGY=="Very conservative"|dat1$IDEOLOGY=="Somewhat conservative"|dat1$IDEOLOGY=="Slightly conservative"] <- 'Conservative'
dat1$Ideology[dat1$IDEOLOGY=="Neither liberal nor conservative; Moderate"] <- 'Other'
dat1$ideo <- factor(dat1$Ideology , levels=c('Liberal','Other','Conservative'))
###Attn to politics

for(i in 1:ncol(dat1)){
  if(is.na(dat1[i,'POL_ATT'])==F){
    if(dat1[i,'POL_ATT'] =='-99' | dat1[i,'POL_ATT'] ==''){ 
        dat1[i,'POL_ATT'] <- NA
    }
  }
}  

dat1$polattn <- as.numeric(factor(dat1$POL_ATT, levels=c("Never","Sometimes","Most of the time","Always")))

###Gender
#male- 0, female-1, other, 2
dat1$male[dat1$GENDER=='Male'] <-1
dat1$male[dat1$GENDER=='Female'] <-0
dat1$male[dat1$GENDER=='Other'] <-NA


###Education
dat1$bach <- ifelse(dat1$EDU =='BA' | dat1$EDU=='POSGRAD' | dat1$EDU=='SOMCOLL' ,1,0)

dat1$nonwhite <- ifelse(dat1$Race==0,0,1)






tri_flip <- c(1,3,5:9,12:15,17:20,22:24,26:29,31:32,34,36:38,40,42:43,45:46,48:49,51,53:56,58)

###Psychopathy
###
###for(i in c(26:46,48:84)){


for(i in 1:58){
  dat1[dat1[,which(colnames(dat1)==paste0('tri',i))] =='-99',which(colnames(dat1)==paste0('tri',i))] <- NA
  dat1[, which(colnames(dat1)==paste0('tri',i))] <-factor(dat1[,which(colnames(dat1)==paste0('tri',i))], levels =c('False','Somewhat false','Somewhat true','True'))
  dat1[, which(colnames(dat1)==paste0('tri',i))] = as.numeric(as.factor(dat1[, which(colnames(dat1)==paste0('tri',i))]))
}

for(i in tri_flip){
  dat1[, which(colnames(dat1)==paste0('tri',i))] <-6-dat1[, which(colnames(dat1)==paste0('tri',i))]
}

dat1$sumtri<- 0
for(i in 1:nrow(dat1)){  
  for(j in 1:58){
    if(is.na(dat1[i, which(colnames(dat1)==paste0('tri',j))]) ==F){
      dat1$sumtri[i] <- dat1$sumtri[i] + dat1[i, which(colnames(dat1)==paste0('tri',j))]
    }
  }  
}


dat1$sumVALIDtri <- 0

for(i in 1:nrow(dat1)){
  for(j in 1:58){
    dat1$sumVALIDtri[i] <- dat1$sumVALIDtri[i] + (is.na(dat1[i,which(colnames(dat1)==paste0('tri',j))])==F)
  }
}


dat1$Psycho <- (dat1$sumtri/(dat1$sumVALIDtri/58))/232
for(i in 1:length(dat1$Psycho)){
  if(dat1$Psycho[i] !='NaN'){
    if(dat1$Psycho[i] <= .33){
      dat1$Psycho[i] <- 'Low'
    }else if( dat1$Psycho[i] > .33 & dat1$Psycho[i]  <=.67){
      dat1$Psycho[i] <- 'Medium'
    }else if(dat1$Psycho[i] > .67){
      dat1$Psycho[i] <- 'High'}
  }  
}

dat1$Psycho <- factor(dat1$Psycho, levels = c("Low", "Medium", "High"))

###Troll

#Higher==More Troll


troll_cols <- which(colnames(dat1)=='TROLL_1'):which(colnames(dat1)=='TROLL_7')

for(i in troll_cols ){
  new_col <- ncol(dat1) +1
  dat1[,new_col] <- dat1[,i]
  colnames(dat1)[new_col] <-  paste0('troll',which(troll_cols == i))
}

troll_cols2 <- which(colnames(dat1)=='troll1'):which(colnames(dat1)=='troll7')

for(j in troll_cols2){
  for(i in 1:nrow(dat1)){
    if(is.na(dat1[i,j]) ==F){
      if(dat1[i,j] =='-99' | dat1[i,j] ==''){ 
        dat1[i,j] <- NA
      }
    }
  }
  dat1[,j] <- factor(dat1[,j], levels =c('Strongly disagree','Somewhat disagree',"Neither agree nor disagree",'Somewhat agree','Strongly agree'))
  dat1[,j] <- as.numeric(dat1[,j])
}


dat1$sumtroll<- 0
for(i in 1:nrow(dat1)){
  for(j in which(colnames(dat1)=='troll1'):which(colnames(dat1)=='troll7')){
    if(is.na(dat1[i,j])==F){
      dat1$sumtroll[i] <- dat1$sumtroll[i] + dat1[i,j]
      
    }
  }
}

dat1$sumVALIDtroll <- 0
for(i in 1:nrow(dat1)){
  for(j in which(colnames(dat1)=='troll1'):which(colnames(dat1)=='troll7')){
    dat1$sumVALIDtroll[i] <- dat1$sumVALIDtroll[i] + (is.na(dat1[i,j])==F)
  }
}

dat1$ideo <- factor(dat1$ideo, levels=c('Liberal','Other','Conservative'))
dat1$pid <- factor(dat1$pid, levels=c('Democrat','Other','Republican'))
dat1$ageCat <- factor(dat1$ageCat, levels=c("18-- 29 years old","30-- 44 years old", "46-- 49 years old", "60+ years old" ))


dat1$Troll <- (dat1$sumtroll/(dat1$sumVALIDtroll/7))/28
dat1$countTroll <- dat1$sumtroll


dat1$Fear <- dat1$Fear_2-dat1$Fear_1
dat1$Inst <- dat1$Inst_2-dat1$Inst_1
dat1$Rare<- dat1$Rare_2-dat1$Rare_1
dat1$Racism <- dat1$Racism_2-dat1$Racism_1
dat1$date   <- dat1$date_2-dat1$date_1
dat1$RRV11<- dat1$RRV_2-dat1$RRV
write.csv(dat1,'data/clean_dat4-22.csv')

colnames(dat1)[which(colnames(dat1) =='dehuman_wb')] <- 'dehumanbw'

colnames(dat1)[which(colnames(dat1) =='marry_bw')] <- 'marrybw'

dat1$X <- rownames(dat1)


colnames(dat1)[which(colnames(dat1)=='RRV')] <-'RRV_1'


dat2<- as.data.frame(dat1 %>% 
                       pivot_longer(cols = c("Fear_1" ,"Inst_1"  ,"Rare_1" ,"Racism_1" ,
                                             "Fear_2","Inst_2" , "Rare_2"  , "Racism_2", 
                                             "date_1", "date_2",
                                             "RRV_1","RRV_2"), 
                                    names_to = c(".value", "Var"), names_sep = "_") )


