library(readr)
library(dplyr)
library(magrittr)
library(knitr)
library(ggplot2)
library(labelled)
library(haven)
library(tidyverse)
library(OneR)
setwd("C:/Users/Path/to/Data/Prospectus work/Theory Driven Draft/Results/")

dat1 <- read.csv(file = 'dat_text.csv', header = TRUE, stringsAsFactors = FALSE)
dat2 <- read.csv(file = 'dat_values.csv', header = TRUE, stringsAsFactors = FALSE)


#################Weed out some participants
sum(dat1$Progress=='100')
sum(dat1$Consent=='0')


dat1 <- subset(dat1,dat1$Consent=='1')
dat2 <- subset(dat2,dat2$Consent=='Yes')

dat1 <- subset(dat1,dat1$Birth_Year!="")
dat2 <- subset(dat2,dat2$Birth_Year!="")

dat1 <- subset(dat1, dat1$Progress=='100')
dat2 <- subset(dat2, dat2$Progress=='100')

dat1$Birth_Year <- as.numeric(dat1$Birth_Year)
dat2$Birth_Year <- as.numeric(dat2$Birth_Year)
dat2$Birth_Year <- 2021-dat2$Birth_Year
dat1$Birth_Year[dat1$Birth_Year==0] <-18

######################Fix Column Name
colnames(dat1)[colnames(dat1) == 'TIME.T1_First.Click.1'] <- 'TIME.T2_First.Click'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Last.Click.1'] <- 'TIME.T2_Last.Click'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Page.Submit.1'] <- 'TIME.T2_Page.Submit'
colnames(dat1)[colnames(dat1) == 'TIME.T1_Click.Count.1'] <- 'TIME.T2_Click.Count'



#######################Add race colname dehuman
colnames(dat1)[colnames(dat1) == 'DEHUMAN_1'] <- 'DEHUMAN_W'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_2'] <- 'DEHUMAN_B'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_3'] <- 'DEHUMAN_L'
colnames(dat1)[colnames(dat1) == 'DEHUMAN_4'] <- 'DEHUMAN_A'

colnames(dat2)[colnames(dat2) == 'DEHUMAN_1'] <- 'DEHUMAN_W'
colnames(dat2)[colnames(dat2) == 'DEHUMAN_2'] <- 'DEHUMAN_B'
colnames(dat2)[colnames(dat2) == 'DEHUMAN_3'] <- 'DEHUMAN_L'
colnames(dat2)[colnames(dat2) == 'DEHUMAN_4'] <- 'DEHUMAN_A'

#######################Add race colname ExplRacism
colnames(dat1)[colnames(dat1) == 'ER2_1'] <- 'ER_W'
colnames(dat1)[colnames(dat1) == 'ER2_2'] <- 'ER_L'
colnames(dat1)[colnames(dat1) == 'ER2_3'] <- 'ER_A'
colnames(dat1)[colnames(dat1) == 'ER2_4'] <- 'ER_B'

colnames(dat2)[colnames(dat2) == 'ER2_1'] <- 'ER_W'
colnames(dat2)[colnames(dat2) == 'ER2_2'] <- 'ER_L'
colnames(dat2)[colnames(dat2) == 'ER2_3'] <- 'ER_A'
colnames(dat2)[colnames(dat2) == 'ER2_4'] <- 'ER_B'


#####################ageCat
for (i in 1:nrow(dat1)){
  if (dat1$Birth_Year[i] < 30 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '18-- 29 years old'
  } else if (dat1$Birth_Year[i] > 30 & dat1$Birth_Year[i] < 45 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '30-- 44 years old'
  } else if (dat1$Birth_Year[i] > 44 & dat1$Birth_Year[i] < 60 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '45-- 49 years old'
  } else if (dat1$Birth_Year[i] > 59 & is.na(dat1$Birth_Year[i]) == F) {
    dat1$ageCat[i] <- '60+ years old'
  }}



##Latino
#2==NO
dat1$LATINO <- as.numeric(dat1$LATINO)

#################Race
#White ==0'; Black ==1; American Indian ==2;  Asain ==3; Native Hawaiin ==4


dat1$latino[dat1$LATINO==1] <- 5
dat1$Race[dat2$LATINO=='Yes'] <- 5



dat1$latino[dat1$LATINO=='Yes'] <- 1

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
dat1$Race[dat1$RACE.LATINOSKIP =="Native Hawaiian, or other Pacific islander" ]<4
dat1$Race <- as.factor(dat1$Race)

dat1$Race[c(which(dat1$RACE.LATINON!=0&dat1$RACE.LATINON!=1&dat1$RACE.LATINON!=2&dat1$RACE.LATINON!=3&dat1$RACE.LATINON!=4&dat1$RACE.LATINON!=""))] <-6

dat1$Race[c(which(dat1$RACE.LATINOY!=0&dat1$RACE.LATINOY!=1&dat1$RACE.LATINOY!=2&dat1$RACE.LATINOY!=3&dat1$RACE.LATINOY!=4&dat1$RACE.LATINOY!=""))] <-6

dat1$Race[dat1$RACE.LATINOSKIP ==0]<-0

dat1$nonwhite <- ifelse(dat1$Race ==)
dat1$latino <- ifelse(dat1$RACE.LATINOY != "", 1,0)



#############################Party Traits
dat2$PID.ALT[dat2$PID.ALT != ""]
dat2$PID.ALT
dat2$PID.ALT[dat2$PID.ALT=='none' | dat2$PID.ALT =='None' |dat2$PID.ALT =='N/A'|dat2$PID.ALT =='na' |dat2$PID.ALT =='Apolitical' |dat2$PID.ALT =='Nonpartisan' |dat2$PID.ALT =='NONE'| dat2$PID.ALT =="I don't know"| dat2$PID.ALT == "don't really have a side of either I'm very neutral "| dat2$PID.ALT == 'I dont'| dat2$PID.ALT == 'nothing'| dat2$PID.ALT == 'Unaffiliated with any party'| dat2$PID.ALT == 'independent of political party'| dat2$PID.ALT == 'Unaffiliated'| dat2$PID.ALT == 'no us party'| dat2$PID.ALT == 'Undecided'| dat2$PID.ALT == "Don't know"| dat2$PID.ALT == 'none of the pigs really'| dat2$PID.ALT == 'Non-Party'| dat2$PID.ALT == 'None of them.'| dat2$PID.ALT == 'Im not sure. ' | dat2$PID.ALT == "I don't."] <- 'None'

dat2$PID.ALT[dat2$PID.ALT == 'none - anarchist'| dat2$PID.ALT == 'No party, anarchy' |dat2$PID.ALT == 'anarchist/socialist'] <- 'Anarchist'

dat2$PID.ALT[dat2$PID.ALT == 'Libertarian'| dat2$PID.ALT == 'Libertarian Party' | dat2$PID.ALT == 'libertarian'| dat2$PID.ALT == 'Libertarian Party ' | dat2$PID.ALT == 'Libertarian '] <- 'Libertarian'

dat2$PID.ALT[dat2$PID.ALT == 'Liberal'| dat2$PID.ALT == 'Liberal '| dat2$PID.ALT == 'liberal' ] <- 'Liberal'

dat2$PID.ALT[dat2$PID.ALT == 'Independent '| dat2$PID.ALT == 'INDEPENDANT' | dat2$PID.ALT == 'Independent'| dat2$PID.ALT == 'independent '] <- 'Independent'
 
dat2$PID.ALT[dat2$PID.ALT == 'Democratic'] <- 'Democrat'

dat2$PID.ALT[dat2$PID.ALT == 'republican'] <- 'Republican'

sort(dat2$PID.ALT[dat2$PID.ALT != ""])
df$Party <- dat2$PID



for(i in 1:length(df$Party)){
  if(df$Party[i]=='Something else'){
    df$Party[i] <- dat2$PID.ALT[i]
  }
}
#############################
#######################1==REPUBLICAN; 2==DEMOCRAT; 3==SOMETHINGELSE
df$PID_ <- as.numeric(dat1$PID)

df$PID <- NA
dat1$PID_STRENGTH.D <- as.numeric(dat1$PID_STRENGTH.D)
dat1$PID_STRENGTH.R <- as.numeric(dat1$PID_STRENGTH.R)

for(i in 1:length(df$PID)){
  if(dat1$PID_STRENGTH.D[i]==1 & is.na(dat1$PID_STRENGTH.D[i])==F){
    df$PID[i] <- 1
  }else if(dat1$PID_STRENGTH.D[i]==0& is.na(dat1$PID_STRENGTH.D[i])==F){
    df$PID[i] <- 2
  }else if(dat1$PID_STRENGTH.R[i]==0& is.na(dat1$PID_STRENGTH.R[i])==F){
    df$PID[i] <- 3
  }else if(dat1$PID_STRENGTH.R[i]==1& is.na(dat1$PID_STRENGTH.R[i])==F){
    df$PID[i] <- 4
  }
}

for(i in 1:length(df$PID)){
  if(is.na(df$PID[i])==T & dat2$PID_LEAN.OTHER[i]=='LEAN_REP'){
    df$PID[i] <- 3
  }else if(is.na(df$PID[i])==T & dat2$PID_LEAN.OTHER[i]=='LEAN_DEM'){
    df$PID[i] <- 2
  }
  
}



#######Triarchic scale
#######
for(i in c(26:46,48:84)){
  dat1[,i] <- as.numeric(dat1[,i])
}


for(i in c(1,3,5:9,12:15,17:20,22:24,26:29,31:32,34,36:38,40,42:43,45:46,48:49,51,53:56,58)){
  dat1$tri[i] <- 3-dat1$tri[i]
}


df$sumtri<- 0
for(i in 1:nrow(dat1)){
  for(j in c(26:46,48:84)){
    if(is.na(dat1[i,j])==F){
      df$sumtri[i] <- df$sumtri[i] + dat1[i,j]
      
    }
  }
}

df$sumVALIDtri <- 0
for(i in 1:nrow(dat1)){
  for(j in c(26:46,48:84)){
    df$sumVALIDtri[i] <- df$sumVALIDtri[i] + (is.na(dat1[i,j])==F)
  }
}


df$Psycho <- (df$sumtri/(df$sumVALIDtri/58))/174


#################Troll Higher==More Troll
#################

####### TROLL_1:7 - 1-STRONGLY DISAGREE - 4-STRONGLY AGREE 0-NEITHER (DIS)AGREE

#which(colnames(dat1)=='TROLL_1')
#which(colnames(dat1)=='TROLL_7')

for(i in 103:109){
  dat1[,i] <- as.numeric(dat1[,i])
  }

df$sumtroll<- 0
for(i in 1:nrow(dat1)){
  for(j in c(103:109)){
    if(is.na(dat1[i,j])==F){
      df$sumtroll[i] <- df$sumtroll[i] + dat1[i,j]
      
    }
  }
}

df$sumVALIDtroll <- 0
for(i in 1:nrow(dat1)){
  for(j in c(103:109)){
    df$sumVALIDtroll[i] <- df$sumVALIDtroll[i] + (is.na(dat1[i,j])==F)
  }
}


df$Troll <- (df$sumtroll/(df$sumVALIDtroll/7))/28
df$countTroll <- df$sumtroll


##########################Racial Resentment
##########################
colnames(dat1)[119]<-'RR5.1'
colnames(dat1)[120]<-'RR6.1'




#which(colnames(dat1)=='RR1.1')
#which(colnames(dat1)=='RR6.1')

for(i in 115:120){
  dat1[,i] <- as.numeric(dat1[,i])
}


#Need to flip some of the RR 
#RR2; RR3; RR5

##which(colnames(dat1)=='RR2.1')
#which(colnames(dat1)=='RR3.1')
#which(colnames(dat1)=='RR5.1')

for(i in c(116,117,119)){
  dat1[,i] <- 5-dat1[,i]
  dat1[,i][dat1[,i]==5] <-0
}

df$sumRR1<- 0
for(i in 1:nrow(dat1)){
  for(j in c(115:120)){
    if(is.na(dat1[i,j])==F){
      df$sumRR1[i] <- df$sumRR1[i] + dat1[i,j]
      
    }
  }
}

df$sumVALIDRR1 <- 0
for(i in 1:nrow(dat1)){
  for(j in c(115:120)){
    df$sumVALIDRR1[i] <- df$sumVALIDRR1[i] + (is.na(dat1[i,j])==F)
  }
}


df$RR1 <- (df$sumRR1/(df$sumVALIDRR1/6))/24

(10/(.8333))/24
10/24
#which(colnames(dat1)=='RR1.2')
#which(colnames(dat1)=='RR6.2')

for(i in 148:153){
  dat1[,i] <- as.numeric(dat1[,i])
}


#Need to flip some of the RR 
#RR2; RR3; RR5

# which(colnames(dat1)=='RR2.2')
# which(colnames(dat1)=='RR3.2')
# which(colnames(dat1)=='RR5.2')

for(i in c(149,150,152)){
  dat1[,i] <- 5-dat1[,i]
  dat1[,i][dat1[,i]==5] <-0
}

df$sumRR2<- 0
for(i in 1:nrow(dat1)){
  for(j in c(148:153)){
    if(is.na(dat1[i,j])==F){
      df$sumRR2[i] <- df$sumRR2[i] + dat1[i,j]
      
    }
  }
}

df$sumVALIDRR2 <- 0
for(i in 1:nrow(dat1)){
  for(j in c(148:153)){
    df$sumVALIDRR2[i] <- df$sumVALIDRR2[i] + (is.na(dat1[i,j])==F)
  }
}


df$RR2 <- (df$sumRR2/(df$sumVALIDRR2/6))/24



###########################Dehumanization
###########################

# which(colnames(dat1)=='DEHUMAN_B')
# which(colnames(dat1)=='DEHUMAN_W')
# #which(colnames(dat1)=='DEHUMAN_A')
# which(colnames(dat1)=='DEHUMAN_L')
# 

for(i in 158:161){
  dat1[,i]<- as.numeric(dat1[,i])
  }



####



df$Dehuman <- dat1$DEHUMAN_W - dat1$DEHUMAN_B


##################Explicit Racism
# which(colnames(dat1)=='ER_W')
# which(colnames(dat1)=='ER_B')
# which(colnames(dat1)=='ER_L')
# which(colnames(dat1)=='ER_A')


for(i in 154:157){
  dat1[,i]<- as.numeric(dat1[,i])
}


df$Explicit <- dat1$ER_B

df$ExplicitWB <- dat1$ER_W - dat1$ER_B
df$ExplicitWL <- dat1$ER_W - dat1$ER_L
df$ExplicitWA <- dat1$ER_W - dat1$ER_A
 
df$ExplicitBW <- dat1$ER_B - dat1$ER_W
df$ExplicitBL <- dat1$ER_B - dat1$ER_L
df$ExplicitBA <- dat1$ER_B - dat1$ER_A

df$ExplicitLW <- dat1$ER_L - dat1$ER_W
df$ExplicitLB <- dat1$ER_L - dat1$ER_B
df$ExplicitLA <- dat1$ER_L - dat1$ER_A


df$ExplicitAW <- dat1$ER_A - dat1$ER_W
df$ExplicitAB <- dat1$ER_A - dat1$ER_B
df$ExplicitAL <- dat1$ER_A - dat1$ER_L


############FIRE
############Flip 2 and 4
# which(colnames(dat1)=='FIRE1.1')
# which(colnames(dat1)=='FIRE2.1')
# which(colnames(dat1)=='FIRE3.1')
# which(colnames(dat1)=='FIRE4.1')

for(i in 110:113){
  dat1[,i]<- as.numeric(dat1[,i])
}


for(i in c(111,113)){
  dat1[,i] <- 5-dat1[,i]
  dat1[,i][dat1[,i]==5] <-0
}

df$Fear1 <- dat1$FIRE1.1
df$Inst1 <- dat1$FIRE2.1
df$Rare1 <- dat1$FIRE3.1
df$Racism1 <- dat1$FIRE4.1
which(colnames(dat1)=='FIRE1.2')
#which(colnames(dat1)=='FIRE2.2')
#which(colnames(dat1)=='FIRE3.2')
#which(colnames(dat1)=='FIRE4.2')

for(i in 143:146){
  dat1[,i]<- as.numeric(dat1[,i])
}



for(i in c(143,146)){
  dat1[,i] <- 5-dat1[,i]
  dat1[,i][dat1[,i]==5] <-0
}

df$Fear2 <- dat1$FIRE1.2
df$Inst2 <- dat1$FIRE2.2
df$Rare2 <- dat1$FIRE3.2
df$Racism2 <- dat1$FIRE4.2

##############Opinions
##############Higher == More counternormative
##############Flip Norm1:3

which(colnames(dat1)=="NORM.1")
which(colnames(dat1)=="NORM.2")
which(colnames(dat1)=="NORM.3")
which(colnames(dat1)=="COUNTER.1")
which(colnames(dat1)=="COUNTER.2")
which(colnames(dat1)=="COUNTER.3")


for(i in 137:142){
  dat1[,i]<- as.numeric(dat1[,i])
}


for(i in c(137,139,141)){
  dat1[,i] <- 5-dat1[,i]
  dat1[,i][dat1[,i]==5] <-0
}

df$sumOpinion<- 0
for(i in 1:nrow(dat1)){
  for(j in c(137:142)){
    if(is.na(dat1[i,j])==F){
      df$sumOpinion[i] <- df$sumOpinion[i] + dat1[i,j]
      
    }
  }
}

df$sumVALIDOpinion <- 0
for(i in 1:nrow(dat1)){
  for(j in c(137:142)){
    df$sumVALIDOpinion[i] <- df$sumVALIDOpinion[i] + (is.na(dat1[i,j])==F)
  }
}


df$Opinion <- (df$sumOpinion/(df$sumVALIDOpinion/6))/24


###########Attention Check 
###########People are very busy these days and many do not have time to follow what goes on in the government. We are testing whether people read questions. To show that you've read this much, answer both "Very interested" and "Extremely interested"
###########It is important that you pay attention, please select "Somewhat true"
##which(colnames(dat1)=="ATT.2")
#which(colnames(dat1)=="attn1")


dat1$att1 <- as.numeric(dat1$ATT.2=="1,2")

dat1$att2 <- ifelse(dat1$attn1 == 1, 1, 0)

dat1$att <- dat1$att1 + dat1$att2

df$Attention <- ifelse(dat1$att == 2, 1, 0)


####################Treatment
####################

sum(dat1$TIME.T2_Page.Submit!="")
sum(dat1$TIME.T3_Page.Submit!="")
sum(dat1$TIME.T4_Page.Submit!="")
sum(dat1$TIME.T1_Page.Submit==""&dat1$TIME.T2_Page.Submit==""&dat1$TIME.T3_Page.Submit==""&dat1$TIME.T4_Page.Submit=="")


df$Treat[dat1$TIME.T1_Page.Submit==""&dat1$TIME.T2_Page.Submit==""&dat1$TIME.T3_Page.Submit==""&dat1$TIME.T4_Page.Submit==""] <- "Control"

df$Treat[dat1$TIME.T1_Page.Submit!=""] <- 'Tweet only'
df$Treat[dat1$TIME.T2_Page.Submit!=""] <- 'Tweet w/ negative comments'
df$Treat[dat1$TIME.T3_Page.Submit!=""] <- 'Tweet w/ positive comments'
df$Treat[dat1$TIME.T4_Page.Submit!=""] <- 'Tweet w/ mixed commnets'

df$Treat <- as.factor(df$Treat)


################other
################
df$Age <- dat1$ageCat
df$age <- dat1$Birth_Year
df$income <- as.numeric(dat1$INCOME)
df$Income <- dat2$INCOME

df$Ideology[dat1$IDEOLOGY==1|dat1$IDEOLOGY==2|dat1$IDEOLOGY==3] <- 3
df$Ideology[dat1$IDEOLOGY==5|dat1$IDEOLOGY==6|dat1$IDEOLOGY==7] <- 1
df$Ideology[dat1$IDEOLOGY==0] <- 2

df$PolAttn <- as.numeric(dat1$POL_ATT)


df$Marry1 <- 5-as.numeric(dat1$ER1.1)
df$Marry1[df$Marry1 == 5] <- 0
df$Marry2 <- 5-as.numeric(dat1$ER1.2)
df$Marry2[df$Marry2 == 5] <- 0


df$Argue <- dat1$ARGUE.FREQ
df$Online <- dat1$SM_Freq
df$Harassed <- dat1$HARRASS.SELF
df$Witness <- dat1$HARRASS.OTHER

df$Gender[dat1$GENDER==0] <-2
df$Gender[dat1$GENDER==1] <-1
df$Gender[dat1$GENDER==2] <-0
df$Education <- dat2$EDU

for(i in 1:length(df$Psycho)){
  
  if(df$Psycho[i] <= .33){
    df$Psycho[i] <- 'Low'
  }else if( df$Psycho[i] > .33 & df$Psycho[i]  <=.67){
    df$Psycho[i] <- 'Medium'
  }else if(df$Psycho[i] > .67){
    df$Psycho[i] <- 'High'}
  
}

df$Psycho <- factor(df$Psycho, levels = c("Low", "Medium", "High"))



for(i in 1:length(df$Troll)){
  
  if(df$Troll[i] <= .33){
    df$Troll[i] <- 'Low'
  }else if( df$Troll[i] > .33 & df$Troll[i]  <=.67){
    df$Troll[i] <- 'Medium'
  }else if(df$Troll[i] > .67){
    df$Troll[i] <- 'High'}
  
}
df$Psycho <- relevel(df$Psycho, ref = 'Low')

df$Troll <- factor(df$Troll, levels = c("Low", "Medium", "High"))

df$Age <- as.factor(df$Age)
df$Treatment <- relevel(df$Treat, ref = "Control")

df$pid <- ifelse(df$Party != 'Republican' & df$Party != 'Democrat', 'Other',NA)
df$pid[df$Party == 'Republican'] <- 'Republican'
df$pid[df$Party == 'Democrat'] <- 'Democrat'


df <- df[(df$Race==0& df$Attention==1),]

df$Treatment <- relevel(df$Treat, ref = "Control" )
df$treat1 <- ifelse(df$Treatment == 'Control',0,1)
df$pid <- NA


write.csv(df, 'dat.csv')















