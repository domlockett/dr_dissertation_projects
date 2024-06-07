library(readr)
library(dplyr)
library(magrittr)
library(knitr)
library(ggplot2)
library(labelled)
library(haven)
library(tidyverse)
library(OneR)
library(texreg)
library(weights)
setwd("C:/Users/dl0ck/OneDrive/Fall 2021/Grants/Race and Misinfo/tass")

#WEIGHTED MEANS + CIS
wtd.ci <- function(x, weights, conf.level = 0.95) {
  require(Hmisc)
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights, na.rm=T)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  CI <- c(cint * stderr)
  c( Estimate=mx,"CI lower"=CI[1],"CI upper"=CI[2],"Std. Error"=stderr)
}

df <- read.csv(file = 'C:/Users/dl0ck/OneDrive/Fall 2021/Academic work/Grants/Race and Misinfo/tass/8557_WUSTL-Weidenbaum08_20211217.csv', header = TRUE, stringsAsFactors = FALSE)

dat <- df[,c('P_MISINF1','P_MISINF2','MISINF1_TOTALTIME','MISINF2_TOTALTIME','MISINF1A','MISINF2A','MISINF1B','MISINF2B','EDUC5','AGE4','RACETHNICITY','PartyID7','INCOME4','WEIGHT1','WEIGHT2')]

dat$MISINF1A[dat$MISINF1A > 4] <- NA
dat$MISINF1A <- 5 - dat$MISINF1A
dat$post_lat <- dat$MISINF1A

dat$MISINF1B[dat$MISINF1B > 4] <- NA
dat$MISINF1B <- 5 - dat$MISINF1B
dat$pre_lat <- dat$MISINF1B

dat$MISINF2A[dat$MISINF2A > 4] <- NA
dat$MISINF2A <- 5 - dat$MISINF2A
dat$post_black <- dat$MISINF2A

dat$MISINF2B[dat$MISINF2B > 4] <- NA
dat$MISINF2B <- 5 - dat$MISINF2B
dat$pre_black <- dat$MISINF2B




#Table 1: latino experiment- by treatment


mean_lat <- data.frame(matrix(ncol=4, nrow=4))
rownames(mean_lat)<- c('Control:','No correction:','General correction :','Cultural correction (UNIdos):')
colnames(mean_lat) <- c('Estimate','Lower CI','Upper CI','Std. Error')
mean_lat[,1] <- rep(c('pre-treatment ','post-treatment'),4)

##LATINO
#pre
con_lpre <-wtd.ci(dat$MISINF1B[dat$P_MISINF1 == 4], weights=dat$WEIGHT1[dat$P_MISINF1 == 4])
t1_lpre <- wtd.ci(dat$MISINF1B[dat$P_MISINF1 == 1], weights=dat$WEIGHT1[dat$P_MISINF1 == 1])
t2_lpre <- wtd.ci(dat$MISINF1B[dat$P_MISINF1 == 2], weights=dat$WEIGHT1[dat$P_MISINF1 == 2])
t3_lpre <- wtd.ci(dat$MISINF1B[dat$P_MISINF1 == 3], weights=dat$WEIGHT1[dat$P_MISINF1 == 3])

#post
con_lpost <-wtd.ci(dat$MISINF1A[dat$P_MISINF1 == 4],weights=dat$WEIGHT1[dat$P_MISINF1 == 4])
t1lpost  <- wtd.ci(dat$MISINF1A[dat$P_MISINF1 == 1], weights=dat$WEIGHT1[dat$P_MISINF1 == 1])
t2lpost  <- wtd.ci(dat$MISINF1A[dat$P_MISINF1 == 2], weights=dat$WEIGHT1[dat$P_MISINF1 == 2])
t3lpost  <- wtd.ci(dat$MISINF1A[dat$P_MISINF1 == 3], weights=dat$WEIGHT1[dat$P_MISINF1 == 3])




#BL
#pre
con_blpre <-wtd.ci(dat$MISINF2B[dat$P_MISINF2 == 4], weights=dat$WEIGHT1[dat$P_MISINF2 == 4])
t1_blpre <- wtd.ci(dat$MISINF2B[dat$P_MISINF2 == 1], weights=dat$WEIGHT1[dat$P_MISINF2 == 1])
t2_blpre <- wtd.ci(dat$MISINF2B[dat$P_MISINF2 == 2], weights=dat$WEIGHT1[dat$P_MISINF2 == 2])
t3_blpre <- wtd.ci(dat$MISINF2B[dat$P_MISINF2 == 3], weights=dat$WEIGHT1[dat$P_MISINF2 == 3])

#post
con_blpost <-wtd.ci(dat$MISINF2A[dat$P_MISINF2 == 4], weights=dat$WEIGHT1[dat$P_MISINF2 == 4])
t1_blpost <- wtd.ci(dat$MISINF2A[dat$P_MISINF2 == 1], weights=dat$WEIGHT1[dat$P_MISINF2 == 1])
t2_blpost <- wtd.ci(dat$MISINF2A[dat$P_MISINF2 == 2], weights=dat$WEIGHT1[dat$P_MISINF2 == 2])
t3_blpost <- wtd.ci(dat$MISINF2A[dat$P_MISINF2 == 3], weights=dat$WEIGHT1[dat$P_MISINF2 == 3])


con_lpre
con_lpost

t1_lpre
t1lpost

t2_lpre
t2lpost

t3_lpre
t3lpost

con_blpre
con_blpost

t1_blpre
t1_blpost

t2_blpre
t2_blpost

t3_blpre
t3_blpost

##Race 1-White; 2-Black;; 4-Hispanic or Latino
lat_wh <- data.frame(matrix(ncol=4))

for(j in c(4,1,2,3)){
  print(j)
    lat_wh  <-rbind( lat_wh, 
     wtd.ci(dat$MISINF2B[dat$P_MISINF2 == j &  dat$RACETHNICITY ==4], 
      weights=dat$WEIGHT1[dat$P_MISINF2 == j&  dat$RACETHNICITY ==4]))
}
lat_wh<-lat_wh[-1,]
colnames(lat_wh)<-c('estimate','loci','hici','std')
rownames(lat_wh)<-c('Control','No corr','Gen corr','Cultural corr')
lat_wh

lat_bl<-lat_bl[-1,]
colnames(lat_bl)<-c('estimate','loci','hici','std')
rownames(lat_bl)<-c('Control','No corr','Gen corr','Cultural corr')
lat_bl



lat_lat<-lat_lat[-1,]
colnames(lat_lat)<-c('estimate','loci','hici','std')
rownames(lat_lat)<-c('Control','No corr','Gen corr','Cultural corr')
lat_lat

##Race 1-White; 2-Black;; 4-Hispanic or Latino
lat_wh <- data.frame(matrix(ncol=4))

for(j in c(4,1,2,3)){
  print(j)
  lat_wh  <-rbind( lat_wh, 
                   wtd.ci(dat$MISINF2B[dat$P_MISINF2 == j &  dat$RACETHNICITY ==1], 
                          weights=dat$WEIGHT1[dat$P_MISINF1 == j&  dat$RACETHNICITY ==1]))
}
lat_wh<-lat_wh[-1,]
colnames(lat_wh)<-c('estimate','loci','hici','std')
rownames(lat_wh)<-c('Control','No corr','Gen corr','Cultural corr')
lat_wh

lat_bl<-lat_bl[-1,]
colnames(lat_bl)<-c('estimate','loci','hici','std')
rownames(lat_bl)<-c('Control','No corr','Gen corr','Cultural corr')
lat_bl



lat_lat<-lat_lat[-1,]
colnames(lat_lat)<-c('estimate','loci','hici','std')
rownames(lat_lat)<-c('Control','No corr','Gen corr','Cultural corr')


lat_lat



dat$black <- ifelse(dat$RACETHNICITY==2,1,0)
  
dat$latino <- ifelse(dat$RACETHNICITY==4,1,0)

dat$treat_cond <- NA

for(i in 1:nrow(dat)){
  if(dat$P_MISINF2[i] == 4){
    dat$treat_cond[i] <- 'Control'
  }else if(dat$P_MISINF2[i]==1){
    dat$treat_cond[i] <- 'No correction'
  }else if(dat$P_MISINF2[i]==2){
    dat$treat_cond[i] <- 'General correction'
  }else if(dat$P_MISINF2[i] == 3){
    dat$treat_cond[i] <- 'Cultural correction'
  }
}


dat$treat_cond <- factor(dat$treat_cond, levels = c('Control','No correction', 'General correction','Cultural correction'))




# post_treatment = constant + black + latino + treatment_cond + treatment_cont*black + treatment_cond*hispanic
# Just for the experiment about black voters.
# where treatment_cond is a factor (and control is the baseline)


bl_xp <- lm( post_black-pre_black~black*treat_cond+latino*treat_cond ,data=dat, weights = WEIGHT2)

summary(bl_xp)


lat_xp <- lm( post_lat-pre_lat~black*treat_cond + latino*treat_cond,data=dat, weights = WEIGHT2
              )

summary(lat_xp)

mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=F)
}

mod_texreg('misinfo_xp',list(lat_xp, bl_xp))
