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
###Higher values == More Racist, More Counternormative, More Conservative, Male lol
###
dat1 <- read.csv(paste0(local,'/data/clean_dat4-22.csv'), header = TRUE, stringsAsFactors = F)




#write.csv(dat1,'data/clean_dat-pivot.csv')


#functions
##global custom theme
####################
#Functions objects 
#and transformations
#for plots tables
#and models
####################

####################
#global custom theme


theme_bw1 <- function(base_size = text.size, base_family = "") {
  label.baseline = TRUE
  text.size = 30
  text.color = "black"
  point.size = 0.5
  dodge.size = 0.9
  font.family=NULL
  theme_bw(base_size = base_size, base_family = base_family 
  ) %+replace% 
    theme(axis.text.x = 
            element_text(size = base_size *  0.9, 
                         colour = text.color, hjust = 0.5,
                         vjust = 1),  
          axis.text.y = element_text(size = base_size, 
                                     colour = text.color,
                                     hjust = 0, vjust = 0.5,
                                     family = font.family), 
          axis.title.y = element_text(size = base_size, 
                                      angle = 180, vjust = 0.01,
                                      hjust = 0.1, 
                                      family = font.family), 
          
          plot.title = element_text(face = "bold", 
                                    family = font.family),
          axis.ticks = element_blank(), 
          
          legend.background = element_blank(), 
          
          legend.key = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          
          
          plot.background = element_blank(), complete = TRUE,
          panel.grid.minor =   element_blank(),
          panel.grid.major =  element_line(colour = "grey87",size=0.5))
}
theme_set(theme_bw1())





dat1$Treatmen <- ifelse(dat1$Treatment =='Control',0,1)

##############
#export tables

#small function that exports tables to .tex file}
#only texreg handles lm_robust-it doesnt do regular tables
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=F)
}

dat1$Treatment <- factor(dat1$Treatment, levels=c('Control','Tweet only','Normative comments','Mixed comments','Counternormative comments'))
dat1$RRV <- (dat1$sumRR/(dat1$sumVALIDRR/6))/24
median(dat1$RRV[dat1$Race ==2], na.rm=T)

f1<- lm(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f1)

f2<- lm(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f2)

f3 <- lm( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f3)

f4<- lm(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f4)

f5 <- lm(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f5)



f6 <- lm(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f6)

f7<- lm(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f7)
#

f8<- lm(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f8)
screenreg(list(f1,f2,f3,f4,f5,f6,f7,f8))






mod_texreg(paste0(local,'/tables/pre-post-white.tex'),
           list(f1,f2,f3,f4,f5),  
           custom.model.names =  c('Date','Fear' ,'Institutional', 'Rare','Empathy'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30-- 44 years old','45-- 49 years old ','60+ years old ','Male', 'Income','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))


mod_texreg(paste0(local,'/tables/post-only-white.tex'),
           list(f6,f7,f8),  
           custom.model.names =  c('Dehumanization' ,'Marry (white - black)', 'Opinion'),
           caption = "Effect of exposure to Tweet on attitudes",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30-- 44 years old','45-- 49 years old ','60+ years old ','Male', 'Income','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

screenreg(list(f1,f6,f8))



f1<- lm(date~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f1)

f2<- lm(Fear~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f2)

f3 <- lm( Inst~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f3)

f4<- lm(Rare ~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f4)

f5 <- lm(Racism ~  Treatment*RRV +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f5)



f6 <- lm(dehuman_wb ~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f6)

f7<- lm(marrybw~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f7)
#

f8<- lm(opin~ Treatment*RRV+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])

plot_model(f1,'eff',terms=c('RRV','Treatment'))
plot_model(f2,'eff',terms=c('RRV','Treatment'))
plot_model(f3,'eff',terms=c('RRV','Treatment'))
plot_model(f4,'eff',terms=c('RRV','Treatment'))
plot_model(f5,'eff',terms=c('RRV','Treatment'))
plot_model(f6,'eff',terms=c('RRV','Treatment'))
plot_model(f7,'eff',terms=c('RRV','Treatment'))
plot_model(f8,'eff',terms=c('RRV','Treatment'))


mod_texreg(paste0(local,'/tables/postonly.tex'),
           list(f11,f21,f31,f41,f51,f61),  
           caption = "Effect of exposure to Tweet on opinions",
           caption.above=T,float.pos='h!',  
           fontsize='footnotesize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Counternormative comments','Normative comments','Mixed comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30-- 44 years old','45-- 49 years old ','60+ years old ','Male', 'Income','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))













f11 <- lm(NORM.1~  Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
summary(f1)

f21<- lm(NORM.2~ Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
#summary(f21)

f31 <- lm( NORM.3~ Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
#summary(f31)

f41<- lm(COUNTER.1 ~ Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
#summary(f41)

f51 <- lm(COUNTER.2 ~  Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
#summary(f51)

f61<- lm(COUNTER.3~ Treatment*loRR_pre+pid+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true',])
#summary(f71)

screenreg(list(f11,f21,f31,f41,f51,f61))