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
library(lm_robuste4)
library(coefplot)
library(pBrackets)
library(estimatr)
library(ggtext)
library(texreg)
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=F)
}


theme_bw1 <- function(base_size = text.size, base_family = "") {
  label.baseline = TRUE
  text.size = 18
  text.color = "black"
  point.size =2
  dodge.size = 0.9
  font.family=NULL
  theme_bw(base_size = base_size, base_family = base_family 
  ) %+replace% 
    theme(axis.text.x = 
            element_text(size = base_size , 
                         colour = text.color, hjust = 0.5,
                         vjust = 1),  
          axis.text.y = element_text(size = base_size, 
                                     colour = text.color,
                                     hjust = .45, vjust = 0.95,
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


local <- 'C:/Users/dl0ck/OneDrive/Fall 2021/Prospectus 2022'

###Data
###Higher values == More Racist, More Counternormative, More Conservative, Male lol
###
dat1 <- read.csv(paste0(local,'/data/clean_dat4-22.csv'), header = TRUE, stringsAsFactors = F)




dat1$Treatment <- factor(dat1$Treatment, levels=c('Control','Tweet only','Normative comments','Mixed comments','Counternormative comments'))
dat1$RRV <- (dat1$sumRR/(dat1$sumVALIDRR/6))/24



dat1$RR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.5){
      dat1$RR_pre[i] <- 'Low RR'
    }else if( dat1$RRV[i] >=.5){
      dat1$RR_pre[i] <- 'High RR'
    }  
  }
}
dat1$RR_pre <-factor(dat1$RR_pre, levels=c('High RR','Low RR'))

f1a<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1w<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f1b<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f1)

f2a<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2w<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f2b<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f2)

f3a <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3w <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f3b <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f3)

f4a <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4w <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f4b <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f4)

f5a <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5w <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f5b <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f5)



f6a <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6w <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f6b <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f6)

f7a <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7w <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f7b <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f7)
#

f8a<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8w<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f8b<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])



ests <- bind_rows(tidy(f1a)[2,], tidy(f1w)[2,],tidy(f1b)[2,],
                           tidy(f2a)[2,], tidy(f2w)[2,],tidy(f2b)[2,],
                           tidy(f3a)[2,], tidy(f3w)[2,],tidy(f3b)[2,],
                           tidy(f4a)[2,], tidy(f4w)[2,],tidy(f4b)[2,],
                           tidy(f5a)[2,], tidy(f5w)[2,],tidy(f5b)[2,],
                           tidy(f6a)[2,], tidy(f6w)[2,],tidy(f6b)[2,],
                           tidy(f7a)[2,], tidy(f7w)[2,],tidy(f7b)[2,],
                           tidy(f8a)[2,], tidy(f8w)[2,],tidy(f8b)[2,],
                  
                  tidy(f1a)[3,], tidy(f1w)[3,],tidy(f1b)[3,],
                  tidy(f2a)[3,], tidy(f2w)[3,],tidy(f2b)[3,],
                  tidy(f3a)[3,], tidy(f3w)[3,],tidy(f3b)[3,],
                  tidy(f4a)[3,], tidy(f4w)[3,],tidy(f4b)[3,],
                  tidy(f5a)[3,], tidy(f5w)[3,],tidy(f5b)[3,],
                  tidy(f6a)[3,], tidy(f6w)[3,],tidy(f6b)[3,],
                  tidy(f7a)[3,], tidy(f7w)[3,],tidy(f7b)[3,],
                  tidy(f8a)[3,], tidy(f8w)[3,],tidy(f8b)[3,],
                  
                  tidy(f1a)[4,], tidy(f1w)[4,],tidy(f1b)[4,],
                  tidy(f2a)[4,], tidy(f2w)[4,],tidy(f2b)[4,],
                  tidy(f3a)[4,], tidy(f3w)[4,],tidy(f3b)[4,],
                  tidy(f4a)[4,], tidy(f4w)[4,],tidy(f4b)[4,],
                  tidy(f5a)[4,], tidy(f5w)[4,],tidy(f5b)[4,],
                  tidy(f6a)[4,], tidy(f6w)[4,],tidy(f6b)[4,],
                  tidy(f7a)[4,], tidy(f7w)[4,],tidy(f7b)[4,],
                  tidy(f8a)[4,], tidy(f8w)[4,],tidy(f8b)[4,],
                  
                  tidy(f1a)[5,], tidy(f1w)[5,],tidy(f1b)[5,],
                  tidy(f2a)[5,], tidy(f2w)[5,],tidy(f2b)[5,],
                  tidy(f3a)[5,], tidy(f3w)[5,],tidy(f3b)[5,],
                  tidy(f4a)[5,], tidy(f4w)[5,],tidy(f4b)[5,],
                  tidy(f5a)[5,], tidy(f5w)[5,],tidy(f5b)[5,],
                  tidy(f6a)[5,], tidy(f6w)[5,],tidy(f6b)[5,],
                  tidy(f7a)[5,], tidy(f7w)[5,],tidy(f7b)[5,],
                  tidy(f8a)[5,], tidy(f8w)[5,],tidy(f8b)[5,]
                  

            )


ests$outcome <- rep(c(rep("Date other race", 3),
                      rep(paste("Fear" , bold("F"),"IRE"), 3), 
                      rep(paste("Institutionalized"  , "F",bold("I"),"RE"), 3), 
                      rep(paste("Rare" , "FI",bold("R"),"E"), 3),
                      rep(paste("Empathy" , "FIRE",bold("E")), 3),
                      rep("Dehumanization",3),
                      rep("Marry other race", 3),
                      rep("Opinions of announcer", 3),4))




ests$race <- rep(c('All participants','White participants','Black participants'), 32)
ests$Treatment <- rep(c("Tweet only", "Normative comments", "Mixed comments", "Counternormative comments"), each=24)

ests$Treatment=fct_relevel(ests$Treatment,c('Tweet only','Normative comments','Mixed comments','Counternormative comments'))
ests$outcome=fct_relevel(ests$outcome,c('Racism','Rare','Inst','Fear' ,'opinionV','date', 'marrybw','dehuman_wb'))

ests[ests$outcome=='marrybw',2:7]<- ests[ests$outcome=='marrybw',2:7]/100
ests[ests$outcome=='opinionV',2:7]<- ests[ests$outcome=='opinionV',2:7]*10
ests[ests$outcome=='dehuman_wb',2:7]<- ests[ests$outcome=='dehuman_wb',2:7]/100




f1a <- ggplot(data = (ests[ests$outcome == 'Fear' | ests$outcome == 'Inst'| ests$outcome == 'Rare'| ests$outcome == 'Racism',]), aes(y = estimate, x = (outcome), color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") +
  scale_x_discrete(labels = c('Empathy','Rare','Institutionalized','Fear'))+
  coord_flip()+ theme(legend.position = "bottom")+
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))
f1a
ggsave(paste0(local,"/images/fireplot.pdf"), width = 15, height = 12, units = "in")





f1b <- ggplot(data = ests[ests$outcome == 'date' |ests$outcome == 'marrybw' |ests$outcome == 'opinionV'| ests$outcome == 'dehuman_wb',]
              , aes(y = estimate, x = outcome, color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") + 
  coord_flip()+ theme(legend.position = "bottom")+
  scale_x_discrete(labels = c('Opinion of announcer','Miscegeny date (general)','Miscegeny marry (familial)','Dehumanization scale'))+
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))
f1b
ggsave(paste0(local,"/images/otherplot.pdf"), width = 16, height = 13, units = "in")

#ests[ests$outcome=='marrybw',2:7]<- ests[ests$outcome=='marrybw',2:7]/100


f1c <- ggplot(data = ests[ ests$outcome == 'dehuman_wb',]
, aes(y = estimate, x = outcome, color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") +
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))+
 coord_flip()+ theme(legend.position = "bottom")
f1c


f1d <- ggplot(data = ests[ ests$outcome == 'opinionV',]
              , aes(y = estimate, x = outcome, color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") + 
  coord_flip()+ theme(legend.position = "bottom")+
  scale_x_discrete(labels = c("Opinion of announcer"))+
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))
f1d

screenreg(list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b,f4a,f4w,f4b,f5a,f5w,f5b), 
          custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black', 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'))

screenreg(list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'))

mod_texreg(paste0(local,'/tables/pre-post105.tex'),
           list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b),  
           custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/pre-post205.tex'),
           list(f4a,f4w,f4b,f5a,f5w,f5b),  
           custom.model.names =  c( 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/post-only05.tex'),
           list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),  
           custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'),
           caption = "Effect of exposure to Tweet on attitudes",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))





































































dat1$RR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.33){
      dat1$RR_pre[i] <- 'Low RR'
    }else if( dat1$RRV[i] >=.33){
      dat1$RR_pre[i] <- 'High RR'
    }  
  }
}
dat1$RR_pre <-factor(dat1$RR_pre, levels=c('High RR','Low RR'))

f1a<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1w<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f1b<- lm_robust(date~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f1)

f2a<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2w<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f2b<- lm_robust(Fear~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f2)

f3a <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3w <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f3b <- lm_robust( Inst~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f3)

f4a <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4w <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f4b <- lm_robust(Rare ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f4)

f5a <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5w <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f5b <- lm_robust(Racism ~  Treatment*RR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f5)



f6a <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6w <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f6b <- lm_robust(dehuman_wb ~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f6)

f7a <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7w <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f7b <- lm_robust(marrybw~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f7)
#

f8a<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8w<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f8b<- lm_robust(opinionV~ Treatment*RR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])



screenreg(list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b,f4a,f4w,f4b,f5a,f5w,f5b), 
          custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black', 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'))

screenreg(list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'))
mod_texreg(paste0(local,'/tables/pre-post1033.tex'),
           list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b),  
           custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/pre-post2033.tex'),
           list(f4a,f4w,f4b,f5a,f5w,f5b),  
           custom.model.names =  c( 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/post-only033.tex'),
           list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),  
           custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'),
           caption = "Effect of exposure to Tweet on attitudes",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))




dat1$loRR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.58){
      dat1$loRR_pre[i] <- 'Low RR'
    }else if( dat1$RRV[i] >=.58){
      dat1$loRR_pre[i] <- 'High RR'
    }  
  }
}
dat1$loRR_pre <-factor(dat1$loRR_pre, levels=c('High RR','Low RR'))




f1a<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1w<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f1b<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f1)

f2a<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2w<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f2b<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f2)

f3a <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3w <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f3b <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f3)

f4a <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4w <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f4b <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f4)

f5a <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5w <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f5b <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f5)



f6a <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6w <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f6b <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f6)

f7a <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7w <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f7b <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f7)
#

f8a<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8w<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f8b<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f8)
screenreg(list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b,f4a,f4w,f4b,f5a,f5w,f5b), 
          custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black', 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'))

screenreg(list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'))


mod_texreg(paste0(local,'/tables/pre-post1058.tex'),
           list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b),  
           custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/pre-post2058.tex'),
           list(f4a,f4w,f4b,f5a,f5w,f5b),  
           custom.model.names =  c( 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))


mod_texreg(paste0(local,'/tables/post-only058.tex'),
           list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),  
           custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'),
           caption = "Effect of exposure to Tweet on attitudes",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))




dat1$loRR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.67){
      dat1$loRR_pre[i] <- 'Low RR'
    }else if( dat1$RRV[i] >=.67){
      dat1$loRR_pre[i] <- 'High RR'
    }  
  }
}
dat1$loRR_pre <-factor(dat1$loRR_pre, levels=c('High RR','Low RR'))


f1a<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1w<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f1b<- lm_robust(date~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f1)

f2a<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2w<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f2b<- lm_robust(Fear~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f2)

f3a <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3w <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f3b <- lm_robust( Inst~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f3)

f4a <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4w <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f4b <- lm_robust(Rare ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f4)

f5a <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5w <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f5b <- lm_robust(Racism ~  Treatment*loRR_pre +pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f5)



f6a <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6w <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f6b <- lm_robust(dehuman_wb ~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f6)

f7a <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7w <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f7b <- lm_robust(marrybw~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f7)
#

f8a<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8w<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f8b<- lm_robust(opinionV~ Treatment*loRR_pre+pid+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==1,])
#summary(f8)
screenreg(list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b,f4a,f4w,f4b,f5a,f5w,f5b), 
          custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black', 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'))

screenreg(list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'))

mod_texreg(paste0(local,'/tables/pre-post1067.tex'),
           list(f1a,f1w,f1b,f2a,f2w,f2b,f3a,f3w,f3b),  
           custom.model.names =  c('Date- all','Date- white','Date- black','Fear- all' ,'Fear- white' ,'Fear- black' ,'Institutional- all','Institutional- white','Institutional- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/pre-post2067.tex'),
           list(f4a,f4w,f4b,f5a,f5w,f5b),  
           custom.model.names =  c( 'Rare- all' ,'Rare- white', 'Rare black','Empathy- all','Empathy- white','Empathy- black'),
           caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



mod_texreg(paste0(local,'/tables/post-only067.tex'),
           list(f6a,f6w,f6b,f7a,f7w,f7b,f8a,f8w,f8b),  
           custom.model.names =  c('Dehumanization- all' ,'Dehumanization- white' ,'Dehumanization- black' ,'Marry (white - black)- all','Marry (white - black)- white','Marry (white - black)- black', 'Opinion- all', 'Opinion- white', 'Opinion- black'),
           caption = "Effect of exposure to Tweet on attitudes",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))
qqq