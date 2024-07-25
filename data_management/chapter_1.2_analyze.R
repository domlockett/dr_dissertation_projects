
# A clean file for my paper:
library(foreign)
library(readstata13)
library(lme4)
library(sjPlot)
library(sjmisc)
library(gridExtra)
library(gmodels)
library(ggplot2)
library(effects)
library(stargazer)
library(tidyr)
library("RColorBrewer")
library(wesanderson)
library(estimatr)
library(dplyr)
library(margins)
library(multiwayvcov)
library(lmtest)
library(estimatr)
library(texreg)
library(grid)


library(ggplot2)
library(grid)
library(pBrackets)
setwd('C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/data/data')

theme_bw1 <- function(base_size = 18, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text.x = element_text(
        size = base_size, 
        color = "black",
        hjust = 0.5,
        vjust = 1
      ),  
      axis.text.y = element_text(
        size = base_size, 
        color = "black",
        hjust = 0.45, 
        vjust = 0.95,
        family = base_family
      ), 
      axis.title.y = element_text(
        size = base_size, 
        angle = 90,
        margin = margin(r = 10),
        family = base_family
      ), 
      axis.title.x = element_text(
        size = base_size,
        margin = margin(t = 25),
        family = base_family
      ),
      plot.title = element_text(
        size = base_size,
        face = "bold", 
        family = base_family
      ),
      axis.ticks = element_blank(), 
      legend.background = element_blank(), 
      legend.key = element_blank(),
      panel.background = element_blank(), 
      panel.border = element_blank(), 
      strip.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey87", size = 0.5)
    )
}
theme_set(theme_bw1())


#RQ 1: Are individuals capable of distinguishing between strong and weak arguments?
#Evidence of ACB Difference in Rating
dat <- read.csv('cleanShortTASS')


dat1<- read.csv('dataFACTORTASS')
dat1$pid <- as.factor(dat1$P_PARTYID7)
dat1$AGE4 <-as.factor(dat1$AGE4)
data <- read.csv('cleanLongTASS')
dats <- read.csv('cleanShortTASS')

data$pid <- as.factor(data$P_PARTYID7)
data$AGE4 <-as.factor(data$AGE4)
dat$education <- ifelse(dat$EDUC4 <4, 0,1)


summary(subset(dat[,c('Nonwhite','education','AGE','GENDER','P_IDEO','P_PARTYID7','proIMMB','proGCB')],dat$P_GROUPD==1))
summary(subset(dat[,c('Nonwhite','education','AGE','GENDER','P_IDEO','P_PARTYID7','proIMMB','proGCB')],dat$P_GROUPD==2))
datLong <- read.csv('cleanLongTASS')
strong <- ci(subset(datLong$rating,  datLong$strong == 1), na.rm = T)[1:3]
weak <- ci(subset(datLong$rating,  datLong$strong == 0), na.rm = T)[1:3]

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
strongi <- wtd.ci(subset(datLong$rating, datLong$topic==0 & datLong$strong == 1 &is.na(datLong$rating)==F), 
                  weights =subset(datLong$WEIGHT, datLong$topic==0 & datLong$strong == 1 &is.na(datLong$rating)==F))

weaki <- wtd.ci(subset(datLong$rating,  datLong$topic==0 &datLong$strong == 0), 
                weights=subset(datLong$WEIGHT,  datLong$topic==0 &datLong$strong == 0))

stronggc <- wtd.ci(subset(datLong$rating,  datLong$topic==1 &datLong$strong == 1),
                   weights=subset(datLong$WEIGHT,  datLong$topic==1 &datLong$strong == 1))

weakgc <- wtd.ci(subset(datLong$rating,  datLong$topic==1 &datLong$strong == 0), 
                 weights=subset(datLong$WEIGHT,  datLong$topic==1 &datLong$strong == 0))
compare2 <- as.data.frame(rbind(strongi, weaki, stronggc, weakgc))



#Congenial strong 
preCS <- wtd.ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 1&is.na(datLong$rating)==F),
                weights=subset(datLong$WEIGHT, datLong$congenialA == 1 & datLong$strong == 1&is.na(datLong$rating)==F))

#Congenial weak
preCW <- wtd.ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 0&is.na(datLong$rating)==F),
                weights=subset(datLong$WEIGHT, datLong$congenialA == 1 & datLong$strong == 0&is.na(datLong$rating)==F))

#Uncongenial strong
preUS <- wtd.ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 1&is.na(datLong$rating)==F),
                weights=subset(datLong$WEIGHT, datLong$congenialA == 0 & datLong$strong == 1&is.na(datLong$rating)==F))

#Uncongenial weak
preUW <- wtd.ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 0&is.na(datLong$rating)==F),
                weights=subset(datLong$WEIGHT, datLong$congenialA == 0 & datLong$strong == 0&is.na(datLong$rating)==F))

pre1 <- NULL
pre1 <- as.data.frame(rbind(preCS, preCW, preUS, preUW))
pre1 <- as.data.frame(cbind(pre1, 'strength' = (c('Strong', 'Weak', 'Strong', 'Weak'))))
pre1 <- as.data.frame(cbind(pre1, 'congenial' = as.factor(c('Congenial', 'Congenial', 'Uncongenial', 'Uncongenial'))))


#H1

#congenial <- factor(congenial, levels = rev(levels(congenial)))
rq <- ggplot(pre1, ### The data frame to use.
              aes(x = strength, y = Estimate, color = congenial, scale = congenial)) +
  geom_errorbar(aes(ymin = `CI lower`,
                    ymax = `CI upper`),
                width = 0.05,
                size = 0.5) +
  geom_point(shape = c(15, 15, 19, 19),
             size = 2) +
  guides(colour = guide_legend(override.aes = list(shape = c(15, 19)))) +
  scale_shape_manual("Congeniality", values = c(19, 15)) +
  scale_color_manual("", values = c("darkseagreen4","lightpink3")) +
  ylab("Average Rating") +
  guides(colour = guide_legend(override.aes = list(shape = c(15, 19), linetype = c('dashed', 'dotted')))) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  geom_segment(linetype = "dashed", aes(x = strength[1], y = Estimate[1], xend = strength[2], yend = Estimate[2]), data = pre1, color = "darkseagreen4", size = 1) +
  geom_segment(linetype = "dotted", aes(x = strength[1], y = Estimate[3], xend = strength[2], yend = Estimate[4]), data = pre1, color = "lightpink3", size = 1) +
  xlab("Strength of Argument") +
  ylim(1,5)


rq 
ggsave("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_1.0_plot.pdf", width = 12, height = 8, units = "in")


grid.locator(unit = "native")
grid.brackets(170,427,170,293, col ='grey56', type=4 ,ticks=NA)
grid.brackets(800, 502, 800, 602, col ='grey56', type=4 ,ticks=NA)


dat<- read.csv('dataNUMERICTASS')
###Operationalization: Difference in rating of congenial and uncongenial arguments

dat1$college <- ifelse(dat1$EDUC4 <4, 0,1)

#plots

#H1

main1 <- lm_robust(rating ~  (treatment)*(Strength)+ GENDER + college + Nonwhite + +P_IDEO+ pid+ AGE4 , data = subset(dat1), clusters = CaseId, weights=WEIGHT)

main1P <- plot_model(main1, type = 'eff',terms = c("treatment","Strength") ,color = c("sienna4", 'navajowhite3'), line.size = 1, title = "", axis.title = c( "Treatment","Difference in Rating"))
main1P

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/main-2.pdf", width = 12, height = 8, units = "in")
#thinking about the immigration argument being rated substantially lower than the gun control arguments
dat<- read.csv('dataFACTORTASS')

dat$pid <- as.factor(dat$P_PARTYID7)
dat$AGE4 <-as.factor(dat$AGE4)
dat$college <- ifelse(dat$EDUC4 <4, 0,1)

dat$Strength <- NA
dat$Strength[dat$strength==1] <- 'Strong'
dat$Strength[dat$strength==0] <- 'Weak'

dat$Political <- NA
dat$Political[dat$political==0] <- 'Immigration'
dat$Political[dat$political==1] <- 'Gun Control'
dat$Political <-  factor(dat$Political, levels = c('Immigration','Gun Control'))
topic1 <- lm_robust(rating ~ (Strength)*(Political) + GENDER + college + Nonwhite + +P_IDEO+ pid + AGE4, data = subset(dat), clusters = CaseId)
topic1P <- plot_model(topic1, type = 'eff',terms = c("Political","Strength") ,color =c("sienna4", 'navajowhite3'), line.size = 1, title = "", axis.title = c( "Topic","Difference in Rating"),axis.lim=c(0,.99))
topic1P

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/topic-2.pdf", width = 12, height = 8, units = "in")



control1 <- ggplot(h2a, ### The data frame to use.
               aes(x = strength,
                   y = Estimate, color = congenial), linetype = congenial) +
  geom_errorbar(aes(ymin = `CI lower`,
                    ymax = `CI upper`),
                width = 0.05,
                size = 0.5) +
  geom_point(shape = c(15, 15, 19, 19),
             size = 2) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  theme(legend.position = "none") +
  
  scale_color_manual("", values =c('darkseagreen4','lightpink3')) +
  ylab("Average Rating") +
  ylim(1, 5)  +
  guides(colour = guide_legend(override.aes = list(shape = c(15, 19), linetype = c('dashed','dotted')))) +
  geom_segment(linetype = "dashed", aes(x = strength[1], y = Estimate[1], xend = strength[2], yend = Estimate[2]), data = h2a, color ='darkseagreen4', size=1) +
  geom_segment(linetype = "dotted", aes(x = strength[1], y = Estimate[3], xend = strength[2], yend = Estimate[4]), data = h2a, color ='lightpink3',size=1)+
  xlab('')


control1

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/control-2.pdf", width = 10, height = 8, units = "in")


treat1 <- ggplot(h2b, ### The data frame to use.
               aes(x = strength,
                   y = Estimate, color = congenial)) +
  geom_errorbar(aes(ymin = `CI lower`,
                    ymax = `CI upper`),
                width = 0.05,
                size = 0.5) +
  geom_point(shape = c(15, 15, 19, 19),
             size = 2) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_color_manual("", values = c('darkseagreen4','lightpink3')) +
  ylab("") +
  ylim(1, 5) +
  guides(colour = guide_legend(override.aes = list(shape = c(15, 19), linetype = c('dashed', 'dotted')))) +
  geom_segment(linetype = "dashed", aes(x = strength[1], y = Estimate[1], xend = strength[2], yend = Estimate[2], colour = "Congenial"), data = h2b, color = 'darkseagreen4', size = 1) +
  geom_segment(linetype = "dotted", aes(x = strength[1], y = Estimate[3], xend = strength[2], yend = Estimate[4], colour = "Uncongenial"), data = h2b, color ='lightpink3' , size = 1) +
  xlab("")

treat1


ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/treat-2.pdf", width = 15, height = 8, units = "in")

dat1<- read.csv('dataNUMERICTASS')
dat1$pid <- as.factor(dat1$P_PARTYID7)
dat1$AGE4 <-as.factor(dat1$AGE4)

#main
top1 <-lm_robust(rating ~  (political)*(strength) +GENDER + EDUC4 + Nonwhite + +P_IDEO+ pid+ AGE4 , data = subset(dat1), clusters = CaseId, weights = WEIGHT)

mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/tables/politics1.tex',list(top1),   custom.model.names  = c("Main Analysis"),  caption.above=T, float.pos='h!',custom.coef.names = c('(Intercept)','Topic','Strength','Gender','Education','Nonwhite','Ideology','Moderate Democrat','Lean Democrat',"Don't Lean/independent/None",'Lean Republican','Moderate Republican','Strong Republican','30-44 years old','45-59 years old','60+ years old','Treatment�Strength of argument'), fontsize='small', include.ci=F, booktabs=T)

main <-lm_robust(rating ~  (treat)*(strength) +GENDER + EDUC4 + Nonwhite + +P_IDEO+ pid+ AGE4 , data = subset(dat1), clusters = CaseId, weights=WEIGHT)


topic <- lm_robust(rating ~ (Strength)*(Political) + GENDER + college + Nonwhite + +P_IDEO+ pid + AGE4, data = subset(dat), clusters = CaseId)
#Immigration only
imm <- lm_robust(rating ~  (treat)*(strength)+ GENDER + EDUC4 + Nonwhite + +P_IDEO+ pid+ AGE4 , data = subset(dat1, dat1$political ==0), clusters = CaseId, weights=WEIGHT)

screenreg(main, include.ci=F)


#Gun control only
gc <- lm_robust(rating ~  (treat)*(strength)+ GENDER + EDUC4 + Nonwhite + +P_IDEO+ pid+ AGE4 , data = subset(dat1,political ==1), clusters = CaseId, weights=WEIGHT)

screenreg(gc, include.ci=F)


# Alt Hypothesis- GC will have a more ACB

mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}



mod_texreg('C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/tables/all_tass.tex',list(main,imm,gc),   custom.model.names  = c(" Main Analysis", "Immigration only","Gun control only"),  caption.above=T, float.pos='h!',custom.coef.names = c('(Intercept)','Treatment','Strength','Gender','Education','Nonwhite','Ideology','Moderate Democrat','Lean Democrat',"Don't Lean/independent/None",'Lean Republican','Moderate Republican','Strong Republican','30-44 years old','45-59 years old','60+ years old','Treatment�Strength of argument'), fontsize='small', include.ci=F, booktabs=T)
library(DescTools)
dat <- dat1
dat$college <- NA
dat$college <- ifelse(dat$EDUC4>3,1,0)


Desc(subset(dat[,c('proIMMB')], dat$treat==0), plotit=F)
Desc(subset(dat[,c('proIMMB')], dat$treat==1), plotit=F)


ggplot(data, aes(x=proGC)) +
  geom_histogram(binwidth=.2, colour="black", fill="sienna4")+
  ggtitle("Gun control")+ ylab("Frequency")+xlab('Opinion')+ 
  scale_x_continuous(breaks=seq(2,9,1))




ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/dist-1.pdf", width = 9, height = 7, units = "in")

  ggplot(data, aes(x=proIMM)) +
    geom_histogram(binwidth=.2, colour="black", fill="darkseagreen4")+
    ggtitle("Immigration")+ ylab("Frequency")+xlab('Opinion')+ 
    scale_x_continuous(breaks=seq(2,9,1))

ggsave("C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/plots_publish/dist-2.pdf", width = 9, height = 7, units = "in")
  