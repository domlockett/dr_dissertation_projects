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
library(DescTools)
library(ggplot2)
library(grid)
library(pBrackets)
setwd('C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/data/data2')
datLong <- read.csv('cleanLong_orig')
dat<- read.csv('dataNUMERIC_orig')

dat$Treat <- ifelse(dat$treat==0,'Control','Treatment')
dat$Treatment <- as.factor(dat$Treat)
dat$str <- ifelse(dat$strength==0,'Weak','Strong')
dat$Strength <- as.factor(dat$str)
dat$pid <- as.factor(dat$pid)
dat$agecat <- as.factor(dat$agecat)
dat$pol <- ifelse(dat$political==0,'Non-political','Political')

dat$Political <- as.factor(dat$pol)

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




#Congenial strong 
preCS <- ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 1), na.rm = T)[1:3]

#Congenial weak
preCW <- ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 0), na.rm = T)[1:3]

#Uncongenial strong
preUS <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 1), na.rm = T)[1:3]

#Uncongenial weak
preUW <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 0), na.rm = T)[1:3]

pre1 <- NULL
pre1 <- as.data.frame(rbind(preCS, preCW, preUS, preUW))
pre1 <- as.data.frame(cbind(pre1, 'strength' = (c('Strong', 'Weak', 'Strong', 'Weak'))))
pre1 <- as.data.frame(cbind(pre1, 'congenial' = as.factor(c('Congenial', 'Congenial', 'Uncongenial', 'Uncongenial'))))



H2CCS <- ci(subset(datLong$rating, datLong$congenialA== 1 & datLong$strong == 1 & datLong$treat == 0), na.rm = T)[1:3]

H2CCW <- ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 0 & datLong$treat == 0), na.rm = T)[1:3]

H2UCS <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 1 & datLong$treat == 0), na.rm = T)[1:3]

H2UCW <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 0 & datLong$treat == 0), na.rm = T)[1:3]

h2a <- NULL
h2a <- as.data.frame(rbind(H2CCS, H2CCW, H2UCS, H2UCW))
h2a <- as.data.frame(cbind(h2a, 'strength' = as.factor(c('Strong', 'Weak', 'Strong', 'Weak'))))
h2a <- as.data.frame(cbind(h2a, 'congenial' = as.factor(c('Congenial', 'Congenial', 'Uncongenial', 'Uncongenial'))))



H2CTS <- ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 1 & datLong$treat == 1), na.rm = T)[1:3]

H2CTW <- ci(subset(datLong$rating, datLong$congenialA == 1 & datLong$strong == 0 & datLong$treat == 1), na.rm = T)[1:3]

H2UTS <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 1 & datLong$treat == 1), na.rm = T)[1:3]

H2UTW <- ci(subset(datLong$rating, datLong$congenialA == 0 & datLong$strong == 0 & datLong$treat == 1), na.rm = T)[1:3]


h2b <- as.data.frame(rbind(H2CTS, H2CTW, H2UTS, H2UTW))
h2b <- as.data.frame(cbind(h2b, 'strength' = as.factor(c('Strong', 'Weak', 'Strong', 'Weak'))))
h2b <- as.data.frame(cbind(h2b, 'congenial' = as.factor(c('Congenial', 'Congenial', 'Uncongenial', 'Uncongenial'))))

#H1

#pre1$congenial <- factor(pre1$congenial, levels = rev(levels(pre1$congenial)))
rq <- ggplot(pre1, ### The data frame to use.
              aes(x = strength, y =Estimate, color = congenial, scale = congenial)) +
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
  geom_segment(linetype = "dashed", aes(x = strength[1], y = Estimate[1], xend = strength[2], yend = Estimate[2]), data = pre1, color = 'darkseagreen4', size = 1) +
  geom_segment(linetype = "dotted", aes(x = strength[1], y = Estimate[3], xend = strength[2], yend = Estimate[4]), data = pre1, color = 'lightpink3', size=1) +
  xlab("Strength of Argument") +
  ylim(1,5)


rq
#grid.locator(unit = "native")
grid.brackets(170,196,170,60, col ='grey56', type=4 ,ticks=NA)
grid.brackets(800, 363, 800, 465, col ='grey56', type=4 ,ticks=NA)





screenreg(list(fit,noatt,pine,gun),  custom.model.names  = c(" Main Analysis", " With attention check failures","Pineapples only","Gun control only"),  caption.above=T, float.pos='h!',custom.coef.names = c('(Intercept)','Treatment','Strength','Gender','Education','Nonwhite','Ideology','Republican','31-44 years old','45-59 years old','60+ years old','Attention to politics','Need to evaluate','Need for cognition','Political knowledge','Strength of partisanship','Treatment�Strength of argument'), fontsize='small', include.ci=F, booktabs=T)


#political vs nonpolitical

set_theme(base = theme_light(base_size = 25))

pol <- lm_robust(rating ~  Strength*  Political + gender + edu + Nonwhite + +ideology+ pid +  + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat, dat$attnCB == 1 ), clusters = ID)
screenreg(pol, include.ci=F)

polP <- plot_model(pol, type = 'eff',terms = c("Political","Strength") ,color = c("sienna4", 'navajowhite3'), line.size = 1, title = "", axi,s.title = c( "Topic","Difference in Rating"),is.title = c( "Treatment","Difference in Rating"),axis.lim=c(0,.99),digits=2)

polP


polaTT <- lm_robust(rating ~  Strength*  Political + gender + edu + Nonwhite + +ideology+ pid +  + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat), clusters = ID)
screenreg(pol, include.ci=F)

polA <- plot_model(polaTT, type = 'eff',terms = c("Political","Strength") ,color = c("sienna4", 'navajowhite3'), line.size = 1, title = "", axi,s.title = c( "Topic","Difference in Rating"),is.title = c( "Treatment","Difference in Rating"),axis.lim=c(0,.99),digits=2)
polA




ggsave("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/tables/political.pdf", width = 12, height = 8, units = "in")




main <- lm_robust(rating ~  (Treatment)*(Strength) + gender + edu + Nonwhite +ideology+ pid + agecat + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat, dat$attnCB == 1 ), clusters = ID)


mainP <- plot_model(main,type='eff',terms = c("Treatment","Strength") ,color = c("sienna4", 'navajowhite3'), line.size = 1, title = "", axis.title = c( "Treatment","Difference in Rating"), show.values = T)
mainP



ggsave("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/tables/main.pdf", width = 12, height = 8, units = "in")

set_theme(base = theme_light(base_size = 45))

control <- ggplot(h2a, ### The data frame to use.
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
  
  scale_color_manual("", values =  c('darkseagreen4','lightpink3')) +
  ylab("Average Rating") +
  ylim(2, 4.5) +
  guides(colour = guide_legend(override.aes = list(shape = c(15, 19), linetype = c('dashed','dotted')))) +
  geom_segment(linetype = "dashed", aes(x = strength[1], y = Estimate[1], xend = strength[2], yend = Estimate[2]), data = h2a, color ='darkseagreen4', size=1) +
  geom_segment(linetype = "dotted", aes(x = strength[1], y = Estimate[3], xend = strength[2], yend = Estimate[4]), data = h2a, color ='lightpink3',size=1)+
  xlab('')


control

ggsave("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/tables/control.pdf", width = 10, height = 8, units = "in")


treat <- ggplot(h2b, ### The data frame to use.
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

treat

ggsave("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_1.1.1_plot.pdf", width = 15, height = 8, units = "in")

dat$pid <- as.factor(dat$pid)
dat$agecat <- as.factor(dat$agecat)



pol1<- lm_robust(rating ~  (political)*(strength) + gender + edu + Nonwhite + +ideology+ as.factor(pid) + as.factor(agecat) + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat, dat$attnCB == 1) , clusters = ID)
screenreg(pol1,include.ci=F)

polA<- lm_robust(rating ~  (political)*(strength) + gender + edu + Nonwhite + +ideology+ as.factor(pid) + as.factor(agecat) + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat) , clusters = ID)
screenreg(list(pol1,polA),include.ci=F)


#save tables
mod_texreg <- function(output.file, ...) {
  output <- capture.output(texreg(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}
mod_texreg('C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/tables/politics.tex',list(pol1,polA),   custom.model.names  = c("Main analysis", " With attention check failures"),  caption.above=T, float.pos='h!',custom.coef.names = c('(Intercept)','Topic','Strength','Gender','Education','Nonwhite','Ideology','Republican','31-44 years old','45-59 years old','60+ years old','Attention to politics','Need to evaluate','Need for cognition','Political knowledge','Strength of partisanship','Topic�Strength of argument'), fontsize='small',include.ci=F, booktabs=T)



#source('C:/Users/dl0ck/OneDrive/Fall 2021/Third Year Paper/Fall2021Publish/CleanOrig092020.R')
dat<- read.csv('C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/data/data2/dataNUMERIC_orig')

dat$Treat <- ifelse(dat$treat==0,'Control','Treatment')
dat$Treatment <- as.factor(dat$Treat)
dat$str <- ifelse(dat$strength==0,'Weak','Strong')
dat$Strength <- as.factor(dat$str)
dat$pid <- as.factor(dat$pid)
dat$agecat <- as.factor(dat$agecat)
dat$pol <- ifelse(dat$political==0,'Non-political','Political')

###Operationalization: Difference in rating of congenial and uncongenial arguments
# Main Hypothesis- Intervention and strength of argument impact one's rating to be more objective
main1<- lm_robust(rating ~  (treat)*(strength) + gender + edu + Nonwhite + ideology+ as.factor(pid) + as.factor(agecat) + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat, dat$attnCB == 1), clusters = ID )

# BY TOPIC
# 
pine <- lm_robust(rating ~  (treat) * (strength) + gender + edu + Nonwhite + ideology+ as.factor(pid) + as.factor(agecat)+ attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat,dat$attnCB == 1 & dat$political==0), clusters = ID)


gun <- lm_robust(rating ~  (treat) * (strength) +edu+Nonwhite+ideology+strngPart , data = subset(dat, dat$attnCB == 1 &dat$political==1), clusters = ID)

# ATTN CHECK REMOVED
noatt <- lm_robust(rating ~  (treat) * (strength) + gender + edu + Nonwhite + ideology+ as.factor(pid) + as.factor(agecat) + attnPol  + NTE + NFC + polKnow + strngPart, data = subset(dat), clusters = ID)



screenreg(list(main1,noatt,pine,gun), include.ci=F)
mod_texreg('C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/tables/all_orig.tex',list(main1,noatt,pine,gun),   custom.model.names  = c(" Main Analysis", " With attention check failures","Pineapples only","Gun control only"),  caption.above=T, float.pos='h!',custom.coef.names = c('(Intercept)','Treatment','Strength','Gender','Education','Nonwhite','Ideology','Republican','31-44 years old','45-59 years old','60+ years old','Attention to politics','Need to evaluate','Need for cognition','Political knowledge','Strength of partisanship','Treatment�Strength of argument'), fontsize='small',include.ci=F, booktabs=T)




dat$college <- NA
dat$college <- ifelse(dat$edu>3,1,0)





dat <- read.csv('cleanShort')
Desc(subset(dat[,c('gender', 'Nonwhite','age','income',  'ideology', 'college', 'gun_B','pine_B','polKnow','NFC', "attnCB")], dat$treat==0), plotit=F)
Desc(subset(dat[,c('gender', 'Nonwhite','age','income',  'ideology', 'college', 'gun_B','pine_B','polKnow','NFC', "attnCB")], dat$treat==1), plotit=F)

