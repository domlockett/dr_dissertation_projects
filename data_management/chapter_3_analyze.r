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
library(lm_robust)
library(coefplot)
library(pBrackets)
library(estimatr)
library(ggtext)
library(texreg)
mod_texreg <- function(output.file, caption_below, ...) {
  # Capture the output of texreg
  output <- capture.output(texreg(...))
  # Find the position of \begin{center}
  begin_center_pos <- grep("\\\\begin\\{center\\}", output)
  # Find the position of \end{center}
  end_center_pos <- grep("\\\\end\\{center\\}", output)
  # Insert caption below the table with \footnotesize environment
  output <- append(output, paste0("  \\caption*{\\footnotesize ", caption_below, "}"), after = end_center_pos)

  # Replace problematic escape characters
  output <- gsub("(\\\\times)([a-zA-Z])", "\\1 \\2", output)
    # Replace other problematic characters
  output <- gsub("–", "--", output)
  
  # Write the modified output to the file
  writeLines(output, output.file)
}
# Load required libraries
library(ggplot2)
library(stringr)  # For str_wrap

# Set working directory if necessary
setwd("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures")
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
        margin = margin(r = 25),
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


# Define the base colors
base_colors <- c("darkseagreen4", "lightpink3", "navajowhite3", "sienna4", 
                 "steelblue", "coral", "goldenrod", "cadetblue", "tomato", "mediumorchid")

# Function to generate a color map based on unique terms
generate_color_map <- function(terms) {
    base_colors <- c("darkseagreen4", "lightpink3", "navajowhite3", "sienna4", 
                     "steelblue", "coral", "goldenrod", "cadetblue", "tomato", "mediumorchid")
    unique_terms <- unique(terms)
    if (length(unique_terms) > length(base_colors)) {
        stop("There are more terms than available colors. Please add more colors to the base_colors vector.")
    }
    colors <- setNames(base_colors[seq_along(unique_terms)], unique_terms)
    return(colors)
}

local <- 'C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation'

###Data
###Higher values == More Racist, More Counternormative, More Conservative, Male lol
###
dat1 <- read.csv(paste0(local,'/data/chapter_3_data.csv'), header = TRUE, stringsAsFactors = F)

dat1$Treatment <- factor(dat1$Treatment, levels=c('Control','Tweet only','Normative comments','Mixed comments','Counternormative comments'))
dat1$RRV <- (dat1$sumRR/(dat1$sumVALIDRR/6))/24


# Reverse the coding of df$Inst1 and df$Inst2
df$Inst_2 <- 5 - df$Inst_2
df$Racism_2 <- 5 - df$Racism_2
df$date_2 <- 5 - df$date_2




median(dat1$RRV,na.rm=T)
#rr high low is based on the median of racial resentment in the results
dat1$RR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.584){
      dat1$RR_pre[i] <- 'Low RR'
    }else if( dat1$RRV[i] >.583){
      dat1$RR_pre[i] <- 'High RR'
    }  
  }
}

dat1$RR_pre <-factor(dat1$RR_pre, levels=c('High RR','Low RR'))
dat1$high_rr <- ifelse(dat1$RR_pre == 'High RR', 'High racial resentment',"Low racial resentment")
dat1$high_rr <- factor(dat1$high_rr, levels= c('Low racial resentment', 'High racial resentment'))
dat1$marrywb <- dat1$marry_w - dat1$marry_b

f1_a<- lm_robust(date_2~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1_w- lm_robust(date_2~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f1a<- lm_robust(date_2~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f1w<- lm_robust(date_2~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f1)

f2_a<- lm_robust(Fear_2~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2_w<- lm_robust(Fear_2~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f2a<- lm_robust(Fear_2~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f2w<- lm_robust(Fear_2~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f2)

f3_a <- lm_robust( Inst_2~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3_w <- lm_robust( Inst_2~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f3a <- lm_robust( Inst_2~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f3w <- lm_robust( Inst_2~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f3)

f4_a <- lm_robust(Rare_2 ~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4_w <- lm_robust(Rare_2 ~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f4a <- lm_robust(Rare_2 ~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f4w <- lm_robust(Rare_2 ~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f4)

f5_a <- lm_robust(Racism_2 ~  Treatment +bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5_w <- lm_robust(Racism_2 ~  Treatment +bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f5a <- lm_robust(Racism_2 ~  Treatment*high_rr +bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f5w <- lm_robust(Racism_2 ~  Treatment*high_rr +bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f5)

f6_a <- lm_robust(dehuman_wb ~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6_w <- lm_robust(dehuman_wb ~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f6a <- lm_robust(dehuman_wb ~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f6w <- lm_robust(dehuman_wb ~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f6)


f7_a <- lm_robust(marrywb~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7_w <- lm_robust(marrywb~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f7a <- lm_robust(marrywb~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f7w <- lm_robust(marrywb~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
#summary(f7)
#

f8_a<- lm_robust(opinionV~ Treatment+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8_w<- lm_robust(opinionV~ Treatment+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])
f8a<- lm_robust(opinionV~ Treatment*high_rr+bach+ageCat+male+income+nonwhite,data=dat1[dat1$attn1=='Somewhat true',])
f8w<- lm_robust(opinionV~ Treatment*high_rr+bach+ageCat+male+income,data=dat1[dat1$attn1=='Somewhat true'&dat1$Race==0,])


#Treatment x rr all participants
mod_texreg(paste0(local,'/tables/fire_allX.tex'),
          list(f2a,f3a,f4a,f5a),
           caption = "Impact of post/comments and racial resentment level on F.I.R.E battery among all participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:fire_allX',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments','High racial resentment',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 high racial resentment', 'Normative comments \u00D7 high racial resentment','Mixed comments \u00D7 high racial resentment', 'Counter-normative comments \u00D7 high racial resentment'),
           caption_below = "Includes all particpants that passed the attention checks. Regression models predicting racial attitudes based on their treatment condition and level of racial resentment. In all instances, higher values indicate more anti-black racism. Model 1 (Fear) uses fear of other races as the dependent variable. Model 2 (Institutional) uses belief that racial problems are rare and isolated as the dependent variable. Model 3 (Rare) uses the belief that racial problems in the U.S. are rare and isolated as the dependent variable. Model 4 (Empathy) uses respondents' self-reported racial empathy as the dependent variable. Control variables are excluded from the table for brevity. The full regression specifications can be seen on {color{burntumber} Pageref{table:fire_mainX}}.}",
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

mod_texreg(paste0(local,'/tables/opin_allX.tex'),
          list(f2a,f3a,f4a,f5a),
          caption = "Impact of post/comments and racial resentment level on attitudes and opinions of all participants", caption.above = T,
           float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:opin_allX',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments','High racial resentment',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 high racial resentment', 'Normative comments \u00D7 high racial resentment','Mixed comments \u00D7 high racial resentment', 'Counter-normative comments \u00D7 high racial resentment'),
           caption_below = "Includes all particpants that passed the attention checks. Regression models predicting racial attitudes based on their treatment condition and level of racial resentment. In all instances, higher values indicate more anti-black racism. Model 1 (Date) uses the belief that it is acceptable for whites and blacks to date each other as the dependent variable. Model 2 (Dehumanization) uses the dehumanization scale as the dependent variable. Model 3 (Marry) uses respondents' reported comfort with a close relative marrying a black person as the dependent variable. Model 4 (Opinion) uses a scale constructed from 4 items measuring racial policy opinions as the dependent variable.}", include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))



#main analysis among white people no interactionsc
mod_texreg(paste0(local,'/tables/fire_main.tex'),
          list(f2_w,f3_w,f4_w,f5_w), 
          caption = "Impact of post/comments on F.I.R.E battery among White participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:fire_main',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income'),
           caption_below = "Responses to F.I.R.E battery questions. Includes White participants that passed the attention check. F.I.R.E model was constructed to look at four underlying sources of racism (separately). Regression models predicting racial attitudes based on their treatment condition. Model 1 (Fear) uses fear of other races as the dependent variable. Model 2 (Institutional) uses belief that racial problems are rare and isolated as the dependent variable. Model 3 (Rare) uses the belief that racial problems in the U.S. are rare and isolated as the dependent variable. Model 4 (Empathy) uses respondents' self-reported racial empathy as the dependent variable. Control variables are excluded from the table for brevity.}",
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

mod_texreg(paste0(local,'/tables/opin_main.tex'),
          list(f1_w,f6_w,f7_w,f8_w),
          caption = "Impact of post/comments on attitudes and opinions of White participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:opin_main',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income'),
             caption_below = "Includes White particpants that passed the attention checks. Regression models predicting racial attitudes based on their treatment condition. In all instances, higher values indicate more anti-black racism. Model 1 (Date) uses the belief that it is acceptable for whites and blacks to date each other as the dependent variable; Values greater than 0 indicate more anti-black racism. Model 2 (Dehumanization) uses the dehumanization scale as the dependent variable. Model 3 (Marry) uses respondents' reported comfort with a close relative marrying a white person minus their comfort with relatives marrying a black person as the dependent variable; Values greater than 0 indicate more anti-black racism. Model 4 (Opinion) uses a scale constructed from 4 items measuring racial policy opinions as the dependent variable.}",
           include.ci=F,include.rm
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))


# Two main tables, no interactions all participants

mod_texreg(paste0(local,'/tables/fire_all.tex'),
          list(f2_a,f3_a,f4_a,f5_a),
           caption = "Impact of post/comments on F.I.R.E battery among all participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:fire_all',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite'),
           caption_below = "Includes all participants that passed the attention check. Regression models predicting racial attitudes based on their treatment condition and level of racial resentment. In all instances, higher values indicate more anti-black racism. Model 1 (Fear) uses fear of other races as the dependent variable. Model 2 (Institutional) uses belief that racial problems are rare and isolated as the dependent variable. Model 3 (Rare) uses the belief that racial problems in the U.S. are rare and isolated as the dependent variable. Model 4 (Empathy) uses respondents' self-reported racial empathy as the dependent variable. Control variables are excluded from the table for brevity.}",
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

mod_texreg(paste0(local,'/tables/opin_all.tex'),
          list(f2_a,f3_a,f4_a,f5_a),
          caption = "Impact of post/comments level on attitudes and opinions of all participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:opin_all',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite'),
           caption_below = "Includes all particpants that passed the attention checks. Regression models predicting racial attitudes based on their treatment condition. In all instances, higher values indicate more anti-black racism.Model 1 (Date) uses the belief that it is acceptable for whites and blacks to date each other as the dependent variable. Model 2 (Dehumanization) uses the dehumanization scale as the dependent variable. Model 3 (Marry) uses respondents' reported comfort with a close relative marrying a black person as the dependent variable. Model 4 (Opinion) uses a scale constructed from 4 items measuring racial policy opinions as the dependent variable.}",
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

# Treatment x racial resentment, white participants only
mod_texreg(paste0(local,'/tables/fire_mainX.tex'),
          list(f2w,f3w,f4w,f5w), 
          
          caption = "Impact of post/comments and racial resentment level on F.I.R.E battery among White participants",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:fire_mainX',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counter-normative comments','High racial resentment',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Tweet only \u00D7 high racial resentment', 'Normative comments \u00D7 high racial resentment','Mixed comments \u00D7 high racial resentment', 'Counter-normative comments \u00D7 high racial resentment'),
           caption_below = "Includes White participants that passed the attention check. Regression models predicting racial attitudes based on their treatment condition and level of racial resentment. In all instances, higher values indicate more anti-black racism. Model 1 (Fear) uses fear of other races as the dependent variable. Model 2 (Institutional) uses belief that racial problems are rare and isolated as the dependent variable. Model 3 (Rare) uses the belief that racial problems in the U.S. are rare and isolated as the dependent variable. Model 4 (Empathy) uses respondents' self-reported racial empathy as the dependent variable. Control variables are excluded from the table for brevity.}",
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))
