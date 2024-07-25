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
library(broom)
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
setwd("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures")

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



dat1$RR_pre<-NA
for(i in 1:length(dat1$RRV)){
  if(dat1$RRV[i] !='NaN'& is.na(dat1$RRV[i])==F){
    if(dat1$RRV[i] <.5){
      dat1$RR_pre[i] <- 'Low racial resentment'
    }else if( dat1$RRV[i] >=.5){
      dat1$RR_pre[i] <- 'High racial resentment'
    }  
  }
}
dat1$RR_pre <- factor(dat1$RR_pre, levels = c('High racial resentment', 'Low racial resentment'))



# Running the linear models for each outcome, focusing specifically on 'High racial resentment'


f2w <- lm_robust(Fear ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f2w)

f3w <- lm_robust(Inst ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f3w)

f4w <- lm_robust(Rare ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f4w)

f5w <- lm_robust(Racism ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f5w)

f1w <- lm_robust(date ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f1w)

f6w <- lm_robust(dehuman_wb ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f6w)

f7w <- lm_robust(marrybw ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f7w)

f8w <- lm_robust(opinionV ~ Treatment * I(RR_pre == "High racial resentment") + pid + bach + ageCat + male + income, 
                 data = dat1[dat1$attn1 == 'Somewhat true' & dat1$Race == 0,])
summary(f8w)

# Assuming f1w to f8w are your model objects
# Create a list of all your models
model_list <- list(f1w, f2w, f3w, f4w, f5w, f6w, f7w, f8w)

# Names corresponding to each model for easier identification later
model_names <- c("date", "Fear", "Inst", "Rare", "Racism", "dehuman_wb", "marrybw", "opinionV")

# Tidy each model and add a column to identify the model/data source
ests <- bind_rows(lapply(seq_along(model_list), function(i) {
  tidy(model_list[[i]]) %>%
    mutate(model = model_names[i])
}))

# View the structure of the combined data
print(ests)


terms <- c(
    "Intercept", "Tweet only", "Normative comments",
    "Mixed comments", "Counternormative comments",
    "High racial resentment", "Other PID", "Republican PID", "Bachelor's degree",
    "Age 30--44", "Age 46--49", "Age 60+", 
    "Male", "Income", 
    "Tweet only : High racial resentment", 
    "Normative comments : High racial resentment", 
    "Mixed comments : High racial resentment", 
    "Counternormative comments : High racial resentment"
)

# Ensure the vector length matches the number of rows in ests
# Repeat the vector if your dataset has multiple sets of these terms
terms <- rep(terms, length.out = nrow(ests))

# Add the cleaned-up terms as a new column to the ests data frame
ests$Coefficient <- terms
ests$resent <- as.factor(ifelse(grepl("High racial resentment", ests$term), "High", "Low"))

# View the updated data frame
print(ests)


ests$why <- NA
ests1$why <- factor(ests1$Coefficient, levels =rev( c("Tweet only",
    "Normative comments",
    "Mixed comments",
    "Counternormative comments",
    "Tweet only : High racial resentment",
    "Normative comments : High racial resentment",
    "Mixed comments : High racial resentment",
    "Counternormative comments : High racial resentment")))



# Use a named vector to map old names to new names
old_model_names <- c("date", "Fear", "Inst", "Rare", "Racism", "dehuman_wb", "marrybw", "opinionV")
new_model_names <- c("Interracial dating", "Fear of other races", "Belief in institutionalized racism", "Belief that racism is rare","Empathetic (anger) toward racism", "Dehumanization of blacks compared to whites", "Interracial marriage of family member", "Opinion of announcer")

# Use a named vector to map old names to new names
model_name_mapping <- setNames(new_model_names, old_model_names)

# Replace old model names with new model names
ests1$models <- model_name_mapping[ests1$model]


ests1$why <- NA
ests1$why <- factor(ests2$Coefficient, levels =rev( c("Tweet only",
    "Normative comments",
    "Mixed comments",
    "Counternormative comments",
    "Tweet only : High racial resentment",
    "Normative comments : High racial resentment",
    "Mixed comments : High racial resentment",
    "Counternormative comments : High racial resentment")))

ests1$models <- factor(ests1$models, levels =rev( c("Empathetic (anger) toward racism", "Belief that racism is rare", "Belief in institutionalized racism", "Fear of other races")))



f1 <- ggplot(data = (ests1), aes(y = estimate, x = why, color = why)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~models)+
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high))+
  coord_flip()+
theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Angle the x-axis text
    axis.text.y = element_text(angle = 0, hjust = 1), 
          legend.position = "none",)+
              xlab("Coefficient") + ylab("Estimate") 


f1

ggsave(paste0("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_3.1_plot.pdf"), width = 15, height = 9, units = "in")


ests2 <- ests[
  (ests$outcome %in% c('date', 'dehuman_wb', 'marrybw', 'opinionV')) & 
  (ests$Coefficient %in% c(
    "Tweet only",
    "Normative comments",
    "Mixed comments",
    "Counternormative comments",
    "Tweet only : High racial resentment",
    "Normative comments : High racial resentment",
    "Mixed comments : High racial resentment",
    "Counternormative comments : High racial resentment"
  )),]

ests2$why <- NA
ests2$why <- factor(ests2$Coefficient, levels =rev( c("Tweet only",
    "Normative comments",
    "Mixed comments",
    "Counternormative comments",
    "Tweet only : High racial resentment",
    "Normative comments : High racial resentment",
    "Mixed comments : High racial resentment",
    "Counternormative comments : High racial resentment")))

library(ggplot2)
library(dplyr)

# Calculate mean and standard deviation for each model
scale_params <- ests2 %>%
  group_by(models) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE),
            sd_estimate = sd(estimate, na.rm = TRUE))

# Merge scale parameters back to ests2
ests2 <- ests2 %>%
  left_join(scale_params, by = "models")

# Scale the estimates and confidence intervals
ests2 <- ests2 %>%
  mutate(scaled_estimate = (estimate - mean_estimate) / sd_estimate,
         scaled_conf.low = (conf.low - mean_estimate) / sd_estimate,
         scaled_conf.high = (conf.high - mean_estimate) / sd_estimate)

# Plot using ggplot2 with scaled estimates
ggplot(data = ests2, aes(y = scaled_estimate, x = why, color = why)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) +
  geom_pointrange(aes(y = scaled_estimate,
                      ymin = scaled_conf.low,
                      ymax = scaled_conf.high), 
                  position = position_dodge(width = -.3)) +
  facet_wrap(~models, scales = "fixed") +
  coord_flip() +
  theme_bw1() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  xlab("Coefficient") + ylab("Scaled Estimate")


ests2$models <- model_name_mapping[ests2$model]

ests2$models <- factor(ests2$models, levels =rev( c("Opinion of announcer","Interracial marriage of family member","Interracial dating","Dehumanization of blacks compared to whites")))

f2 <- ggplot(data = (ests2), aes(y = scaled_estimate, x = why, color = why)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~models, scales = "fixed")+
  geom_pointrange(aes(y = scaled_estimate,
                      ymin = scaled_conf.low,
                      ymax = scaled_conf.high))+
  coord_flip()+
theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Angle the x-axis text
    axis.text.y = element_text(angle = 0, hjust = 1), 
          legend.position = "none",)+
              xlab("Coefficient") + ylab("Scaled Estimate") 


f2

ggsave(paste0("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_3.2_plot.pdf"), width = 15, height = 9, units = "in")

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
  coord_flip()+theme_bw1() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),  # Angle the x-axis text
          axis.text.y = element_text(angle = 0, hjust = 1), 
          legend.position = "none",)
f1a

f1b <- ggplot(data = ests[ests$outcome == 'date' | ests$outcome == 'dehuman_wb' |ests$outcome == 'marrybw' | ests$outcome == 'opinionV' ,]
              , aes(y = estimate, x = outcome, color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") + 
    xlab("Coefficient") + ylab("Estimate") +
  coord_flip()+ theme(legend.position = "right")+
  theme_bw1() +
  scale_x_discrete(labels = c('Opinion of announcer','Interracial marriage among family','Dehumanization scale','Interracial dating'))+
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))
f1b
ggsave(paste0("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_3.2_plot.pdf"), width = 15, height = 9, units = "in")



f1c <- ggplot(data = ests[ests$outcome == 'opinionV',]
              , aes(y = estimate, x = outcome, color = Treatment)) +
  geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2) + facet_wrap(~race) +
  geom_pointrange(aes(y = estimate,
                      ymin = conf.low,
                      ymax = conf.high), 
                  position=position_dodge(width = -.3)) +
  #scale_color_manual("Randomized exposure to:", values=c("blue", "red")) +
  xlab("") + ylab("") + 
  coord_flip()+ theme(legend.position = "right")+
      xlab("Coefficients") + ylab("Estimates") +

  scale_x_discrete(labels = c('Opinion of announcer','Interracial marriage of family member','Dehumanization scale','Interracial dating in general'))+
  scale_color_manual( values= c("sienna4", 'navajowhite3',"paleturquoise4","lightpink3"))
f1c
ggsave(paste0("C:/Users/dee_l.DEEZ-WORKPC/Documents/GitHub/doctoral_dissertation/figures/chapter_3.3_opin.pdf"))
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

mod_texreg(paste0(local,'/tables/FIRE.tex'),
          list(f2w,f3w,f4w,f5w), 
          custom.model.names =  c('Fear','Institutional','Rare','Empathy'),
          caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

mod_texreg(paste0(local,'/tables/explicitopinions.tex'),
          list(f1w,f6w,f7w,f8w),custom.model.names =  c('Date','Dehumanization' ,'Marry', 'Opinion'),caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))


mod_texreg(paste0(local,'/tables/FIRE1.tex'),
          list(f2w,f3w,f4w,f5w), 
          custom.model.names =  c('Fear','Institutional','Rare','Empathy'),
          caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))

mod_texreg(paste0(local,'/tables/explicitopinions1.tex'),
          list(f1w,f6w,f7w,f8w),custom.model.names =  c('Date','Dehumanization' ,'Marry', 'Opinion'),caption = "Effect of exposure to Tweet on attitudes (post - pre)",
           caption.above=T,float.pos='h!',  
           fontsize='scriptsize', dcolumn=F, digits =2,use.packages=F, 
           label='table:ra-source-interact',  booktabs =T,  custom.coef.names = c('Constant','Tweet only', 'Normative comments','Mixed comments','Counternormative comments','Low racial resentment','PID: other','PID: republican',"Bachelor's",'30 -- 44 years old','45 -- 49 years old ','60+ years old ','Male', 'Income', 'Nonwhite','Tweet only \u00D7 low RR', 'Counternormative comments \u00D7 low RR','Normative comments \u00D7 low RR', 'Mixed comments \u00D7 low RR'),
           include.ci=F,include.rmse=F, include.adjrs =F,include.n.clusters=F ,custom.gof.names=c('R^2' ,'N'))




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