#Anova test for Starling Vocalizations----

#Library----
library(tidyverse)
library(tidylog)
library(janitor)
library(visdat) #for visualizing missing data
library(lme4) #for fitting mixed effects models, difference between means, doesn't give p values
library(lmerTest) #for statistical inference in mixed models, provides p-values
library(here)
library(emmeans)
library(piecewiseSEM)

#Import
clean_df <- readRDS(here("data/final", "clean_df.RDS"))

#Saving table to pdf
summary(clean_df)


#mixed model effects----
#running the model with paired effects
#Average Low Frequency----
#Average Low Freq - Individual Call only
lmm_1 <- clean_df %>%
  filter(call_type == "Individual Call 1" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_low_freq ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_1)
anova(lmm_1)
rsquared(lmm_1)
plot(lmm_1) # diagnostic plot: constant variance?
hist(residuals(lmm_1)) # diagnostic plot: normally distributed residuals?


#Average Low Freq
#Average Low Freq - Cacophony Call only
lmm_2 <- clean_df %>%
  filter(call_type == "Cacophony" & treatment != "Experimental_PT")%>%
  filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_low_freq ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_2)
anova(lmm_2)
rsquared(lmm_2)
plot(lmm_2) # diagnostic plot: constant variance?
hist(residuals(lmm_2)) # diagnostic plot: normally distributed residuals?


#Average High Freq----
#Average High Freq - Individual Call only
lmm_3 <- clean_df %>%
  filter(call_type == "Individual Call 1" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_high_frequency ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_3)
anova(lmm_3)
rsquared(lmm_3)
plot(lmm_3) # diagnostic plot: constant variance?
hist(residuals(lmm_3)) # diagnostic plot: normally distributed residuals?

#Average High Freq
#Average High Freq - Cacophony only
lmm_4 <- clean_df %>%
  filter(call_type == "Cacophony" & treatment != "Experimental_PT")%>%
  filter(nest_box != "25" & nest_box != "44")%>%
  lmer(average_high_frequency ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_4)
anova(lmm_4)
rsquared(lmm_4)
plot(lmm_4) # diagnostic plot: constant variance?
hist(residuals(lmm_4)) # diagnostic plot: normally distributed residuals?


#Average BW----
#Average BW - Individual Call only 
lmm_5 <- clean_df %>%
  filter(call_type == "Individual Call 1" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_bandwidth ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_5)
anova(lmm_5)
rsquared(lmm_5)
plot(lmm_5) # diagnostic plot: constant variance?
hist(residuals(lmm_5)) # diagnostic plot: normally distributed residuals?

#Average BW - Cacophony only 
lmm_6 <- clean_df %>%
  filter(call_type == "Cacophony" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_bandwidth ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_6)
anova(lmm_6)
rsquared(lmm_6)
plot(lmm_6) # diagnostic plot: constant variance?
hist(residuals(lmm_6)) # diagnostic plot: normally distributed residuals?

#Average Call length----
#Average Call length - Individual Call only 
lmm_7 <- clean_df %>%
  filter(call_type == "Individual Call 1" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_call_length ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_7)
anova(lmm_7)
rsquared(lmm_7)
plot(lmm_7) # diagnostic plot: constant variance?
hist(residuals(lmm_7)) # diagnostic plot: normally distributed residuals?

#Average Call length - Cacophony
lmm_8 <- clean_df %>%
  filter(call_type == "Cacophony" & treatment != "Experimental_PT")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_call_length ~ treatment + (1|loca),
       contrasts = list(treatment = "contr.treatment"),
       REML = TRUE, 
       data =.)

summary(lmm_8)
anova(lmm_8)
rsquared(lmm_8)
plot(lmm_8) # diagnostic plot: constant variance?
hist(residuals(lmm_8)) # diagnostic plot: normally distributed residuals?


#Feeding to emmeans
#emmeans(lmm_1, pairwise ~ treatment)

#attempted pairing the groups in ANOVA but the model was too complicated



#export table to word
write.table(clean_df)
write.table(lmm_1)



#############################################################################################
#ANOVA - variance was 0 when taking location into consideratin, singular fit means variance is 0

lm1 <- clean_df %>%
  filter(call_type == "Individual Call 1" & treatment != "Control")%>%
  #filter(nest_box != "7B" & nest_box != "36T")%>%
  lmer(average_low_freq ~ treatment + (1|nest_box),
       contrasts = list(treatment = "contr.treatment"),
       data =.)
summary(lm1)
anova(lm1)
rsquared(lm1)
hist(residuals(lm1))
