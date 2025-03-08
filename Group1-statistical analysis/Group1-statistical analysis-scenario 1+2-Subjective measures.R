#install.packages("readxl")
#install.packages("lmerTest")

# group1 analysis:

#libraries:
library(ggplot2)
library(dplyr)  
library(readxl)
library(stats) 
library(lme4)
library(lmerTest)

#---------------------------#

file_path <- "C:\\Users\\mayag\\Desktop\\תשפד סמסטר ב 2024\\פרוייקט גמר\\statistical analysis\\All groups-statistical analysis-scenario 1+2-Subjective measures.xlsx"

# Read data from the sheet "Group1"
data <- read_excel(path = file_path, sheet = "Group1")
str(data)

colnames(data) <- c("Group1","Participant_Number","Gender","Age_Group","Age","Explanation_satisfaction_Scale","Trust_Scale","Fluency_of_interaction","Adaptability")


#-------------------------------------------------------------------------#
# 1.Explanation Satisfaction Scale (average):

#Shapiro-Wilk test for normality:

# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Explanation_satisfaction_Scale[data$Adaptability == 1])
# Result: data is normally distributed (p > 0.05).

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Explanation_satisfaction_Scale[data$Adaptability == 0])
# Result: data is normally distributed (p > 0.05).

#-------------------------------------------------------------------------#

# 2.Trust Scale (average):

#Shapiro-Wilk test for normality:

# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Trust_Scale[data$Adaptability == 1])
# Result: data is normally distributed (p > 0.05).

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Trust_Scale[data$Adaptability == 0])
# Result: data is normally distributed (p > 0.05).
#--------------------------------------------------------------------------#
# 3.Fluency of Interaction (average) :

#Shapiro-Wilk test for normality:
# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Fluency_of_interaction[data$Adaptability == 1])
# Result: data is normally distributed (p > 0.05).

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Fluency_of_interaction[data$Adaptability == 0])
# Result: data is normally distributed (p > 0.05).
#-------------------------------------------------------------------------#

# Paired t-tests for Each Measure:

# 1.Explanation Satisfaction Scale:
t.test(data$Explanation_satisfaction_Scale[data$Adaptability == 1], 
       data$Explanation_satisfaction_Scale[data$Adaptability == 0], 
       paired = TRUE)
#### results:
#The p-value is larger than 0.05, there is no statistically significant difference#
#in the mean Explanation Satisfaction Scale scores between the adaptive and non-adaptive conditions.#


# 2.Trust Scale:
t.test(data$Trust_Scale[data$Adaptability == 1], 
       data$Trust_Scale[data$Adaptability == 0], 
       paired = TRUE)
#### results:
#The p-value is larger than 0.05, there is no statistically significant difference#
#in the mean Trust Scale scores between the adaptive and non-adaptive conditions.#


# 3.Fluency of Interaction:
t.test(data$Fluency_of_interaction[data$Adaptability == 1], 
       data$Fluency_of_interaction[data$Adaptability == 0], 
       paired = TRUE)

#### results:
#The p-value is larger than 0.05, there is no statistically significant difference#
#in the mean Fluency_of_interaction scores between the adaptive and non-adaptive conditions.#

#--------------------------------------------------------------------------------------------#


# Mixed-effects model for Explanation Satisfaction
model <- lmer(Explanation_satisfaction_Scale ~ Adaptability + (1 | Participant_Number), data = data)

# Summary of results
summary(model)
#Anova:
anova(model)

#result: The fixed effect of Adaptability on the Explanation Satisfaction Scale was 
#not statistically significant (t = -0.509). This indicates that the adaptive and non-adaptive
#conditions did not have a significant impact on the Explanation Satisfaction Scale when accounting
#for variability among participants.

#-----------------------------------------------------------------------------------------------#

# Mixed-effects mode2 for Trust_Scale
mode2 <- lmer(Trust_Scale ~ Adaptability + (1 | Participant_Number), data = data)

# Summary of results
summary(mode2)
anova(mode2)

#result: The fixed effect of Adaptability on the Trust Scale was not statistically 
#significant (t = -0.111). This suggests that there is no evidence to indicate a difference
#in Trust Scale scores between the adaptive and non-adaptive conditions when accounting for 
#variability among participants.

#------------------------------------------------------------------------------------------------#

# Mixed-effects mode3 for Fluency_of_interaction
mode3 <- lmer(Fluency_of_interaction ~ Adaptability + (1 | Participant_Number), data = data)

# Summary of results
summary(mode3)

anova(mode3) 

#result: The fixed effect of Adaptability on the Fluency_of_interaction was not statistically 
#significant (t = -0.111). This suggests that there is no evidence to indicate a difference
#in Fluency_of_interaction scores between the adaptive and non-adaptive conditions when accounting for 
#variability among participants.


