#install.packages("readxl")
#install.packages("lmerTest")
#install.packages("coin")

#libraries:
library(ggplot2)
library(dplyr)  
library(readxl)
library(stats) 
library(lme4)
library(lmerTest)
library(coin)
#---------------------------#
# group4 analysis:


file_path <- "C:\\Users\\mayag\\Desktop\\תשפד סמסטר ב 2024\\פרוייקט גמר\\statistical analysis\\All groups-statistical analysis-scenario 1+2-Subjective measures.xlsx"

# Read data from the sheet "Group4"
data <- read_excel(path = file_path, sheet = "Group4")
str(data)

colnames(data) <- c("Group4","Participant_Number","Gender","Age_Group","Age","Explanation_satisfaction_Scale","Trust_Scale","Fluency_of_interaction","Adaptability")

#Shapiro-Wilk test for normality:
#-------------------------------------------------------------------------#
# 1.Explanation Satisfaction Scale (average):
# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Explanation_satisfaction_Scale[data$Adaptability == 1])

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Explanation_satisfaction_Scale[data$Adaptability == 0])

#-------------------------------------------------------------------------#
# 2.Trust Scale (average):
# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Trust_Scale[data$Adaptability == 1])

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Trust_Scale[data$Adaptability == 0])
#--------------------------------------------------------------------------#
# 3.Fluency of Interaction (average) :
# For Adaptive Dialog (Adaptability = 1)
shapiro.test(data$Fluency_of_interaction[data$Adaptability == 1])

# For Non-Adaptive Dialog (Adaptability = 0)
shapiro.test(data$Fluency_of_interaction[data$Adaptability == 0])
#-------------------------------------------------------------------------#

#Explanation_satisfaction_Scale:
# Wilcoxon Signed-Rank Test for paired data
wilcox.test(data$Explanation_satisfaction_Scale[data$Adaptability == 1], 
            data$Explanation_satisfaction_Scale[data$Adaptability == 0], 
            paired = TRUE)

# calculating the t value 
wilcoxsign_test(data$Explanation_satisfaction_Scale[data$Adaptability == 1] ~ 
                  data$Explanation_satisfaction_Scale[data$Adaptability == 0], 
                data = data, distribution = "asymptotic")

n <- sum(data$Explanation_satisfaction_Scale[data$Adaptability == 1] != 
           data$Explanation_satisfaction_Scale[data$Adaptability == 0])
df <- n - 1

t_value <- 3.8785 / sqrt(df)
t_value


# Median for Adaptive condition (Adaptability = 1)
median_adaptive <- median(data$Explanation_satisfaction_Scale[data$Adaptability == 1])

# Median for Non-Adaptive condition (Adaptability = 0)
median_non_adaptive <- median(data$Explanation_satisfaction_Scale[data$Adaptability == 0])

# Print the results
cat("Median for Adaptive condition (Adaptability = 1):", median_adaptive, "\n")
cat("Median for Non-Adaptive condition (Adaptability = 0):", median_non_adaptive, "\n")


#----------------------------------------------------------------------------#

# 2.Trust Scale:
t.test(data$Trust_Scale[data$Adaptability == 1], 
       data$Trust_Scale[data$Adaptability == 0], 
       paired = TRUE)

# Mixed-effects mode2 for Trust_Scale
mode2 <- lmer(Trust_Scale ~ Adaptability + (1 | Participant_Number), data = data)

# Summary of results
summary(mode2)
#Anova:
anova(mode2)

#---------------------------------------------------------------------------#
# 3.Fluency of Interaction:
t.test(data$Fluency_of_interaction[data$Adaptability == 1], 
       data$Fluency_of_interaction[data$Adaptability == 0], 
       paired = TRUE)

# Mixed-effects mode3 for Fluency_of_interaction
mode3 <- lmer(Fluency_of_interaction ~ Adaptability + (1 | Participant_Number), data = data)

# Summary of results
summary(mode3)
#Anova:
anova(mode3)

