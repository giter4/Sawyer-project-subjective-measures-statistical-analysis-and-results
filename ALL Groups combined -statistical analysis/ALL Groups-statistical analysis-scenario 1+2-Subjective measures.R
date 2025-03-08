#all Groups analysis:

#install.packages("readxl")
#install.packages("lmerTest")


#libraries:
library(ggplot2)
library(dplyr)  
library(readxl)
library(stats) 
library(lme4)
library(lmerTest)
#---------------------------#

# all groups analysis:

file_path <- "C:\\Users\\mayag\\Desktop\\תשפד סמסטר ב 2024\\פרוייקט גמר\\statistical analysis\\All groups-statistical analysis-scenario 1+2-Subjective measures.xlsx"

# Read data from the sheet "All Groups"
data <- read_excel(path = file_path, sheet = "All Groups")
str(data)

colnames(data) <- c("Groups","Participant_Number","Gender","Age_Group","Age","Explanation_satisfaction_Scale","Trust_Scale","Fluency_of_interaction","Adaptability")

#---------------------------------------------------------------------------------------------#

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

#1.Explanation_satisfaction_Scale
# Kruskal-Wallis Test: This test determines if there are significant differences between the four groups.
kruskal.test(Explanation_satisfaction_Scale ~ Groups, data = data)
#_---------------------------------------------#

# 2.Trust Scale (average):

# Two-Way ANOVA for Trust Scale (with Groups and Adaptability as independent variables)
anova_result <- aov(Trust_Scale ~ Groups * Adaptability, data = data)
summary(anova_result)

# Convert Adaptability to a factor
data$Adaptability <- as.factor(data$Adaptability)

# Run the Two-Way ANOVA again
anova_result <- aov(Trust_Scale ~ Groups * Adaptability, data = data)
summary(anova_result)

# Tukey's HSD test
TukeyHSD(anova_result)

#---------------------------------------------------------------------

# 3.Fluency of Interaction (average) :

# Kruskal-Wallis Test: This test determines if there are significant differences between the four groups.
kruskal.test(Fluency_of_interaction ~ Groups, data = data)



