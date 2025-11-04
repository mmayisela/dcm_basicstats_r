###########################Exploring Genetic and Demographic Predictors of Left Ventricular Ejection Fraction######################################

#Data of patients from Pineapple hospital who had their LVEF and genotype recorded.


library(dplyr)
library(ggplot2)
library(tableone)
library(gtsummary)
library(gt)
library(tidyverse)

patient_data <- read.csv("/Users/minenhle/Library/CloudStorage/OneDrive-UniversityofWitwatersrand/mini_projects/dcm_rproject/data.csv")

#Change gender,genotype to a categorical variables)
patient_data$Gender <-factor(patient_data$Gender)
patient_data$SNP_TTN <-factor(patient_data$SNP_TTN)

#Check for missing data
colSums(is.na(patient_data))
rowSums(is.na(patient_data))

#Check for duplicates
duplicated(patient_data[,c("ID")])
#Remove duplicates
patient_data<-patient_data[!duplicated(patient_data),]

#Basic summaries
str(patient_data)
summary(patient_data)

######Create baseline characteristics table

#Specify variables to include
vars<- c("SNP_TTN", "Gender", "LVEF", "Age")

#Specify categorical variables
factorVars <- c("SNP_TTN", "Gender")

# Create table (no grouping)
table1 <- CreateTableOne(vars = vars, data = patient_data, factorVars = factorVars)

# Print table
table_1 <- print(table1, showAllLevels = TRUE)

library(knitr)
kable(table_1)

library(xtable)
xtable(table_1)

#Convert to gt table

library(gt)

gt_table <- table_1 %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Baseline Characteristics**")
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left",
    data_row.padding = px(4)
  )

gt_table

#Descriptive and distributional insights

#Distribution of age by gender
agebygender<-ggplot(patient_data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth =10, position = "stack", color = "white", alpha = 0.8) +
  labs(title = "Age Distribution by Gender",
       x = "Age (years)",
       y = "Count") +
  theme_minimal()
agebygender

#Distribution of lvef by age 
lvefbyagegender<-ggplot(patient_data, aes(x = Age, y = LVEF)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~Gender) +
  labs(title = "LVEF vs Age by Gender")

lvefbyagegender

#LVEF by Genotype
lvefgenotype<-ggplot(patient_data, aes(x = SNP_TTN, y = LVEF, fill = SNP_TTN)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "LVEF Distribution by Genotype",
       x = "Genotype",
       y = "LVEF (%)") +
  theme_minimal()

lvefgenotype


#See if there is mean difference in LVEF between male and female
t.test(LVEF ~ Gender, data = patient_data)

##t = 2.7397, df = 95.467, p-value = 0.007339
## we reject null hypothesis, difference is explained, there is statistical difference


#How Genotype varies by age and gender
chisq.test(table(patient_data$SNP_TTN, patient_data$Gender)) 
##X-squared = 0.38044, df = 2, p-value = 0.8268

chisq.test(table(patient_data$SNP_TTN, patient_data$Age))
##X-squared = 178.3, df = 92, p-value = 1.806e-07

##Difference in genotype can be explained by age and gender

#Correlation between Age and lvef

#Check normality
shapiro.test(patient_data$LVEF)

cor.test(patient_data$Age, patient_data$LVEF, method = "pearson")
## = -20.591, df = 98, p-value < 2.2e-16
##alternative hypothesis: true correlation is not equal to 0
##95 percent confidence interval:
##  -0.9325840 -0.8564469
##sample estimates:
##  cor 
##-0.9012545 
summary(patient_data)
