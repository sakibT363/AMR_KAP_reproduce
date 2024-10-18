#Table-5:Factors associated with the level of attitude among parents of school-going children (N=704)
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(nnet)
library(report)
library(tidyverse)
library(gt)
# Load the dataset
data<-read_excel("D:/CHIRAL project/AMR_Assignment/AMR_KAP_reproduce/Coded data/Coded_calculated.xlsx")

#Add Knowledge level category
data <- data %>%
  mutate(Knowledge_Level = case_when(
    `Percentage Knowledge` >= 65 ~ "Good",
    `Percentage Knowledge` >= 40 & `Percentage Knowledge` < 65 ~ "Moderate",
    `Percentage Knowledge` < 40 ~ "Poor"
  ))

# Add Attitude level category
data <- data %>%
  mutate(Attitude_Level = case_when(
    `Percentage Attitude` >= 80 ~ "Positive",
    `Percentage Attitude` >= 50 & `Percentage Attitude` < 80 ~ "Uncertain",
    `Percentage Attitude` < 50 ~ "Negative"
  ))

# Add Practice level category
data <- data %>%
  mutate(Practice_Level = case_when(
    `Percentage Practice` >= 80 ~ "Good",
    `Percentage Practice` < 80 ~ "Misuse"
  ))
data$Attitude_Level<-factor(data$Attitude_Level, levels = c("Positive", "Uncertain", "Negative"))
#creating multivariate logistic regression
reg_model <- multinom(Attitude_Level ~ `Parent’s age (years)` + `Parent’s sex` + 
                        `Parent’s education level` + `Employment status` + 
                        `Family type` + `Your average household income per month (BDT)` + 
                        `Child’s sex` + `Child’s age (years)` + `Number of children`,
                      data = data)

# Summary of the model
summary(reg_model)
#representing it in a table
reg_model |> 
  tbl_regression(exponentiate = TRUE)  |>   
  modify_header(estimate = "**OR**") |>  
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Tables/Table5.docx")

