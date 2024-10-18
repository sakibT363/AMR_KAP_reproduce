#Table-3:Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school going children (N=704).
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(gt)
# Load the dataset
data<-read_excel("D:/CHIRAL project/AMR_Assignment/AMR_KAP_reproduce/Coded data/Coded_calculated.xlsx")

# Example categories-customize these as per the data:
knowledge_summary <- data %>%
  summarise(Good = sum(`Percentage Knowledge` >= 65),               
            Moderate = sum(`Percentage Knowledge` >= 40 & `Percentage Knowledge` < 65),
            Poor = sum(`Percentage Knowledge` < 40)) %>%
  mutate(Total = Good + Moderate + Poor,
         Good_perc = round((Good / Total) * 100, 0),
         Moderate_perc = round((Moderate / Total) * 100, 0),
         Poor_perc = round((Poor / Total) * 100, 0))

attitude_summary <- data %>%
  summarise(Positive = sum(`Percentage Attitude` >= 80),            
            Uncertain = sum(`Percentage Attitude` >= 50 & `Percentage Attitude` < 80),
            Negative = sum(`Percentage Attitude` < 50)) %>%
  mutate(Total = Positive + Uncertain + Negative,
         Positive_perc = round((Positive / Total) * 100, 0),
         Uncertain_perc = round((Uncertain / Total) * 100, 0),
         Negative_perc = round((Negative / Total) * 100, 0))

practice_summary <- data %>%
  summarise(Good = sum(`Percentage Practice` >= 80),                 # Replace with correct column
            Misuse = sum(`Percentage Practice` < 80)) %>%
  mutate(Total = Good + Misuse,
         Good_perc = round((Good / Total) * 100, 0),
         Misuse_perc = round((Misuse / Total) * 100, 0))

#Create a final table combining all these summaries
final_table <- tibble(
  Characteristic = c("Knowledge level", "", "", "Attitude level", "", "", "Practice level", ""),
  Category = c("Good", "Moderate", "Poor", "Positive", "Negative", "Uncertain", "Good", "Misuse"),
  Count = c(knowledge_summary$Good, knowledge_summary$Moderate, knowledge_summary$Poor,
            attitude_summary$Positive, attitude_summary$Negative, attitude_summary$Uncertain,
            practice_summary$Good, practice_summary$Misuse),
  Percentage = c(paste0(knowledge_summary$Good_perc, "%"), 
                 paste0(knowledge_summary$Moderate_perc, "%"), 
                 paste0(knowledge_summary$Poor_perc, "%"),
                 paste0(attitude_summary$Positive_perc, "%"), 
                 paste0(attitude_summary$Negative_perc, "%"), 
                 paste0(attitude_summary$Uncertain_perc, "%"),
                 paste0(practice_summary$Good_perc, "%"), 
                 paste0(practice_summary$Misuse_perc, "%"))
)

#Create the formatted table using gt
gt_table <- final_table %>%
  gt() %>%
  tab_header(title = "Characteristic", subtitle = "N = 704") %>%
  cols_label(Characteristic = "Characteristic",
             Category = "Category",
             Count = "Count",
             Percentage = "Percentage") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(Characteristic))
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black"),
    locations = cells_body()
  )

# Display the table
gt_table
# Save the gt table as a docx
gtsave(gt_table, "Tables/Table3.docx")

