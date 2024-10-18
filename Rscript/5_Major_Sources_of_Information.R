#Table-2:Major sources of information about antibiotic parents (N=704).
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(gt)
# Load the dataset
data<-read_excel("D:/CHIRAL project/AMR_Assignment/AMR_KAP_reproduce/Raw data/AMR_KAP_Data.xlsx")
#selecting columns
data_selected<-data |> 
  select(41:49)
#creating table
data_selected |> 
  tbl_summary() |> 
  as_gt() |>
  gtsave("Tables/Table2.docx")
  
  
