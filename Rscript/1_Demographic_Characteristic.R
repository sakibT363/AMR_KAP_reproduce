#reading data from excel file in R
library(readxl)
data<-read_excel("D:/CHIRAL project/AMR_Assignment/AMR_KAP_reproduce/Coded data/AMR_KAP_Data_coded.xlsx")
#Loading necessary Libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(gtsummary)
library(gt)

#Table:1-Demographic Characteristics of study participants
#Finding missing data
data_1_11<-data |> 
  select(1:11)
sum(is.na(data_1_11))
#There is one missing data,thus omitted in the table manually
#Creating Table
data|>
  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Tables/Table1.docx")






  
  
  
  
  
  
  
  
  
  
