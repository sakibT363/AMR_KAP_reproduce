#Figure:1-Distribution of knowledge of antibiotic resistance among parents of school-going children
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(gt)
# Load the dataset
data<-read_excel("D:/CHIRAL project/AMR_Assignment/AMR_KAP_reproduce/Raw data/AMR_KAP_Data.xlsx")
#Select the required columns
data_selected<-data |> 
  select(12:23)
# Reshape data from wide to long format
df_long <- data_selected |> 
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Response")
head(df_long)
# Calculate response percentages
df_long <- df_long %>%
  group_by(Question, Response) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
head(df_long)
# Create the plot
ggplot(df_long, aes(y = reorder(Question, desc(Question)), x = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "black") +            
  scale_fill_manual(values = c("gold", "gray80", "lightblue3")) + 
  theme_minimal() +
  labs(title = "Distribution of Knowledge on Antibiotic Resistance",
       subtitle = "Among Parents of School-Going Children",
       x = "Percentage", y = NULL, fill = "Response") +
  theme(legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 10))
        ggsave("Figures/Figure1_Distribution_of_Knowledge_of_AMR_among_parents.jpg",width = 14, height = 10)








