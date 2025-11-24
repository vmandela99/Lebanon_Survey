library(tidyverse)

#import the data
df <- readxl::read_xlsx("SUBUL - FSN Baseline Data - 23072025- Compiled - AI for MEAL Practitioners.xlsx")

# select columns
selected_data <- df %>% 
  select(1, 2, 3, 4, 5, 
         main_food_source_pillar1 = 172)
