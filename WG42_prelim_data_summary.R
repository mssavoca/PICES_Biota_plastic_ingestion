#####
# Initial data summary for PICES WG42 meeting on 9/28/20
#####

library(tidyverse)
library(hablar)

ST_data <- read_csv("Biota plastic ingestion PICES review_Sea Turtle_v1_for import.csv") %>% 
  mutate(Reference = as_factor(Reference)) %>% 
  retype()

colnames(ST_data)

ST_summary <- ST_data %>% 
  # group_by(`Scientific name`) %>% 
  summarize(Ref_num = n_distinct(Reference))

    #med_FO = median(`FO of plastic`,
                    )
