##
# Script for reading, editing, and analyzing Suse Kuhn's mega database
###

library(tidyverse)
library(readxl)
library(janitor)


d_sk_org <- read_xlsx("Kuhn_plastic_biota_database_original.xlsx", sheet = 1) 

d_sk <- d_sk_org %>% 
  row_to_names(row_number = 2) %>% 
  mutate(YEAR = parse_number(YEAR),
         FO = parse_number(`%FO`)/100,
         Nsam = parse_number(Nsam),
         Min_size = parse_number(Min),
         GROUP = case_when(GROUP == "fish" ~ "fish",
                           GROUP %in% c("invertebrate", "Invertebrate") ~ "invertebrate",
                           GROUP %in% c("marine mammal", "Marine Mammal") ~ "marine mammal",
                           GROUP %in% c("seabird", "Seabird") ~ "seabird",
                           GROUP %in% c("turtle", "Turtle") ~ "sea turtle")) %>% 
  select(-ENDN, -DPR, -Notes_Mat, -Notes_Gen, -PLA:-UNK)   # removing excess rows, could put back in if need be
  
  


# for regional and temporal comparisons

d_sk_analy <- d_sk %>%  
  filter(!Region %in% c("global", "South Africa unspec.", NA),
         GROUP != "NA")  # remove non-specific regions



# FO by year; Comparison between regions, taxa

p_tempo_spatio <- d_sk_analy %>% 
  filter(YEAR > 2009, 
         Min_size < 5,
         FINC == "y"
         ) %>% 
  ggplot(aes(x = YEAR, y = FO)) +
  geom_point(aes(color = GROUP, size = Nsam), alpha = 0.2) +
  geom_smooth(aes(color = GROUP, weight = Nsam), method = "lm") +
  ylim(0,1) +
  facet_wrap(.~Region)
p_tempo_spatio



