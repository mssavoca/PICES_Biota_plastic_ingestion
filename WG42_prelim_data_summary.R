#####
# Initial data summary for PICES WG42 meeting on 9/28/20
#####

library(tidyverse)
library(hablar)


# Invertebrates ----
Invert_data <- read_csv("Biota plastic ingestion PICES review_invetebrate(bivalve_only).csv") %>% 
  filter(`Oceanographic province` %in% c("CHIN", "CCAL", "KURO"))%>% 
  retype()

str(Invert_data)
colnames(Invert_data)


Invert_summary <- Invert_data %>% 
  #group_by(`Scientific name`) %>% 
  summarize(
    Ref_num = n_distinct(Reference),
    total_sp = n_distinct(`Scientific name`),
    #med_FO = mean(`FO of plastic`, na.rm = TRUE),
    med_num_particles = median(Mean_num_particles_per_indv, na.rm = TRUE)
  )


# Sea turtles ----
ST_data <- read_csv("Biota plastic ingestion PICES review_Sea Turtle_v1_for import.csv") %>% 
  mutate(
    Reference = as_factor(Reference),
    `FO of plastic` = str_replace(`FO of plastic`, "<1", ""),
    `Mean num particles per indv` = str_replace(`Mean num particles per indv`, "n/a", "")
    ) %>% 
  retype()

str(ST_data)

ST_summary <- ST_data %>% 
 #group_by(`Scientific name`) %>% 
  summarize(
    Ref_num = n_distinct(Reference),
    med_FO = mean(`FO of plastic`, na.rm = TRUE),
    med_num_particles = mean(`Mean num particles per indv`, na.rm = TRUE)
    )


# Seabirds ----
SB_data <- read_csv("Biota plastic ingestion PICES review_Seabirds_11.17.20.csv")

SB_summary <- SB_data %>% 
  # group_by(`Scientific name`) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(Scientific_name),
            med_FO = median(FO_of_plastic, na.rm = TRUE),
            med_num_particles = median(Mean_num_particles_per_indv, na.rm = TRUE),
            total_n = sum(Total_num._sampled, na.rm = TRUE))


# Fish ----

d_poll <- as_tibble(read_csv("Spatial Information_microplastics.csv"))

d_R1_PICES = read_csv("Plastics ingestion records fish master_final_GCB_v2.csv") %>% 
  janitor::clean_names() %>% 
  rename(NwP = nw_p,
         N = n,
         ProvCode = "oceanographic_province_from_longhurst_2007") %>% 
  mutate(
    #prime_forage = na_if(prime_forage, "not listed"),
         ProvCode = ifelse(ProvCode == "BPRL", "BPLR",
                           ifelse(ProvCode == "HUMB", "CHIL",
                                  ifelse(ProvCode == "NAST E", "NASE",
                                         ifelse(ProvCode == "NPSE", "NPPF", ProvCode))))) %>% 
  separate(binomial, into = c("genus", "species"), sep = " ", remove = FALSE) %>% 
  left_join(dplyr::select(d_poll, c(ProvCode, adjacency, mean_poll_abund)), by = "ProvCode") %>% 
  mutate(method_type = factor(method_type),
         adjacency = as_factor(case_when(adjacency == 1 ~ "coastal",
                                         adjacency == 0 ~ "oceanic")),
         source = as_factor(source),
         family = ifelse(family == "Gasterostediae", "Gasterosteidae", 
                         ifelse(family == "Merluccidae", "Merlucciidae", family)),
         adjacency_water = as_factor(ifelse(water_type == "estuarine", "estuarine", 
                                            ifelse(adjacency == "coastal", "coastal", "oceanic")))) %>% 
  
  # Additional edits here for R1
  filter(source != "Sun et al. 2019") %>% 
  
  #filters for PICES data
  filter(ProvCode %in% c("BERS", "PSAW", "PSAE", "KURO", "NPPF", "NPSW", "NPTG", "CCAL", "ARCH", "CHIN")) %>% 
  rename(`Scientific name` = binomial) 

write.csv(d_R1_PICES, "Biota plastic ingestion PICES review_fish_11.18.20.csv")

%>% 
  
  
d_R1_PICES[, c("Scientific name", "common_name", "family")]



d_fish <- read_csv("Plastics ingestion records fish master_final_GCB.csv") %>% 
  rename(`Scientific name` = Binomial)


Fish_data <- read_csv("Biota plastic ingestion PICES review_fish.csv") %>% 
  filter(`Commercial status` %in% c("commercial", "highly commercial"))


%>% 
  left_join(d_fish, by = `Scientific name`)

Fish_summary <- Fish_data %>% 
  # group_by(`Scientific name`) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(`Scientific name`),
            med_FO = median(`FO of plastic`),
            med_num_particles = median(`Mean num particles per indv`, na.rm = TRUE),
            total_n = sum(`Total num. sampled`, na.rm = TRUE))


Fig_3 <- ggplot(
  filter(d_full, includes_microplastic == "Y", Found != "NA"), 
  aes(average_depth, prop_w_plastic, size = N)) +
  geom_point(aes(color = prop_w_plastic), alpha = 0.6) + 
  geom_smooth(aes(weight = N), col = "gray 30", method = "loess", se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Found, scales = "free_y", ncol = 1) +
  coord_flip() +
  scale_x_reverse() +
  scale_color_gradientn(colours = c("steelblue4",
                                    "darkgoldenrod1",
                                    "darkorange", "orangered1",
                                    "firebrick1", "red3", "red4"), 
                        name = "Proportion with \ningested plastic") +
  xlim(350,0) +
  xlab("Average depth (m)") +
  ylab("Proportion of individuals with ingested plastic") +
  theme_classic(base_size = 16)
Fig_3 + guides(size = FALSE, color = FALSE)



study_hist_MT_PICES <- Fish_data %>% 
  group_by(`Publication year`) %>% 
  # summarize(n_studies = n_distinct(Reference)) %>% 
  mutate(`Method type` = factor(`Method type`)) %>% 
  ggplot(aes(`Publication year`, n_studies)) + 
  geom_bar(aes(fill = method_type), stat = "identity") + 
  scale_fill_manual(labels = c("1", "2", "3"), 
                    values = c("darkslateblue",  "darkgoldenrod", "darkslategray")) +
  geom_smooth(se = FALSE, color = "gray48") +
  theme_classic(base_size = 18) +
  labs(fill = "Method",
       x = "Publication year",
       y = "Number of studies") +
  theme(axis.title.x = element_blank())
study_hist_MT 



# Marine mammals ----
MM_data <- read_csv("Biota plastic ingestion PICES review_marine mammals.csv")


MM_summary <- MM_data %>% 
 # group_by(`Scientific name`) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(`Scientific name`),
            med_FO = median(`FO of plastic`),
            med_num_particles = median(`Mean num particles per indv`, na.rm = TRUE),
            total_n = sum(`Total num. sampled`))





