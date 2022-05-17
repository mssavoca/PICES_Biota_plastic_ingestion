
# Code for figures for PICES indicators of plastic ingestion paper----


library(tidyverse)
library(forcats)
library(readxl)
library(janitor)
library(ggpubr)
library(fmsb)
library("ggradar")
library(ggradar2)
library(ggridges)
library(viridis)


# Abbreviate a binomial e.g. Balaenoptera musculus -> B. musculus
abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}


#PICES WG42 data review, summary and plots----


WG42_AllRevData <- read_xlsx("Supplementary data_NP biota ingestion review and bioindictaor results.xlsx", sheet = 1) %>% 
  mutate(`Num. w plastic` = as.numeric(`Num. w plastic`), 
         `Total num. sampled` = as.numeric(`Total num. sampled`),
         `FO of plastic` = as.numeric(`FO of plastic`),
         `Trophic level` = as.numeric(`Trophic level`),
         `Total bioindicator score` = as.numeric(`Total bioindicator score`))


WG42_AllRevData_summ <- WG42_AllRevData %>% 
  #filter(`Total bioindicator score` > 9) %>% 
  #arrange(-`Total bioindicator score`) %>% 
  # #filter(`FO of plastic` > 0) %>% 
  #filter(`FO of plastic` != "NA") %>% 
  filter(`Taxonomic group` == "invertebrate") %>% 
  group_by(
    `Scientific name`
    #`Taxonomic group`
    ) %>% 
  summarise(
    Overall_FO = sum(`Num. w plastic`, na.rm = TRUE)/sum(`Total num. sampled`, na.rm = TRUE),
    `Total bioindicator score` = first(`Total bioindicator score`)
    #Total_spp = n_distinct(`Scientific name`)
    ) %>% 
  arrange(-Overall_FO) 





# Invertebrates

# Invert_data <- read_csv("Biota plastic ingestion PICES review_invetebrate(bivalve_only).csv") %>% 
#   filter(`Oceanographic province` %in% c("CHIN", "CCAL", "KURO"))%>% 
#   retype()

Invert_data <- read_csv("Biota plastic ingestion PICES_invertebrate_rubric_v3.csv") %>% 
  filter(`Oceanographic province` %in% c("CHIN", "CCAL", "KURO")) %>% 
  mutate(Mean_num_particles_per_indv = as.numeric(`Mean num particles per indv (for analysis)`),
         FO_of_plastic = as.numeric(`FO of plastic`))


str(Invert_data)
colnames(Invert_data)


Invert_summary <- Invert_data %>% 
  #filter(`Scientific name` %in% c("Ruditapes philippinarum", "Tapes philippinarum", "Venerupis philippinarum")) %>% 
  #group_by(`Scientific name`) %>% 
  filter(`Total bioindicator score` > 10) %>% 
  summarize(
    Sp_num = n_distinct(`Scientific name`),
    Ref_num = n_distinct(Reference),
    Total_N = sum(`Total num. sample analyzed`, na.rm = TRUE),
    med_FO = mean(FO_of_plastic, na.rm = TRUE),
    med_num_particles = median(Mean_num_particles_per_indv, na.rm = TRUE)
  )


# Sea turtles 
ST_data <- read_csv("Biota plastic ingestion PICES review_Sea Turtle_v2_for import.csv") %>% 
  mutate(
    Reference = as_factor(Reference),
    `Mean num particles per indv` = str_replace(`Mean num particles per indv`, "n/a", "")
  ) %>% 
  mutate(`FO of plastic` = as.double(`FO of plastic`),
         `Total num. sampled` = as.double(`Total num. sampled`),
         `Num. w plastic` = as.double(`Num. w plastic`),
         `Mean num particles per indv` = as.double(`Mean num particles per indv`))


str(ST_data)

ST_summary <- ST_data %>% 
  #group_by(`Scientific name`) %>% 
  #filter(`Total bioindicator score` > 10) %>% 
  summarize(
    Sp_num = n_distinct(`Scientific name`),
    Ref_num = n_distinct(Reference),
    Total_N = sum(`Total num. sampled`, na.rm = TRUE),
    Overall_FO = sum(`Num. w plastic`, na.rm = T)/sum(`Total num. sampled`, na.rm = T),
    med_num_particles = mean(`Mean num particles per indv`, na.rm = TRUE)
  )


# Seabirds
## change to file from 4.30.21
SB_data <- read_csv("Biota plastic ingestion PICES review_Seabirds_11.17.20.csv") %>% 
  rename(`Publication venue`= `Publication year`,
         `Publication year`= `Year of collection`) %>% 
  mutate(`FO of plastic` = as.numeric(`FO of plastic`)/100,
         `Publication year` = parse_number(`Publication year`))

str(SB_data)

SB_summary <- SB_data %>% 
  group_by(`Scientific name`) %>% 
  #filter(`Total bioindicator score` > 10) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(`Scientific name`),
            med_FO = median(`FO of plastic`, na.rm = TRUE),
            med_num_particles = median(`Mean num particles per indv`, na.rm = TRUE),
            total_n = sum(`Total num. sampled`, na.rm = TRUE))



Detect_FO_PubYear <- ggplot() +
  
  geom_point(data = SB_data,
             aes(`Publication year`, `FO of plastic`,
                 color = `FO of plastic`, size = `Total num. sampled`, weight = `Total num. sampled`), alpha = 0.6) +
  geom_smooth(data = SB_data,
              aes(`Publication year`, `FO of plastic`,
                  size = `Total num. sampled`, weight = `Total num. sampled`),
              method = "lm", color = "black") +
  
  #geom_hline(yintercept = 0.26, linetype="dashed", color = "gray30") +
  scale_color_gradientn(colours = c("steelblue4",
                                    "darkgoldenrod1",
                                    "darkorange", "orangered1",
                                    "firebrick1", "red3", "red4")) +
  #scale_size_continuous(breaks = c(1, 10, 100, 500, 1000)) +
  #xlim(2010,2020) + 
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(
    name = "Plastic frequency of occurrence (FO)") +
  
  labs(x = "Publication year",
       size = "Sample size") +
  theme_classic(base_size = 16) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    #axis.title.y.left = element_text(color = "red3"),
    #axis.text.y.left = element_text(color = "red3"),
    axis.title.y.right = element_text(color = "grey45"),
    axis.text.y.right = element_text(color = "grey45"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none")

Detect_FO_PubYear




# Fish

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


#d_R2_PICES <- read_csv("Biota plastic ingestion PICES review_fish_GCB&SKdat.csv")

d_R2_PICES <- read_csv("Biota plastic ingestion PICES fish rubric_completed_v2_ACedits.csv")

str(d_R2_PICES)

Fish_summary <- d_R2_PICES %>% 
  #group_by(`Scientific name`) %>% 
  filter(
    `Total bioindicator score` > 10,
    `Publication year`) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(`Scientific name`),
            ovarall_FO = sum(`Num. w plastic`, na.rm = TRUE)/sum(`Total num. sampled`, na.rm = TRUE),
            sample_size = sum(`Total num. sampled`, na.rm = TRUE),
            med_num_particles = median(`Mean num particles per indv`, na.rm = TRUE))



d_PICES_by_study <- d_R2_PICES %>% 
  group_by(Reference) %>% 
  summarise(Publication_year = first(`Publication year`),
            Smallest_detect_size = first(`Size detection threshold (mm)`))

# plastic ingestion by fish in PICES region over time

Detect_FO_PubYear <- ggplot() +
  
  geom_point(data = d_R2_PICES,
             aes(`Publication year`, `FO of plastic`,
                 color = `FO of plastic`, size = `Total num. sampled`, weight = `Total num. sampled`), alpha = 0.6) +
  geom_smooth(data = d_R2_PICES,
              aes(`Publication year`, `FO of plastic`,
                  size = `Total num. sampled`, weight = `Total num. sampled`),
              method = "lm", color = "black") +
  
  # geom_point(data = filter(d_full_R1_by_study, Publication_year >2009),
  #            aes(Publication_year, 1-Smallest_detect_size), 
  #            alpha = 0.5, shape = 18, color = "blue") +
  geom_smooth(data = d_PICES_by_study,
              aes(Publication_year, 1-Smallest_detect_size),
              method = "lm", color = "grey50") +
  xlim(2009,2020) + 
  #geom_hline(yintercept = 0.26, linetype="dashed", color = "gray30") +
  scale_color_gradientn(colours = c("steelblue4",
                                    "darkgoldenrod1",
                                    "darkorange", "orangered1",
                                    "firebrick1", "red3", "red4")) +
  #scale_size_continuous(breaks = c(1, 10, 100, 500, 1000)) +
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(
    name = "Plastic frequency of occurrence (FO)",
    sec.axis = sec_axis(~rev(.), name = "Minimum size thresold (mm)")) +
  
  labs(x = "Publication year",
       size = "Sample size") +
  theme_classic(base_size = 16) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    #axis.title.y.left = element_text(color = "red3"),
    #axis.text.y.left = element_text(color = "red3"),
    axis.title.y.right = element_text(color = "grey45"),
    axis.text.y.right = element_text(color = "grey45"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none")

Detect_FO_PubYear

dev.copy2pdf(file="FO_and_Min_Size_by_year_PICES.pdf", width=6.75, height=6)


# regression equation stats for reporting in paper

lm_fishplastic_trend <- lm(`FO of plastic`~ `Publication year`, data = d_R2_PICES)
summary(lm_fishplastic_trend)


lm_sizeplastic_trend <- lm(Smallest_detect_size ~ Publication_year, data = d_PICES_by_study)
summary(lm_sizeplastic_trend)



# Marine mammals
MM_data <- read_csv("Biota plastic ingestion PICES review_marine mammals.csv")


MM_summary <- MM_data %>% 
  # group_by(`Scientific name`) %>% 
  summarize(Ref_num = n_distinct(Reference),
            total_sp = n_distinct(`Scientific name`),
            med_FO = median(`FO of plastic`),
            med_num_particles = median(`Mean num particles per indv`, na.rm = TRUE),
            total_n = sum(`Total num. sampled`))






# Data from Kuhn and van Franeker 2021----
d_sk_org <- read_xlsx("Copy of PICES_request_20201215_JP water body edit_v2.xlsx", sheet = 1) 

d_sk <- d_sk_org %>% 
  row_to_names(row_number = 2) %>% 
  mutate(YEAR = parse_number(YEAR),
         FO = parse_number(`%FO`)/100,
         Nsam = parse_number(Nsam),
         Naff = parse_number(Naff),
         pnavg = parse_number(pnavg),
         pgavg = parse_number(pgavg),
         Min_size = parse_number(Min),
         GROUP = case_when(GROUP == "fish" ~ "fish",
                           GROUP %in% c("invertebrate", "Invertebrate") ~ "invertebrate", #split into molluscis and non molluscis?
                           GROUP %in% c("marine mammal", "Marine mammal") ~ "marine mammal",
                           GROUP %in% c("seabird", "Seabird") ~ "seabird",
                           GROUP %in% c("turtle", "Turtle") ~ "sea turtle"),
         Region = ifelse(Region == "North Atlantic", "N. Atlantic",
                         ifelse(Region == "South Atlantic", "S. Atlantic",
                                ifelse(Region == "North Pacific", "N. Pacific",
                                       ifelse(Region == "South Pacific", "S. Pacific",
                                              ifelse(Region == "Mediterranean Sea", "Mediterranean",
                                                     ifelse(Region == "Australian waters", "Australia", Region))))))
  ) %>% 
  select(-ENDN, -DPR, -Notes_Mat, -Notes_Gen, -PLA:-UNK) %>%   # removing excess rows, could put back in if need be
  unite("source_sp", AU:YEAR, sep = " ", remove = FALSE, na.rm = FALSE) %>% 
  mutate(GROUP = ifelse(ORDFAM == "Mollusca", "mollusk", GROUP),
         Complete_GIT_YN = ifelse(GITdet %in% c("c","u", NA), 1,0),  # examined complete GIT y/n
         Chem_digest_YN = ifelse(DIG %in% c("0", "u", NA), 0,1), 
         FINC_YN = ifelse(FINC == "y", 1,0),
         FC_YN = ifelse(FC == "y", 1,0),
         poly_conf_YN = ifelse(POL %in% c("0", NA), 0,1),
         min_YN = ifelse(is.na(Min), 0,1))    #Minimum measure size included
filter(
  #Region == "N. Pacific",
  #GROUP == "fish",
  YEAR > 2009)



#write.csv(d_sk, "Biota plastic ingestion PICES review_fish_SK data.csv")



#database by STUDY
d_sk_by_study <- d_sk %>% 
  group_by(source_sp) %>% 
  summarise(
    Primary_taxa = first(GROUP),  # NOT PERFECT, WILL NEED TO CHANGE
    Region = first(Region),
    Overall_FO = sum(Naff, na.rm = TRUE)/sum(Nsam, na.rm = TRUE),
    N = sum(Nsam),
    Publication_year = first(YEAR),
    Polymer_confirmation = first(poly_conf_YN),
    Chem_digest_YN = first(Chem_digest_YN),
    Complete_GIT = first(Complete_GIT_YN),
    Min_size = first(min_YN),
    FINC = first(FINC_YN),
    FC_YN = first(FC_YN),
    Smallest_detect_size = first(Min),
  )




# SUMMARY TABLE
d_sk_summ <- d_sk %>% 
  drop_na(FO) %>% 
  #filter(YEAR > 2009) %>% 
  # filter(Min_size < 5) %>% 
  # filter(GITdet == "c") %>% 
  # filter(FINC == "y") %>% 
  
  group_by(Region, GROUP) %>% 
  summarise(num_studies = n_distinct(source_sp),
            total_N = sum(Nsam, na.rm = TRUE),
            ovarall_FO = sum(Naff, na.rm = TRUE)/sum(Nsam, na.rm = TRUE),
            wt_avg_FO = weighted.mean(x = FO, w = Nsam, na.rm = TRUE),
            med_FO = median(FO)) %>% 
  #filter(num_studies > 2) %>% 
  arrange(GROUP)




# for regional and temporal comparisons

d_sk_analy <- d_sk %>%
  # Filters that vary as necessary by taxa
  mutate(valid_method = (
    (GROUP == "fish" & GITdet == "c" & Min_size < 5 & FINC == "y" & FC == "y") |
      (GROUP == "seabird" & Isamp == "d") |
      (GROUP == "sea turtle" & GITdet == "c") |
      (GROUP == "mollusk" & Min_size < 5 & FINC == "y" &  FC == "y") |
      (GROUP == "marine mammal")
  )) %>% 
  filter(!Region %in% c("global", "South Africa unspec.", NA),
         GROUP != "NA",
         valid_method == TRUE)  # remove non-specific regions



d_sk_analy_summ <- d_sk_analy %>% 
  drop_na(FO) %>% 
  #filter(YEAR > 2009) %>% 
  #filter(GROUP == "sea turtle") %>% 
  
  group_by(Region, GROUP) %>% 
  summarise(num_studies = n_distinct(source_sp),
            total_N = sum(Nsam, na.rm = TRUE),
            ovarall_FO = sum(Naff, na.rm = TRUE)/sum(Nsam, na.rm = TRUE),
            wt_avg_FO = weighted.mean(x = FO, w = Nsam, na.rm = TRUE),
            med_FO = median(FO)) %>% 
  #filter(num_studies > 2) %>% 
  arrange(GROUP, -num_studies)



d_sk_analy_sp_summ <- d_sk_analy %>% 
  filter(
    #GROUP == "sea turtle",
    Region == "N. Pacific") %>% 
  group_by(GROUP) %>% 
  summarise(num_studies = n_distinct(source_sp),
            
            total_N = sum(Nsam, na.rm = TRUE),
            ovarall_FO = sum(Naff, na.rm = TRUE)/sum(Nsam, na.rm = TRUE),
            wt_avg_FO = weighted.mean(x = FO, w = Nsam, na.rm = TRUE),
            med_num_particles = median(pnavg, na.rm = TRUE),
            med_FO = median(FO))


# Plots----

# color palette for figures
pal <- c("N. Pacific" = "firebrick3", 
         "Indian Ocean" = "gray 30", 
         "Mediterranean" = "gray 30", 
         "N. Atlantic" = "gray30", 
         "Australia" = "gray 30", 
         "Southern Ocean" = "gray 30",
         "Equatorial Pacific"  = "gray30",
         "S. Pacific" = "gray30",
         "S. Atlantic" = "gray30",
         "Arctic Ocean" = "gray 30"
)


# Prior long-term data of plastic ingestion by NP species (Fig. 1)----
#seabird data by time compiled by Jennifer Provencher

Fig_1_data <- read_csv("NP_seabird_rev_data_JP.csv")

Fig_1_data_LL <- read_csv("CHOY-Annual-LF-plastic.csv")

Fig_1_plot_SB <-  ggplot() +
  geom_bar(data = Fig_1_data, stat="identity",
           aes(x = Decade, y = FO/100, fill = Sample_size)) +
  scale_fill_gradientn(breaks=c(25, 250, 500, 750),
                       colours = c("steelblue4", "tomato3", "red4")) +
  facet_wrap(.~Common_name) +
  labs(x = "",
       y = "Frequency of occurrence (FO)",
       fill = "Sample size") +
  theme_bw(base_size = 14) 

Fig_1_plot_SB

dev.copy2pdf(file="Fig.1_SB.pdf", width=12, height=4.5)


#lancetfish data by time from Anela Choy

breaks = 2010:2020

Fig_1_plot_LL <-  ggplot() +
  geom_bar(data = Fig_1_data_LL, stat="identity",
           aes(x = year_collection, y = fo, fill = num_sampled)) +
  scale_fill_gradientn(breaks=c(25, 250, 500, 750),
                       colours = c("steelblue4", "tomato3", "red4")) +
  scale_x_continuous(breaks = 2009:2019) + 
  labs(x = "",
       y = "Frequency of occurrence (FO)",
       fill = "Sample size") +
  theme_bw(base_size = 14) + 
  theme(strip.text = element_text(face = "italic")) 

Fig_1_plot_LL



dev.copy2pdf(file="Fig.1_LL.pdf", width=8, height=4.5)







#boxplot by region and taxa ----


# Expanded dataframe that accounts for sample size with rows
d_sk_analy_bp <- d_sk_analy %>% 
  drop_na(Nsam) %>% 
  map_df(., rep, .$Nsam)


# Function that allows toggling between short and long format dataframes, and change minimum studies per region
bp_data <- function(taxa, shape = c("short", "long"), min_studies) {
  shape <- match.arg(shape)
  df <- if (shape == "short") d_sk_analy else d_sk_analy_bp
  valid_regions <- filter(d_sk_summ_analy, GROUP == taxa, num_studies >= min_studies)
  semi_join(df, valid_regions,  by = "Region")
}

bp_data <- function(taxa, min_studies) {
  df <- if (shape == "short") d_sk_analy else d_sk_analy_bp
  valid_regions <- filter(d_sk_analy_summ, GROUP == taxa, num_studies >= min_studies)
  semi_join(df, valid_regions,  by = "Region")
}



# Combined boxplot figure (Figs. 3 & 4)----

#Fish 
bp_spatio_fish_short <- d_sk_analy %>%
  filter(
    GROUP == "fish",
    Region %in% c("Mediterranean", "N. Atlantic",
                  "N. Pacific", "Indian Ocean"),
    # YEAR > 2009,
    GITdet == "c",
    Min_size < 5,
    FINC == "y",
    FC == "y"
  ) 

bp_spatio_fish_table <- bp_spatio_fish_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
    Total_N = sum(Nsam, na.rm = TRUE),
    #Med_FO = mean(FO, na.rm = TRUE),
    #first_q = quantile(FO, probs = 0.25, na.rm = TRUE)
    Max_pnavg = max(pnavg,  na.rm = TRUE)
    )




bp_spatio_fish <-  ggplot() +
  geom_boxplot(data = filter(d_sk_analy_bp,
                             GROUP == "fish",
                             Region %in% c("Mediterranean", "N. Atlantic",
                                           "N. Pacific", "Indian Ocean")),
               #bp_data("sea turtle", "long", 5),
               aes(x = 
                     # For FO plot
                     fct_relevel(Region, "N. Pacific", "Mediterranean", "Indian Ocean"),
                     # For part per ind plot
                     #fct_relevel(Region, "Mediterranean", "Indian Ocean", "N. Pacific"), 
                   y = FO, color = Region), 
               fill = "grey85", outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(data = bp_spatio_fish_short,
              #bp_data("fish", "short", 5),
              aes(x = Region, y = FO,
                  color = Region, size = Nsam), 
              width = 0.2,  alpha = 0.3) +
  scale_color_manual(values = pal, guide = FALSE) +
  scale_size_continuous(breaks = c(1,10,100,1000),
                        range = c(1, 6)) +
  #ylim(0,4) +
  labs(
    #title = "Fish",
    y = "Frequency of occurrence (FO)",
    x = "",
    #y =  bquote("Number of particles ind"^-1),
    size = "Sample size") + 
  theme_bw(base_size = 14)  +
  theme(plot.title = element_text(hjust = 0.5))
bp_spatio_fish


layer_data(bp_spatio_fish)



bp_spatio_fish_table <- bp_spatio_fish_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
            Total_N = sum(Nsam, na.rm = TRUE),
            Med_FO = median(FO, na.rm = TRUE))



#Sea turtles

bp_spatio_ST_short <- d_sk_analy %>%
  filter(
    GROUP == "sea turtle",
    Region %in% c("Mediterranean", "N. Atlantic",
                  "N. Pacific",  "S. Atlantic"),
    # YEAR > 2009,
    # GITdet == "c"
  )


bp_spatio_ST_table <- bp_spatio_ST_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
            Total_N = sum(Nsam, na.rm = TRUE),
            Med_FO = mean(FO, na.rm = TRUE))


bp_spatio_ST <-  ggplot() +
  geom_boxplot(data = filter(d_sk_analy_bp,
                             GROUP == "sea turtle",
                             Region %in% c("Mediterranean", "N. Atlantic",
                                           "N. Pacific",  "S. Atlantic")),
               #bp_data("sea turtle", "long", 5),
               aes(x = fct_relevel(Region, "N. Pacific",  "S. Atlantic", "Mediterranean"),
                   y = FO, color = Region), 
               fill = "grey85", outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(data = bp_spatio_ST_short,    
              #bp_data("sea turtle", "short", 5), # NOT WORKING; TOO LONG
              aes(x = Region, y = FO,
                  color = Region, size = Nsam), 
              width = 0.2,  alpha = 0.3) +
  scale_color_manual(values = pal, guide = FALSE) +
  # scale_size_continuous(breaks = c(1,10,100),
  #                       range = c(1, 10)) +
  # scale_y_continuous(breaks = c(0,25,50,75,100),
  #                    limits = c(1,110)) +
  labs(
    #title = "Sea turtle",
    x= "",
    y = "Frequency of occurrence (FO)",
    #y =  bquote("Number of particles ind"^-1),
    size = "Sample size") + 
  #ylim(0,110) +
  theme_bw(base_size = 14)  +
  theme(plot.title = element_text(hjust = 0.5))
bp_spatio_ST

layer_data(bp_spatio_ST)





# Seabirds

bp_spatio_SB_short <- d_sk_analy %>% 
  filter(
    GROUP == "seabird", 
    Region %in% c("Indian Ocean",
                  "N. Atlantic", "S. Atlantic",
                  "N. Pacific",  "S. Pacific", "Southern Ocean"),
    # Min_size < 5,
    # Isamp == "d"
  ) 


bp_spatio_SB_table <- bp_spatio_SB_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
            Total_N = sum(Nsam, na.rm = TRUE),
            Med_FO = mean(FO, na.rm = TRUE))


bp_spatio_SB <- ggplot() +
  geom_boxplot(data = filter(d_sk_analy_bp,
                             GROUP == "seabird",
                             Region %in% c("Indian Ocean",
                                           "N. Atlantic", "S. Atlantic",
                                           "N. Pacific",  "S. Pacific", 
                                           "Southern Ocean")),
               aes(x = 
                     # For items per ind
                     # fct_relevel(Region,"S. Pacific", "N. Pacific",
                     #             "N. Atlantic", "Indian Ocean",
                     #                "S. Atlantic",
                     #               "Southern Ocean"),
                     # For FO
                      fct_relevel(Region, "Indian Ocean", "N. Pacific",
                                 "S. Atlantic", "S. Pacific", "N. Atlantic",
                                   "Southern Ocean"),
                   y = FO, color = Region), 
               fill = "grey85", outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(data = bp_spatio_SB_short,
              #bp_data("seabird", "short", 10),
              aes(x = Region, y = FO,
                  color = Region, size = Nsam), 
              width = 0.2,  alpha = 0.3) +
  scale_color_manual(values = pal, guide = FALSE) +
  # scale_size_continuous(breaks = c(1,10,1000),
  #                       range = c(1, 8)) +
  labs(
    #title = "Seabird",
    y = "Frequency of occurrence (FO)",
    x = "",
    #y =  bquote("Number of particles ind"^-1),
    size = "Sample size") + 
  #ylim(0,40) +
  theme_bw(base_size = 14)  +
  theme(plot.title = element_text(hjust = 0.5))
bp_spatio_SB

layer_data(bp_spatio_SB)



#Marine mammal

bp_spatio_MM_short <- d_sk_analy %>% 
  filter(
    GROUP == "marine mammal", 
    Region %in% c( "Mediterranean", 
                   "N. Atlantic", "S. Atlantic",
                   "N. Pacific", "S. Pacific"),
  ) 

bp_spatio_MM_table <- bp_spatio_MM_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
            Total_N = sum(Nsam, na.rm = TRUE),
            Med_FO = mean(FO, na.rm = TRUE))

bp_spatio_MM <-  ggplot() +
  geom_boxplot(data = filter(d_sk_analy_bp,
                             GROUP == "marine mammal",
                             Region %in% c("Mediterranean", 
                                           "N. Atlantic", "S. Atlantic",
                                           "N. Pacific",  "S. Pacific")),
               #bp_data("sea turtle", "long", 5),
               aes(x = fct_relevel(Region, "S. Pacific", "N. Pacific", "Mediterranean", 
                                   "N. Atlantic","S. Atlantic"),
                   y = FO, color = Region), 
               fill = "grey85", outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(data = bp_spatio_MM_short,
              aes(x = Region, y = FO,
                  color = Region, size = Nsam), 
              width = 0.2,  alpha = 0.3) +
  scale_color_manual(values = pal, guide = FALSE) +
  scale_size_continuous(breaks = c(1,10,100,1000,2500),
                        range = c(1, 8)) +
  labs(
    #title = "Marine mammal",
    x = "",
    y = "Frequency of occurrence (FO)",
    #y =  bquote("Number of particles ind"^-1),
    size = "Sample size") + 
  #ylim(0,60) +
  theme_bw(base_size = 14)  +
  theme(plot.title = element_text(hjust = 0.5))
bp_spatio_MM

layer_data(bp_spatio_MM)



bp_spatio_moll_short <- d_sk_analy %>% 
  filter(
    GROUP == "mollusk", 
    Region %in% c( "Mediterranean", 
                   "N. Atlantic",
                   "N. Pacific"),
    # Min_size < 5,
    # FINC == "y" 
  ) 

bp_spatio_moll_table <- bp_spatio_moll_short %>%
  group_by(Region) %>% 
  summarise(Total_sp_sam = n_distinct(SCIN), 
            Total_N = sum(Nsam, na.rm = TRUE),
            Med_FO = mean(FO, na.rm = TRUE),
            Max_pnavg = max(pnavg,  na.rm = TRUE)
            )


bp_spatio_moll <-  ggplot() +
  geom_boxplot(data = filter(d_sk_analy_bp,
                             GROUP == "mollusk",
                             Region %in% c("Mediterranean", "N. Atlantic",
                                           "N. Pacific")),
               aes(x = 
                     #For FO
                     fct_relevel(Region, "N. Pacific", "N. Atlantic", "Mediterranean"), 
                     # For pieces per ind
                     #fct_relevel(Region,  "Mediterranean", "N. Atlantic", "N. Pacific"), 
                   y = FO, color = Region), 
               fill = "grey85", outlier.shape = NA,
               show.legend = FALSE) +
  geom_jitter(data = bp_spatio_moll_short,
              aes(x = Region, y = FO,
                  color = Region, size = Nsam), 
              width = 0.1,  alpha = 0.3) +
  scale_color_manual(values = pal, guide = FALSE) +
  # scale_size_continuous(breaks = c(1,10,50,100,1000),
  #                       range = c(1, 6)) +
  labs(
    #title = "Mollusk",
    x = "",
    y = "Frequency of occurrence (FO)",
    #y =  bquote("Number of particles ind"^-1),
    size = "Sample size") + 
  #ylim(0,8) +
  theme_bw(base_size = 14)  +
  theme(plot.title = element_text(hjust = 0.5))
bp_spatio_moll


layer_data(bp_spatio_moll)



NP_compare_comb_top  <- ggarrange(bp_spatio_fish, bp_spatio_SB, 
                                  widths = c(1,1.5),
                                  labels = c("A","B"), 
                                  font.label = list(size = 16),
                                  legend = "none",
                                  ncol = 2, nrow = 1)
NP_compare_comb_top


NP_compare_comb_bottom  <- ggarrange(bp_spatio_ST, bp_spatio_MM, bp_spatio_moll,
                                     labels = c("C","D","E"), 
                                     font.label = list(size = 16),
                                     legend = "none",
                                     ncol = 3, nrow = 1)
NP_compare_comb_bottom


NP_compare_comb_FO <- ggarrange(NP_compare_comb_top, NP_compare_comb_bottom, 
                                font.label = list(size = 16),
                                legend = "none",
                                ncol = 1, nrow = 2)
NP_compare_comb_FO


dev.copy2pdf(file="sef.pdf", width=16, height=7.5)





# Plastic ingestion by fish in PICES region over time (Fig. 5)----

Fish_FO_PubYear <- ggplot() +
  
  geom_point(data = d_R2_PICES,
             aes(`Publication year`, `FO of plastic`,
                 color = `FO of plastic`, size = `Total num. sampled`, weight = `Total num. sampled`), alpha = 0.6) +
  geom_smooth(data = d_R2_PICES,
              aes(`Publication year`, `FO of plastic`,
                  size = `Total num. sampled`, weight = `Total num. sampled`),
              method = "lm", color = "black") +

  xlim(2009,2020) + 
  # scale_color_gradientn(colours = c("steelblue4",
  #                                   "darkgoldenrod1",
  #                                   "darkorange", "orangered1",
  #                                   "firebrick1", "red3", "red4")) +
  scale_x_continuous(breaks=seq(2010, 2020, 2)) +
  scale_y_continuous(
    name = "Plastic frequency of occurrence (FO)") +
  
  labs(x = "Publication year",
       size = "Sample size") +
  theme_classic(base_size = 16) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    #axis.title.y.left = element_text(color = "red3"),
    #axis.text.y.left = element_text(color = "red3"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none")

Fish_FO_PubYear

dev.copy2pdf(file="Fish_FO_by_year_PICES.pdf", width=6.75, height=6)




Fish_detect_PubYear <- ggplot() +
  
  geom_point(data = d_PICES_by_study,
              aes(Publication_year, Smallest_detect_size), alpha = 0.6) +

  geom_smooth(data = d_PICES_by_study,
              aes(Publication_year, Smallest_detect_size),
              method = "lm", color = "grey50") +
  xlim(2009,2020) +
  scale_x_continuous(breaks=seq(2010, 2020, 2)) +
  scale_y_continuous(name = "Minimum size threshold (mm)") +
  
  labs(x = "Publication year",
       size = "Sample size") +
  theme_classic(base_size = 16) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    #axis.title.y.left = element_text(color = "red3"),
    #axis.text.y.left = element_text(color = "red3"),
    axis.title.y.right = element_text(color = "grey45"),
    axis.text.y.right = element_text(color = "grey45"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none")

Fish_detect_PubYear


dev.copy2pdf(file="Fish_detect_by_year_PICES.pdf", width=6.75, height=6)


Fish_FO_detect_PubYear <- ggarrange(Fish_FO_PubYear, Fish_detect_PubYear, 
                                    labels = c("A","B"), 
                                font.label = list(size = 16),
                                legend = "none",
                                ncol = 2, nrow = 1)
Fish_FO_detect_PubYear

dev.copy2pdf(file="Fish_FO_detect_by_year_PICES_combined.pdf", width=12, height=5)


# regression equation stats for reporting in paper

lm_fishplastic_trend <- lm(`FO of plastic`~ `Publication year`, data = d_R2_PICES)
summary(lm_fishplastic_trend)


lm_sizeplastic_trend <- lm(Smallest_detect_size ~ Publication_year, data = d_PICES_by_study)
summary(lm_sizeplastic_trend)






# Quality assurance plots (Fig. 6)----

# color palette for figures
pal <- c("Chem_digest_YN" = "firebrick2", "Complete_GIT" = "dodgerblue3", 
         "FC_YN" = "coral4", "FINC" = "cyan3", 
         "Min_size" = "grey40", "Polymer_confirmation" = "chocolate2"
         )


bp_all_long <- d_sk_by_study %>% 
  filter(
    Publication_year >2009, Publication_year < 2021,
         Region == "N. Pacific",         #can toggle in and out
         #Primary_taxa == "fish" #can toggle in and out
  ) %>% 
  drop_na(Primary_taxa, Region) %>% 
  pivot_longer(cols = Polymer_confirmation:FC_YN, names_to = "Metric_type", 
               values_to = "Metric_value") %>% 
  
  ggplot() + 
  
  stat_smooth(aes(Publication_year, Metric_value, color = Metric_type),
              method="glm", method.args=list(family="binomial"),formula=y~x, 
              alpha=0.2, size=1) +
  geom_point(aes(Publication_year, Metric_value, color = Metric_type),
             position=position_jitter(height=0.075, width=0.075),
             alpha = 0.4, size = 2) +
  
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "black") +
  
  scale_color_manual(values = pal, labels = c(
  "\nChemical \ndigestion\n",
                                             "\nComplete GIT \nanalyzed\n",

                                             "\nFiber control \nused\n",
                                              "\nFibers \nanalyzed\n",
                                             "\nMinimum size \nthreshold reported\n",
                                             "\nPolymer \nconfirmation\n"
                                             )
                     ) +
  
  labs(x = "Publication year",
       y = "Quality assurance \nmetrics described",
       color = "Quality assurance \nmetric") +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(breaks = c(0,1), labels = c("No","Yes")) +
  theme_classic(base_size = 18) +
  #facet_wrap(.~Primary_taxa) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.spacing.y = unit(0.25, 'cm'),
    legend.key.size = unit(0.75, "cm"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14))

bp_all_long


dev.copy2pdf(file="Qual_assure_NP_all_comb_R1.pdf", width=10.5, height=6)







# Density plot of rubric scores of all species---- 

d_fish_rubric_eval <- d_R2_PICES %>% 
  select(`Scientific name`,`Common name`, 
         `Degree of prior sampling`, `Plastic FO in PICES region`, 
         Residency, `Distribution in PICES region`,  `Distribution globally`,
         `Already an indicator?`, `Threat of human exposure`, `Total bioindicator score`) %>% 
  group_by(`Scientific name`) %>% 
  summarise(`Degree of prior sampling` = first(`Degree of prior sampling`), 
            `Plastic FO in PICES region`  = first(`Plastic FO in PICES region`), 
            Residency = first(Residency), 
            `Distribution in PICES region` = first(`Distribution in PICES region`),
            `Distribution globally` = first(`Distribution globally`),
            `Already an indicator?` = first(`Already an indicator?`), 
            `Threat of human exposure` = first(`Threat of human exposure`), 
            `Total bioindicator score` = first(`Total bioindicator score`)) %>% 
  mutate(Group = "fishes") 

d_seabird_rubric_eval <- read_csv("Seabird rubric ranking.csv") %>% 
  rename(`Degree of prior sampling` = `Prior data`,
         `Plastic FO in PICES region` = `FO in PICES`) %>% 
  select(`Scientific name`,`Common name`, 
         `Degree of prior sampling`, `Plastic FO in PICES region`, 
         Residency, `Distribution in PICES region`,  `Distribution globally`,
         `Already an indicator?`, `Threat of human exposure`, `Total bioindicator score`) %>% 
  group_by(`Scientific name`) %>% 
  summarise(`Degree of prior sampling` = first(`Degree of prior sampling`), 
            `Plastic FO in PICES region`  = first(`Plastic FO in PICES region`), 
            Residency = first(Residency), 
            `Distribution in PICES region` = first(`Distribution in PICES region`),
            `Distribution globally` = first(`Distribution globally`),
            `Already an indicator?` = first(`Already an indicator?`), 
            `Threat of human exposure` = first(`Threat of human exposure`), 
            `Total bioindicator score` = first(`Total bioindicator score`)) %>% 
  mutate(Group = "seabirds")


d_invert_rubric_ranking <- read_csv("Invert rubric ranking.csv") %>% 
  select(Species:`Total bioindicator score`) %>% 
  remove_empty("rows") %>% 
  rename(`Scientific name` = Species) %>% 
  mutate(Group = "invertebrates")


d_seaturtle_rubric_ranking <- read_csv("Sea turtle rubric ranking.csv") %>% 
  mutate(Group = "sea turtles")


d_full_rubric_eval <- bind_rows(d_fish_rubric_eval, d_seabird_rubric_eval, d_invert_rubric_ranking, d_seaturtle_rubric_ranking) %>%
  arrange(-`Total bioindicator score`)




col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" ,"#F7F7F7", "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F","#67001F"))


Rubric_scores_by_taxa <- ggplot(d_full_rubric_eval, 
                       aes(`Total bioindicator score`, fct_relevel(Group, "seabird", "fish", "invertebrate", "sea turtle"), 
                           fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.1, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  scale_x_continuous(limits = c(7,30),
    breaks=seq(8, 28, 2)) +
  # scale_fill_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"), 
  #                      name = NULL, limits = c(-0.1, 0.5)) +
  ylab("Taxa") +
  scale_fill_viridis(name = "Total bioindicator score") +
  theme_minimal(base_size = 16)
Rubric_scores_by_taxa


ggplot(repex, aes(x=salesfromtarget, fill=..x..))
+geom_histogram(binwidth=.1)



Rubric_scores_by_taxa_hist <- ggplot(d_full_rubric_eval) +
  geom_histogram(aes(x = `Total bioindicator score`, fill=..x..)) +
  geom_density(aes(x = `Total bioindicator score`)) +
  facet_grid(factor(Group, levels=c("seabird", "fish", "invertebrate", "sea turtle"))~.,
             scales = "free_y") +
  scale_x_continuous(limits = c(10,28),
                     breaks=seq(8, 28, 2)) +
  # scale_fill_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"),
  #                      name = NULL, limits = c(-0.1, 0.5)) +
  #scale_fill_viridis(name = "Total bioindicator score") +
  scale_fill_gradient(low = "green", high = "blue") +
  theme_minimal(base_size = 16)
Rubric_scores_by_taxa_hist


pal2 <- c("fishes" = "salmon2", 
         "invertebrates" = "gold2", 
         "sea turtles" = "darkolivegreen3", 
         "seabirds" = "darkslategray4"
)

Rubric_scores_by_taxa_dens <- ggplot(d_full_rubric_eval) +
  geom_density(aes(x = `Total bioindicator score`, fill = Group),
               alpha = 0.3) +
  geom_rug(aes(x = `Total bioindicator score`, y = 0,
               color = Group), 
           position = position_jitter(height = 0)) +
  #facet_grid(factor(Group, levels=c("seabird", "fish", "invertebrate", "sea turtle"))~.) +
  scale_x_continuous(limits = c(10,28),
                     breaks=seq(8, 28, 2)) +
  scale_color_manual(values = pal2) +
  scale_fill_manual(values = pal2) +
  labs(fill = "Taxonomic group") +
  theme_classic(base_size = 16)
Rubric_scores_by_taxa_dens

dev.copy2pdf(file="Rubric eval full.pdf", width=8, height=5)

# Spiderplot of top scoring species (Fig. 7)---- 

# create color pal

BI_pal <- c("M. edulis" = "navy",
            "C. gigas" = "navajowhite4",
            "V. philippinarum" = "orangered4",
            "C. caretta" = "darkorange4", 
            "C. mydas" = "darkolivegreen", 
            "A. ferox" = "gray30", 
            "C. hippurus" = "green3", 
            "Lampris spp." = "deeppink3",
            "Engraulis spp." = "lightcyan2",
            "F. glacialis" = "darkslategray4",
            "O. leucorhoa"  = "tan4",
            "Phoebastria spp." = "dodgerblue2"
)

BI_data <- read_csv("Bioindicator_finalist_scores.csv")



# Fish radar plot
BI_data_fish <- BI_data %>% 
  filter(Taxa == "fish",
         Common_name != "Pacific opah") %>% 
  select(Common_name, `prior sampling`:`human threat`) %>% 
  # mutate(Scientific_name = abbr_binom(Scientific_name),
  #        Scientific_name = case_when(Scientific_name == "E. spp." ~ "Engraulis spp.",
  #                                     TRUE ~ Scientific_name)) %>% 
  column_to_rownames(var="Common_name")

BI_data_fish <- rbind(rep(4,7) , rep(0,7) , BI_data_fish)



par(xpd = TRUE, mar = c(2, 4, 2, 4))
radarchart(BI_data_fish, 
           #custom the points and polygon
           plwd=2, plty=c(1,2,2),
           #custom the grid
           axistype = 1,
           cglcol="grey", cglty=1, axislabcol="grey", 
           pcol=c("gray30", "green3", "lightpink3"),
           caxislabels=seq(0,4,1), cglwd=0.9,
           #custom label size
           vlcex=0)


# Add a legend
legend(
  #"top",
  x = -2, y = 1.55,
  legend = rownames(BI_data_fish[-c(1,2),]), 
  text.font = 1,
  bty = "n", 
  text.width = 0.9,
  x.intersp = 0.5, y.intersp = 0.25, 
  xjust = -0.125,
  pch=20 , 
  col=c("gray30", "green3", "lightpink3"),
  horiz=TRUE,
  text.col = "black", cex=1.25, pt.cex=2)


dev.copy2pdf(file="BI_fish_radar.pdf", width=9, height=6.5)

dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()


# Sea turtle radar plot

# Spider plot for sea turtles
BI_data_ST <- BI_data %>% 
  filter(Taxa == "sea turtle") %>% 
  select(Common_name, `prior sampling`:`human threat`) %>% 
  
  # mutate(Scientific_name = abbr_binom(Scientific_name),
  #        Scientific_name =  case_when(Scientific_name == "L. sp." ~ "Lampris spp.",
  #                                     Scientific_name == "P.NA" ~ "Phoebastria spp.",
  #                                     TRUE ~ Scientific_name)) %>% 
  column_to_rownames(var="Common_name")

BI_data_ST <- rbind(rep(4,7) , rep(0,7) , BI_data_ST)

par(xpd = TRUE, mar = c(2, 4, 2, 4))
radarchart(BI_data_ST, 
           #custom the points and polygon
           plwd=2, plty=1,
           #custom the grid
           axistype = 1,
           cglcol="grey", cglty=1, axislabcol="grey", 
           pcol=c("goldenrod4", "darkolivegreen"),
           caxislabels=seq(0,4,1), cglwd=0.9,
           #custom label size
           vlcex=1)

# Add a legend
legend(
  #"top",
  x = -1.25, y = 1.55,
  legend = rownames(BI_data_ST[-c(1,2),]), 
  text.font = 1,
  bty = "n", 
  xjust = -0.025,
  x.intersp = 0.5, y.intersp = 0.25, 
  pch=20 , col=c("goldenrod4", "darkolivegreen"),
  horiz=TRUE,
  text.col = "black", cex=1.25, pt.cex=2)

dev.copy2pdf(file="BI_ST_radar.pdf", width=9, height=6.5)

dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()


# Seabird radar plot

BI_data_SB <- BI_data %>% 
  filter(Taxa == "seabird") %>% 
  select(Common_name, `prior sampling`:`human threat`) %>% 
  # mutate(Scientific_name = abbr_binom(Scientific_name),
         # Scientific_name =  case_when(Scientific_name == "L. sp." ~ "Lampris spp.",
         #                              Scientific_name == "P.NA" ~ "Phoebastria spp.",
         #                              TRUE ~ Scientific_name)) %>% 
  column_to_rownames(var="Common_name")

BI_data_SB <- rbind(rep(4,7) , rep(0,7) , BI_data_SB)



par(xpd = TRUE, mar = c(2, 4, 2, 4))
radarchart(BI_data_SB, 
           #custom the points and polygon
           plwd=2, plty=c(2,1,2),
           #custom the grid
           axistype = 1,
           cglcol="grey", cglty=1, axislabcol="grey", 
           pcol=c("darkslategray4", "tan4","dodgerblue2"),
           caxislabels=seq(0,4,1), cglwd=0.9,
           #custom label size
           vlcex=1)

# Add a legend
legend(
  #"top",
  x = -2, y = 1.55,
  legend = rownames(BI_data_SB[-c(1,2),]), 
  text.font = 1.2,
  bty = "n", 
  text.width = 0.9,
  x.intersp = 0.5, y.intersp = 0.25, 
  xjust = -0.125,
  pch=20, col=c("darkslategray4", "tan4","dodgerblue2"),
  horiz=TRUE,
  text.col = "black", cex=1.25, pt.cex=2)

dev.copy2pdf(file="BI_SB_radar.pdf", width=9, height=6.5)

dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()



# Bivalve radar plot

BI_data_Bv <- BI_data %>% 
  filter(Taxa == "invertebrate") %>% 
  select(Common_name, `prior sampling`:`human threat`) %>% 
 # mutate(Scientific_name = abbr_binom(Scientific_name))%>% 
  column_to_rownames(var="Common_name")

BI_data_Bv <- rbind(rep(4,7) , rep(0,7) , BI_data_Bv)



"M. edulis" = "navy",
"C. gigas" = "navajowhite4",
"V. philippinarum" = "orangered4",

par(xpd = TRUE, mar = c(2, 4, 2, 4))
radarchart(BI_data_Bv, 
           #custom the points and polygon
           plwd=2, plty=c(1,1,2),
           #custom the grid
           axistype = 1,
           cglcol="grey", cglty=1, axislabcol="grey", 
           pcol=c("mediumblue", "goldenrod3", "orangered4"),
           caxislabels=seq(0,4,1), cglwd=0.9,
           #custom label size
           vlcex=1)

# Add a legend
legend(
  #"top",
  x = -1.5, y = 1.55,
  legend = rownames(BI_data_Bv[-c(1,2),]), 
  text.font = 1,
  bty = "n", 
  x.intersp = 0.5, y.intersp = 0.25, 
  pch=20 , col=c("mediumblue", "goldenrod3", "orangered4"),
  horiz=TRUE,
  text.col = "black", cex=1.25, pt.cex=2)

dev.copy2pdf(file="BI_Bv_radar.pdf", width=9, height=6.5)

dev.off(dev.list()["RStudioGD"]) # Apply dev.off() & dev.list()






