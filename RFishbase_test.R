### Diddling with Package: rfishbase

#remotes::install_github("ropensci/rfishbase") #try this line if regular rfishbase doesnt work
library(rfishbase)
library(rredlist)
library(sf)
library(rgdal) 
library(tidyverse)


longhurst <- sf::read_sf("~/Documents/Research Data/PICES bioindicators of plastic ingestion/longhurst_v4_2010/Longhurst_world_v4_2010.shp")
names(longhurst)



# World map
library(rnaturalearth)
library(rgeos)
world_map <- rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf"))


# Base map
kk <- ggplot() +
  geom_sf(data = world_map, size = .2, fill = "gray80", col = "gray90") +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))
# simplify the object to make it 'usable'
longhurst <- longhurst %>% 
  sf::st_simplify(dTolerance = 0.01) %>% 
  dplyr::group_by(ProvCode,ProvDescr) %>% 
  dplyr::summarise()
# plot(longhurst)

# plot
kk+  
  geom_sf(data = longhurst, aes(fill = ProvCode), size = .2, col = "grey50", alpha=.4)+
  ggtitle(paste("Longhurst Biogeochemical Provinces -", length(unique(longhurst$ProvCode)),"provinces"))+
  theme(legend.position="none")+
  geom_sf_text(data = longhurst %>% group_by(ProvDescr) %>% summarize(n()), aes(label = ProvDescr), colour = "grey20", check_overlap=TRUE)+
  coord_sf(expand = FALSE)


Longhurst_Provs <- readOGR("~/Documents/Research Data/PICES bioindicators of plastic ingestion/longhurst_v4_2010/",
                           "Longhurst_world_v4_2010")


Longhurst_Provs_df <- fortify(Longhurst_Provs)

str(Longhurst_Provs_df)


needed_fish <- validate_names(c("Cheilinus trilobatus", 
                                "Cephalopholis urodelus",
                                "Calotomus carolinus",
                                "Lutjanus gibbus", 
                                "Cetoscarus bicolor", 
                                "Chaetodon lunula", 
                                "Pomacanthus semicirculatus", 
                                "Dentex tumifrons",
                                "Chaetodon auriga", 
                                "Myripristis kuntee", 
                                "Chaetodon auripes", 
                                "Chaetodon trifascialis", 
                                "Naso lituratus", 
                                "Hemigymnus melapterus", 
                                "Epinephelus hexagonatue",
                                "Priacanthus sagittarius", 
                                "Acanthurus triostegus", 
                                "Naso unicornis", 
                                "Neoniphon sammara", 
                                "Cephalopholis argus", 
                                "Scarus quoyi", 
                                "Ctenochaetus binotatus", 
                                "Kyphosus lembus", 
                                "Gnathodentex aureolineatus", 
                                "Scarus forsteni", 
                                "Cypselurus unicolor"))
                              


needed_species <- species(needed_fish)

needed_species_ecology <- ecology(needed_fish)



# Add IUCN API key from email ----

IUCN_REDLIST_KEY <- "b39f17883d8f37ae1b87a2022732bc25ebdfd2f0afaa2b0ea15286b511ab02a8"

needed_species_IUCN <-  rl_search_(key  = IUCN_REDLIST_KEY, 
                                   name = "Cheilinus trilobatus")

needed_species_info <- select(needed_species, 
                              Species, FBname, 
                              DemersPelag, 
                              DepthRangeShallow, DepthRangeDeep,
                              Vulnerability, 
                              Importance, UsedforAquaculture, GameFish)
