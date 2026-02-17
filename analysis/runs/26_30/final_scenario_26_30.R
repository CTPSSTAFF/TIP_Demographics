library(tidyverse)

source("./analysis/new_demogs_26_30.R")

# Rather than try to collect old area fractions and worry about crosswalking 
# CTPS and PROJIS IDs, I decided to just run the full set of final scenario
# projects through the network buffer analysis again to generate area fractions
# using the correct PROJIS IDs. And we don't actually need area fractions, just the
# list of GEOIDs of tracts that the are in each project's buffer.

home_dir <- "C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Projects\\TIP_Demographics\\results\\26_30\\final_scenario"

af <- rbind(
  read_csv(paste0(home_dir, "\\eighth_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\quarter_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\S13292_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\S13292_half_mi_tract.csv")) %>% 
    mutate(PROJIS = "S13152"),
  
  # Removing mbta bus priority project for PTI calculation as it covers a large portion of the region
  # read_csv(paste0(home_dir, "\\mbta_bus_quart_mi_tract.csv")) %>% 
  #   mutate(PROJIS = "S13153"), # update to new PROJIS - doing it here for documentation
  
  read_csv(paste0(home_dir, "\\polygons_tract.csv"))
) %>% 
  filter(!is.na(geoid)) %>% 
  mutate(geoid = as.character(geoid))

# information about groups in each tract
demogs <- st_read("J:\\Shared drives\\MPO_Activities\\Transportation Equity\\Equity Data\\ArcGIS\\Demographic Spatial Files\\TE_BRMPO_tracts_massgis_2023.shp")

# project_info - used to get the investment program for each project
project_info <- read_csv("./data/final_scenario.csv")

# check that all the projects are included
# af_projects <- af %>% 
#   group_by(PROJIS) %>% 
#   summarise()
# projects <- read_csv("C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Projects\\TIP_Demographics\\final_scenario_projects.csv")
# check <- projects %>% 
#   select(PROJIS) %>% 
#   left_join(af_projects, by = "PROJIS", keep = T)

results <- calc_pti_table(af, project_info, demogs)
by_program <- results[[1]]
all_projects <- results[[2]]

write_csv(by_program, "J:\\Shared drives\\MPO_Activities\\Transportation Equity\\TIP\\FFY2026-30 TIP Development\\Project Scoring\\PTI_by_investment_program_no_overlapping_no_mbta.csv")
write_csv(all_projects, "J:\\Shared drives\\MPO_Activities\\Transportation Equity\\TIP\\FFY2026-30 TIP Development\\Project Scoring\\PTI_all_projects.csv")

