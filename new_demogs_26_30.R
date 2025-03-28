library(tidyverse)
library(sf)

# This script generates _ new columns for each project:
# 1. tracts served
# 2. population of tracts served
# 3. percent of tracts served in each category (0 groups, 1 to 2, so on)
# 4. percent of population of tracts served that is in a tract in each category

bg_pti <- st_read("C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Demographic Spatial Files\\TE_BRMPO_blkgps_massgis_2023.shp")
tracts_pti <- st_read("C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Demographic Spatial Files\\TE_BRMPO_tracts_massgis_2023.shp")

home_dir <- "C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Projects\\TIP_Demographics\\results\\26_30"

bg_af <- rbind(
  read_csv(paste0(home_dir, "\\lines_eighth_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\lines_quart_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\mbta_bus_quart_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\lines_half_mi_bg.csv")) %>% filter(PROJIS != 'S13155'),
  read_csv(paste0(home_dir, "\\added_half_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\S12977_half_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\S13155_half_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\S13200_half_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\added_022025_half_mi_bg.csv")),
  read_csv(paste0(home_dir, "\\polygon_bg.csv"))
) %>% 
  filter(!is.na(geoid))

tracts_af <- rbind(
  read_csv(paste0(home_dir, "\\lines_eighth_mi_tracts.csv")),
  read_csv(paste0(home_dir, "\\lines_quart_mi_tracts.csv")),
  read_csv(paste0(home_dir, "\\mbta_bus_quart_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\lines_half_mi_tracts.csv")) %>% filter(PROJIS != 'S13155'),
  read_csv(paste0(home_dir, "\\added_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\S12977_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\S13155_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\S13200_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\added_022025_half_mi_tract.csv")),
  read_csv(paste0(home_dir, "\\polygon_tracts.csv"))
) %>% 
  filter(!is.na(geoid)) %>% 
  mutate(geoid = as.character(geoid))


# Join PTI to af tables
tracts_joined <- tracts_af %>% 
  left_join(tracts_pti, by = join_by("geoid" == "GEOID")) %>% 
  select(PROJIS, 
         geoid, 
         total_pop_,
         poc_exceed, 
         lowinc_exceed = lowincom_3,
         yout_exceed = youth_exce,
         olderadult_exceed = olderadu_2,
         lep_exceed,
         disability_exceed = disabili_4,
         tot_exceed)

# number of served tracts and population by project and category
by_category <- tracts_joined %>% 
  group_by(PROJIS, tot_exceed) %>% 
  summarize(
    tracts = n(),
    pop = sum(total_pop_)
    )

# total tracts served and total pop for each project
total <- tracts_joined %>% 
  group_by(PROJIS) %>% 
  summarize(
    total_tracts = n(),
    total_pop = sum(total_pop_)
  )

# join them together to get percent of tracts and population
joined <- by_category %>% left_join(
  total,
  by = "PROJIS"
) %>% 
  mutate(
    perc_tracts = tracts / total_tracts,
    perc_pop = pop / total_pop
  )

# get stats for each category - across all projects - this is the table that will go in the TIP
final <- joined %>% 
  group_by(tot_exceed) %>% 
  summarize(
    total_tracts = sum(tracts),
    total_pop = sum(pop)
    ) %>% 
  mutate(
    total_tracts_denom = sum(total_tracts),
    total_pop_denom = sum(total_pop),
    perc_tracts = round(total_tracts / total_tracts_denom, 3) * 100,
    perc_pop = round(total_pop / total_pop_denom, 3) * 100
  )


