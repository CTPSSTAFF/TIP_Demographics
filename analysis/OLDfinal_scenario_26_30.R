library(tidyverse)

# We need to generate our new PTI metrics for the 26-30 TIP. To do so, we'll need
# the area fractions for all these projects. There are a few issues that need to be 
# resolved:
# 
# 1. We need area fractions for projects that were scored and programmed last year
# 2. Last year, we used CTPS IDs instead of PROJIS, so we'll need to crosswalk
#    those two IDs.
# 3. A number of projects scored for the 26-30 TIP changed their PROJIS ID since
#    scoring was completed, so we'll need to assign them their new IDs.


# STEP 1: Get area fractions from last year
#
results_dir_25_29 <- "C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\022624_final"

# input_blkgrps_table <- read_csv(paste0(results_dir, "argilla_road_612738_bg_af.csv"))
# input_tracts_table <- read_csv(paste0(results_dir, "argilla_road_612738_tract_af.csv"))

input_blkgrps_table <- do.call("rbind", list(
  read_csv(paste0(results_dir_25_29, "\\lines_af_bg_eighth_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\lines_af_bg_quart_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\lines_af_bg_half_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\polygons_af_bg_022624.csv"))
))

input_tracts_table <- do.call("rbind", list(
  read_csv(paste0(results_dir_25_29, "\\lines_af_tract_eighth_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\lines_af_tract_quart_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\lines_af_tract_half_mi_022624.csv")),
  read_csv(paste0(results_dir_25_29, "\\polygons_af_tract_022624.csv"))
))

af_25_29 <- rbind(input_blkgrps_table, input_tracts_table)


# STEP 2: Get PROJIS IDs for last year's projects
ctps_projis_link <- read_csv("data/CTPS_PROJIS_linking_table.csv") %>% 
  group_by(CTPS_ID) %>% 
  summarise(PROJIS = max(TIP_ID)) %>%  
  filter(PROJIS > 10000)
af_25_29_projis <- af_25_29 %>% 
  left_join(ctps_projis_link, by = join_by("CTPS_ID_text" == "CTPS_ID"))

# STEP 3: Get new PROJIS IDs for this year's projects

