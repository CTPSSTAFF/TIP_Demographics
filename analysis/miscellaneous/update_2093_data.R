library(tidyverse)

final_dir <- "C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\022624_final\\"

half_mi_bg_full <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\022624_final\\lines_af_bg_half_mi_022624.csv")
half_mi_tract_full <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\022624_final\\lines_af_tract_half_mi_022624.csv")

updated_2093_bg <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\update_to_2093_bg.csv")
updated_2093_tract <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\TIP_25_29\\results\\update_to_2093_tract.csv")

# delete old records
half_mi_bg_full_2 <- half_mi_bg_full %>% 
  filter(CTPS_ID_text != 2093)
half_mi_tract_full_2 <- half_mi_tract_full %>% 
  filter(CTPS_ID_text != 2093)

# add new records
half_mi_bg_new <- rbind(half_mi_bg_full_2, updated_2093_bg)
half_mi_tract_new <- rbind(half_mi_tract_full_2, updated_2093_tract)

# write new csvs
write_csv(half_mi_bg_new, paste0(final_dir, "lines_af_bg_half_mi_022624.csv"))
write_csv(half_mi_tract_new, paste0(final_dir, "lines_af_tract_half_mi_022624.csv"))
