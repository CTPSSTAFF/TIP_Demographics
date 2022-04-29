library(tidyverse)

# Join the demographic percentages to the hh/pop values. 
# Let's start fresh and grab the two fields.
# 
tip_pcts <- read_csv("./output/tip_demographics_2019ACS_2020Cen.csv")
tip_vals <- read_csv("./output/tip_proj_hh_pop_2020Cen.csv")

tip_join <- full_join(tip_pcts, tip_vals, by = "tip_id")

tip_join <- tip_join %>% 
  rename(min_perc = MinPerc) %>% 
  mutate(min_num = min_perc *               P1_001N, # Total Population
         lt_2x_pov_num = lt_2x_pov_perc *   P1_001N, # Total Population
         hh_0veh_num = hh_0veh_perc *       H1_002N, # Total Households
         low_inc_hh_num = low_inc_hh_perc * H1_002N, # Total Households
         lep_num = lep_perc *               P1_001N, # Total Population
         u18_num = u18_perc *               P1_001N, # Total Population
         p75_num = p75_perc *               P1_001N, # Total Population
         disab_num = disab_perc *           P1_001N, # Total Population
         workers16p_num = workers16p_perc * P3_001N  # Population 18+
         )

write_csv(tip_join, "./output/tip_demog_pct_num_2019ACS_2020Cen.csv")
