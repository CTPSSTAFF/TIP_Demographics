library(tidyverse)

# Join the demographic percentages to the hh/pop values. 
# Let's start fresh and grab the two fields.
# 
TIP_pcts <- read_csv("./output/tip_demographics_2019ACS_2020Cen.csv")
TIP_vals <- read_csv("./output/tip_proj_hh_pop_cen20.csv")

TIP_join <- full_join(TIP_pcts, TIP_vals)

TIP_join <- TIP_join %>% 
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
