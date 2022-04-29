# Get the total census population and number of households
# Download the total HH and the total pop for each census tract.
# Multiply these values against the shares in each tract.
# Aggregate to the TIP project level. 

library(tidyverse)
library(tidycensus)

cen_vars <- load_variables(year = 2020, dataset = "pl")

cen_labs <- cen_vars %>% filter(name %in% c("H1_002N", "P1_001N", "P3_001N"))

# If we need "occupied housing" add H1_002N.
MA_blkgrp <- get_decennial(geography = "block group",
                          state = "MA",
                          variables = c("H1_002N", "P1_001N", "P3_001N"),
                          year = 2020)

# Read in our TIP project intersections. This is based on 2020 census blocks (!!).
# The CSV was being persnickety always turning things into scientific notation. 
# I added "!!" at the end of the number in the csv then strip it away. 
# This has a side effect of leaving all of the values as `chr` which makes 
# the join easier to do.

tip_proj_areas <- read_csv("./data/tip_bg_cen20.csv")

tip_proj_areas <- tip_proj_areas %>% 
  mutate(geoid = as.character(geoid20)) %>% 
  select(geoid, tip_id, area_fraction) 

# Join the two datasets. This should be X times as big as the original dataset where
# X is the number of variables.
# because we have two tables). 
tip_proj_areas_jn <- left_join(tip_proj_areas, MA_blkgrp, by = c("geoid" = "GEOID"))

# Add up the populations. 
tip_proj_areas_jn <- tip_proj_areas_jn %>% 
  mutate(value_prop = area_fraction * value) %>% 
  group_by(tip_id, variable) %>% 
  summarize(tot_val = sum(value_prop, na.rm = TRUE)) 

# Make it wider if that's useful
tip_proj_areas_wd <- tip_proj_areas_jn %>% 
  pivot_wider(names_from = variable,
              values_from = tot_val)

write_csv(tip_proj_areas_wd, "./output/tip_proj_hh_pop_cen20.csv")

