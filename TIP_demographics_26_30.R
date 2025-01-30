#
# Calculate demographics for TIP project scoring
#
#    1. Join demographics to TIP project area fractions
#    2. Apply area fractions to census data
#    3. Sum by project
#    4. Calculate percentages
#


library(tidyverse)

# Load data
home_dir <- "C:\\Users\\sstrumwasser\\Documents\\ArcGIS\\Projects\\TIP_Demographics\\data"
bg_af <- read_csv(paste0(home_dir, "\\out_bgs.csv"))
tracts_af <- read_csv(paste0(home_dir, "\\out_tracts.csv"))
bg_demos <- read_csv(paste0(home_dir, "\\bg_demos.csv"))
tract_demos <- read_csv(paste0(home_dir, "\\tract_demos.csv"))

bg_joined <- bg_af %>% 
  left_join(bg_demos, by=join_by("geoid" == "GEOID"))
tracts_joined <- tracts_af %>% 
  left_join(tract_demos, by=join_by("geoid" == "GEOID"))


# Apply area fraction ----

# Block Groups ----
bg_pops <- bg_joined %>% 
  mutate(
    minority_universe_af =        round(total_pop_min * area_fraction),
    minority_pop_af =             round(minority_pop * area_fraction),
    
    older_adults_universe_af =    round(total_pop_eld * area_fraction),
    older_adults_pop_af =         round(elderly_pop * area_fraction),
    
    youth_universe_af =           round(total_pop_yth * area_fraction),
    youth_pop_af =                round(youth_pop * area_fraction),
    
    lowincome_universe_af =       round(total_pop_inc * area_fraction),
    lowincome_pop_af =            round(lowincome_pop * area_fraction),
    
    lep_universe_af =             round(total_pop_lep * area_fraction),
    lep_pop_af =                  round(lep_pop * area_fraction)
  ) %>% 
  select(
    PROJIS,
    geoid,
    
    minority_universe_af,
    minority_pop_af,

    older_adults_universe_af,
    older_adults_pop_af,

    youth_universe_af,
    youth_pop_af,

    lowincome_universe_af,
    lowincome_pop_af,

    lep_universe_af,
    lep_pop_af,
  )


# Tracts ----
tract_pops <- tracts_joined %>% 
  mutate(
    disabled_universe_af = round(total_pop_disability * area_fraction),
    disabled_pop_af = round(disability_pop * area_fraction),
    
    zero_vhh_universe_af = round(total_hh * area_fraction),
    zero_vhh_af = round(zero_veh_hh + area_fraction)
  ) %>% 
  select(
    PROJIS,
    geoid,
    
    disabled_universe_af,
    disabled_pop_af,
    
    zero_vhh_universe_af,
    zero_vhh_af
  )


# Sum by project
bg_project_sums <- bg_pops %>% 
  group_by(PROJIS) %>% 
  summarise(
    minority_universe = sum(minority_universe_af),
    minority_pop = sum(minority_pop_af),

    older_adults_universe = sum(older_adults_universe_af),
    older_adults_pop = sum(older_adults_pop_af),

    youth_universe = sum(youth_universe_af),
    youth_pop = sum(youth_pop_af),

    lowincome_universe = sum(lowincome_universe_af),
    lowincome_pop = sum(lowincome_pop_af),

    lep_universe = sum(lep_universe_af),
    lep_pop = sum(lep_pop_af)
  )

tract_project_sums <- tract_pops %>% 
  group_by(PROJIS) %>% 
  summarise(
    disabled_universe = sum(disabled_universe_af),
    disabled_pop = sum(disabled_pop_af),
    
    zero_vhh_universe = sum(zero_vhh_universe_af),
    zero_vhh = sum(zero_vhh_af)
  )

# Combine tract and bg tables
all_project_sums <- merge(bg_project_sums, tract_project_sums, by="PROJIS")

# Calculate percentages
out_table <- all_project_sums %>% 
  mutate(
    minority_pct =     round(minority_pop / minority_universe, 2),
    older_adults_pct = round(older_adults_pop / older_adults_universe, 2),
    youth_pct =        round(youth_pop / youth_universe, 2),
    lowincome_pct =    round(lowincome_pop / lowincome_universe, 2),
    lep_pct =          round(lep_pop / lep_universe, 2),
    disabled_pct =     round(disabled_pop / disabled_universe, 2),
    zero_vhh_pct =     round(zero_vhh / zero_vhh_universe, 2),
  ) %>% 
  select(
    PROJIS,
    minority_universe,
    minority_pop,
    minority_pct,
    older_adults_universe,
    older_adults_pop,
    older_adults_pct,
    youth_universe,
    youth_pop,
    youth_pct,
    lowincome_universe,
    lowincome_pop,
    lowincome_pct,
    lep_universe,
    lep_pop,
    lep_pct,
    disabled_universe,
    disabled_pop,
    disabled_pct,
    zero_vhh_universe,
    zero_vhh,
    zero_vhh_pct
  )

# Write out results
write_csv(out_table, paste0(home_dir, "\\results_test.csv"))
