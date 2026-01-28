#
# Calculate demographics for TIP project scoring
#
#    1. Join demographics to TIP project area fractions
#    2. Apply area fractions to census data
#    3. Sum by project
#    4. Calculate percentages
#


library(tidyverse)

source("analysis\\demographics.R")

# Load data
home_dir <- "data\\27_31\\outputs"

bg_af <- rbind(
  read_csv(paste0(home_dir, "\\rescore_projects_blockgroup_AF.csv"))
) %>% 
  filter(!is.na(GEOID))

tract_af <- rbind(
  read_csv(paste0(home_dir, "\\rescore_projects_tract_AF.csv"))
) %>% 
  filter(!is.na(GEOID))

# Demographics - these can be pulled in from the cloned repo or downloaded here:
# https://github.com/CTPSSTAFF/brmpo-demographics/tree/main/runs/run-2019_2023_acs/output
brmpo_demo_acs_23 <- "../brmpo-demographics/runs/run-2019_2023_acs/output"
bg_census <- read_csv(paste0(brmpo_demo_acs_23, "/brmpo_demographics_blockgroup.csv"))
tract_census <- read_csv(paste0(brmpo_demo_acs_23, "/brmpo_demographics_tract.csv"))


# Not sure what this is for so keeping it around
#
# bg_demos <- read_csv(paste0(home_dir, "\\bg_demos.csv")) %>% 
#   mutate_if(is.numeric, replace_na, 0) %>%  
#   mutate_if(is.character, replace_na, "")  # replace NAs with 0s
# tract_demos <- read_csv(paste0(home_dir, "\\tract_demos.csv")) %>% 
#   mutate_if(is.numeric, replace_na, 0) %>%  
#   mutate_if(is.character, replace_na, "")  # replace NAs with 0s


get_demogs(bg_af = bg_af, 
           tract_af = tract_af, 
           bg_census = bg_census, 
           tract_census = tract_census)



