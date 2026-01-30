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
  read_csv(paste0(home_dir, "\\point_half_mi_blockgroup_AF.csv")),
  read_csv(paste0(home_dir, "\\point_quarter_mi_blockgroup_AF.csv")),
  read_csv(paste0(home_dir, "\\lines_half_mi_blockgroup_AF.csv"))
) %>% 
  filter(!is.na(GEOID))

tract_af <- rbind(
  read_csv(paste0(home_dir, "\\point_half_mi_tract_AF.csv")),
  read_csv(paste0(home_dir, "\\point_quarter_mi_tract_AF.csv")),
  read_csv(paste0(home_dir, "\\lines_half_mi_tract_AF.csv"))
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


output_rescore <- get_demogs(bg_af = read_csv(paste0(home_dir, "\\rescore_projects_blockgroup_AF.csv")), 
                             tract_af = read_csv(paste0(home_dir, "\\rescore_projects_tract_AF.csv")), 
                             bg_census = bg_census, 
                             tract_census = tract_census)
write_csv(output_rescore, "J:\\Shared drives\\Projects\\Transportation Equity\\TIP\\FFY2027-31 TIP Development\\Project Scoring\\demographics\\rescore_projects.csv")

output_new <- get_demogs(bg_af = bg_af, 
                             tract_af = tract_af, 
                             bg_census = bg_census, 
                             tract_census = tract_census)



