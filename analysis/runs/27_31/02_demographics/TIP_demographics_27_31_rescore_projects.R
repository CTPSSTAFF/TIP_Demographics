#
# Calculate demographics for projects being rescored for the 27-31 TIP
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

bg_af <-    read_csv(paste0(home_dir, "\\rescore_projects_blockgroup_AF.csv"), show_col_types = FALSE) %>% 
  filter(!is.na(GEOID))
tract_af <- read_csv(paste0(home_dir, "\\rescore_projects_tract_AF.csv"), show_col_types = FALSE) %>% 
  filter(!is.na(GEOID))


# Demographics - these can be pulled in from the cloned repo or downloaded here:
# https://github.com/CTPSSTAFF/brmpo-demographics/tree/main/runs/run-2019_2023_acs/output
# 2019-23 5-year ACS used for non-decennial demographics. Details here: 
# https://docs.google.com/document/d/1a_wYad6VxoEKoyMz1iszl_gNAKk-_d7HM0Pkhh6q7SE/edit?tab=t.0#heading=h.m5e4gcqcdvn9
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


# output_rescore <- get_demogs(bg_af = read_csv(paste0(home_dir, "\\rescore_projects_blockgroup_AF.csv")), 
#                              tract_af = read_csv(paste0(home_dir, "\\rescore_projects_tract_AF.csv")), 
#                              bg_census = bg_census, 
#                              tract_census = tract_census)
# write_csv(output_rescore, "J:\\Shared drives\\Projects\\Transportation Equity\\TIP\\FFY2027-31 TIP Development\\Project Scoring\\demographics\\rescore_projects.csv")

output_rescore <- get_demogs(bg_af = bg_af, 
                             tract_af = tract_af, 
                             bg_census = bg_census, 
                             tract_census = tract_census)

# Join output to buffer distances
out_with_buffers <- output_rescore %>% 
  left_join(read_csv("data/27_31/inputs/Project buffers_rescore.csv"), by="PROJIS")

write_csv(out_with_buffers, "J:\\Shared drives\\Projects\\Transportation Equity\\TIP\\FFY2027-31 TIP Development\\Project Scoring\\demographics\\rescore_projects.csv")
write_csv(out_with_buffers, paste0(home_dir, '/demographics/rescore_projects.csv'))




