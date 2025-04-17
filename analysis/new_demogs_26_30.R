library(tidyverse)
library(sf)

# This function generates 4 new columns for each project:
# 1. tracts served
# 2. population of tracts served
# 3. percent of tracts served in each category (0 groups, 1 to 2, so on)
# 4. percent of population of tracts served that is in a tract in each category

# testing
tracts_af <- af
tracts_demog <- pti
project_info <- read_csv("./data/final_scenario.csv")

#' Calculate PTI table
#'
#' Generate the breakdown of tracts with 0, 1, 2 etc. populations of interest that
#' are impacted by projects in each investment program.
#' @param tracts_af A data frame of project IDs, tract GEOIDs, and area fractions
#' @param project_info A data frame with investment program column for each project ID
#' @param tracts_demog Demographics for each census tract
#'
#' @return
#' @export
#'
#' @examples
calc_pti_table <- function(tracts_af, project_info, tracts_demog) {
  
  # Join investment program information to af tables
  tracts_af_with_types <- tracts_af %>% 
    left_join(project_info, by = join_by("PROJIS" == "Project ID")) %>% 
    select(PROJIS, geoid, area_fraction, project_type = `Investment Category`)
  
  # Join demographics to af tables
  tracts_joined <- tracts_af_with_types %>% 
    left_join(tracts_demog %>% 
                select(
                  GEOID,
                  total_pop_,
                  poc_exceed, 
                  lowinc_exceed = lowincom_3,
                  yout_exceed = youth_exce,
                  olderadult_exceed = olderadu_2,
                  lep_exceed,
                  disability_exceed = disabili_4,
                  tot_exceed
                ), 
              by = join_by("geoid" == "GEOID"))
  
  # number of served tracts and population by investment program and category
  by_category <- tracts_joined %>% 
    group_by(project_type, tot_exceed) %>% 
    summarize(
      tracts = n(),
      pop = sum(total_pop_)
    )
  
  # total tracts served and total pop for each category-program combo
  total <- tracts_joined %>% 
    group_by(project_type) %>% 
    summarize(
      total_tracts = n(),
      total_pop = sum(total_pop_)
    )
  
  # join them together to get percent of tracts
  by_investment_prg <- by_category %>% left_join(
    total,
    by = "project_type"
  ) %>% 
    mutate(
      perc_tracts = round(tracts / total_tracts, 3) * 100,
      perc_pop = round(pop / total_pop, 3) * 100
    )
  
  # get stats for each category - across all projects
  all_projects <- joined %>% 
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
    ) %>% 
    select(-total_tracts_denom, -total_pop_denom)
  
  # add dummy rows
  all_combos <- data.frame(
    project_type = c(
      rep("BP", 7), 
      rep("CC", 7), 
      rep("CS", 7), 
      rep("MI", 7), 
      rep("II", 7), 
      rep("TT", 7) 
      ),
    tot_exceed = rep(c(0,1,2,3,4,5,6), 6)
  )
  
  final <- all_combos %>% 
    left_join(by_investment_prg, by = c("project_type", "tot_exceed"))
  
  final[is.na(final)] = 0
  
  return(final)
}


