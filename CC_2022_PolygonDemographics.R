#January 12, 2021
#Author: Margaret Atkinson
#Purpose: The intention of this script is to create a table of census and ACS demographic information for each Community Connections project area.
# Hopefully this can be used in the future to replace Access databases because those are challenging for Margaret.

#install.packages("tidycensus")
#install.packages("tidyverse")
library(dplyr)
library(tidycensus)
library(tidyverse)
library(stringr)
library(hash)

std_err_proportion <- function(output, numerator, denominator, std_err_numerator, std_err_denominator){
  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)
  std_err_denominator <- as.numeric(std_err_denominator)
  std_err_numerator <- as.numeric(std_err_numerator)
  if(denominator == 0){
    result <- 0
  }else{
    radicand <- (std_err_numerator^2) - ((numerator / denominator)^2) * (std_err_denominator^2)
    if(radicand < 0){
      radicand <- (std_err_numerator^2) + ((numerator / denominator)^2) * (std_err_denominator^2)
    }
    if(numerator == denominator){
      result <- round(std_err_numerator / denominator, 5)
    } else if(radicand == 0) {
      result <- 0
    } else{
      result <- round((radicand ^0.5) / denominator, 5)
    }
    
  }
}

census_api_key("e4bec76221ba04c7df76c7c580659bf1f54ed2c1", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")
#CACHE
get_decennial(geography = 'county subdivision', variables = "P001001", year = 2010, sumfile = "sf1", state = "MA", cache_table = TRUE)

setwd("G:/Data_Resources/tip stuff/TIP_scoring/ffy2022/CC")

#USER INPUT#####
townNames <- c("Everett", "Littleton", "Boxborough", "Bolton", "Stow", "Salem")
projectNames <- c("Everett_TMA", "MART_Micro", "Salem_Skipper")
namesList <- list("Everett_TMA" = c("Everett"), "MART_Micro" = c("Littleton", "Boxborough", "Bolton", "Stow"), "Salem_Skipper" = c("Salem"))
geoids <- c("2501721990", "2501735950", "2501707350", "2502706365", "2501768050", "2500959105")
#2/27/21 run
townNames <- c("Newton")
projectNames <- c("NewtonCC")
geoids <- c("2501745560")
namesList <- list("Newton"= c("NewtonCC"))
#3/4/21 run
townNames <- c("Sharon")
projectNames <- c("SharonCC")
geoids <- c("2502160785") 
namesList <- list("Sharon"= c("SharonCC"))
geotype = 'county subdivision'

#TRY:
#input a table and derive townNames, projectNames, geoids, namesList, areaFraction, area (sq meters), and projID (start BigTable from this)
input <- read_csv("CC_2125_Partial_Input.csv",
                  col_types = cols(
                    OBJECTID = col_integer(),
                    TIP_ID =  col_character(),      
                    geoid = col_character(),        
                    Area_Fraction = col_double()
                  )) %>% select(OBJECTID, TIP_ID, geoid, Area_Fraction)
geoids <- c(input$geoid)
projectNames <- c(unique(input$TIP_ID))
geotype = 'block'

#Grab all the demographic tables that contribute to this:

####GET CENSUS ####
#Census 2010 Fields
TotalPop <- c("P001001")
Pop75p <- c("P012023", "P012024","P012025", "P012047", "P012048", "P012049")
MinPop <- c("P005001","P005003")
Pop18u <- c("P012027", "P012028", "P012029", "P012030", "P012003", "P012004", "P012005", "P012006")
Pop5p <- c("P012001", "P012003", "P012027")
TotalHH <- c("P018001")
NonInstPop <- c("P016001", "P042001", "P042002","P042008", "P042009")
Pop16p <- c("P012001", "P012003", "P012004", "P012005","P012006","P012027", "P012028", "P012029", "P014018", "P014039")
UnrelU15Pop <- c("P032021","P032028")

hash()
dict <- hash()
censusFields <- list(TotalPop, Pop75p, MinPop, Pop18u, Pop5p, TotalHH, NonInstPop, Pop16p,UnrelU15Pop)
censusNames <- c("TotalPop", "Pop75p", "MinPop", "Pop18u", "Pop5p", "TotalHH", "NonInstPop", "Pop16p","UnrelU15Pop")
dict_cenF <- hash(censusNames, censusFields)

dict_cenTab <- hash()
for (x in keys(dict_cenF)){
  intermed <- get_decennial(geography = geotype, variables = dict_cenF[[x]], year = 2010, sumfile = "sf1", state = "MA") %>% 
    pivot_wider(names_from = variable, values_from = value) %>% filter(GEOID %in% geoids)
  .set(dict_cenTab, paste0(x,"_Table"),intermed)
}

####GET ACS####
#ACS 2014 Fields
acs_noninst_pop <- c("b18101_001e")
acs_sr_noninst_pop <- c("b18101_015e","b18101_018e","b18101_034e","b18101_037e") 
acs_disabled_pop <- c("b18101_004e","b18101_007e","b18101_010e","b18101_013e","b18101_016e","b18101_019e",
                      "b18101_023e","b18101_026e","b18101_029e","b18101_032e","b18101_035e","b18101_038e")
acs_sr_disab_pop <- c("b18101_016e","b18101_019e","b18101_035e","b18101_038e")
acs_hh <- c("b25044_001e")
acs_0veh_hh <- c("b25044_003e","b25044_010e")
acs_low_inc_hh <- c("b19001_001e","b19001_002e","b19001_003e","b19001_004e","b19001_005e","b19001_006e","b19001_007e","b19001_008e","b19001_009e","b19001_010e")
acs_pop_5p <- c("b16004_001e")
acs_lep_pop <- c("b16004_001e","b16004_003e","b16004_005e","b16004_010e","b16004_015e","b16004_020e","b16004_025e","b16004_027e","b16004_032e","b16004_037e",
                 "b16004_042e","b16004_047e","b16004_049e","b16004_054e","b16004_059e","b16004_064e")
pov_stat_det_est <- c("c17002_001e") 
pov_stat_det_moe <- c("c17002_001m")
lt_2x_pov_est <- c("c17002_001e","c17002_008e")
lt_2x_pov_moe <- c("c17002_001m","c17002_008m")
acs_pop16p <- c("b23001_001e")
acs_workers16p <- c("b08301_001e")  
acs_workers16p_shr <- c("b23001_001e","b08301_001e")

dict_acsF <- hash()
acsFields <- list(acs_noninst_pop, acs_sr_noninst_pop, acs_disabled_pop, acs_sr_disab_pop, acs_hh, acs_0veh_hh, acs_low_inc_hh, acs_pop_5p, acs_lep_pop,
               pov_stat_det_est, pov_stat_det_moe, lt_2x_pov_est, lt_2x_pov_moe, acs_pop16p, acs_workers16p, acs_workers16p_shr)
acsNames <- c("acs_noninst_pop", "acs_sr_noninst_pop", "acs_disabled_pop", "acs_sr_disab_pop",
             "acs_hh","acs_0veh_hh", "acs_low_inc_hh", "acs_pop_5p", "acs_lep_pop",
              "pov_stat_det_est", "pov_stat_det_moe", "lt_2x_pov_est", 
              "lt_2x_pov_moe", "acs_pop16p","acs_workers16p","acs_workers16p_shr")
dict_acsF <- hash(acsNames, acsFields)


dict_acsTab <- hash()
for (x in keys(dict_acsF)){
  intermed <-get_acs(geography = geotype, variables = toupper(substr(dict_acsF[[x]], 1,10)), year = 2014, state = "MA")%>% 
    pivot_wider(names_from = variable, names_glue = "{variable}{.value}",values_from = c(estimate, moe)) %>% filter(GEOID %in% geoids)
  .set(dict_acsTab, paste0(x,"_Table"), intermed)
}



#####CALCULATE FIELDS#####
###BEFORE PERCENTAGE CALCULATIONS, MAKE SURE YOU ARE USING THE CORRECT GEOMETRYS###
#Big Table
bigTable <- dict_cenTab$TotalPop_Table[1:2]
bigTable$area_fraction <- 1
bigTable$proj_id <- c("SharonCC")#c("NewtonCC")#c("Salem_Skipper","MART_Micro", "Everett_TMA", "MART_Micro","MART_Micro","MART_Micro")

bigTable$land_sqmi <- 63172533.376213 #47042489.148792 #c(8.0704, 10.406, 3.437, 17.51, 17.995, 20.709) #this is in sq_miles, but will likely be in meters so see below
bigTable$land_sqmi<- bigTable$land_sqmi*0.0000003861* bigTable$area_fraction #arealand*0.0000003861* bigTable$area_fraction


#total population
dict_cenTab$TotalPop_Table$TotalPop <- dict_cenTab$TotalPop_Table$P001001
bigTable <- merge(x=bigTable, y=dict_cenTab$TotalPop_Table[,c("GEOID","TotalPop")],by="GEOID") 

####POV STAT DET POP####
#Non Inst Pop 
dict_cenTab$NonInstPop_Table$inst_gq_pop <- dict_cenTab$NonInstPop_Table$P042002
dict_cenTab$NonInstPop_Table$dorm_pop <- dict_cenTab$NonInstPop_Table$P042008
dict_cenTab$NonInstPop_Table$mil_qtr_pop <- dict_cenTab$NonInstPop_Table$P042009
bigTable <- merge(x=bigTable, y=dict_cenTab$NonInstPop_Table[,c("GEOID","inst_gq_pop", "dorm_pop", "mil_qtr_pop")],by="GEOID") 

#Un Rel u15
dict_cenTab$UnrelU15Pop_Table$unrel_u15_pop <- dict_cenTab$UnrelU15Pop_Table$P032021 - dict_cenTab$UnrelU15Pop_Table$P032028
bigTable <- merge(x=bigTable, y=dict_cenTab$UnrelU15Pop_Table[,c("GEOID","unrel_u15_pop")],by="GEOID") 

#Pov_Stat_Det_Pop Calc
bigTable<- bigTable %>% mutate(pov_stat_det_pop  = (TotalPop - inst_gq_pop - 
                                                                       dorm_pop - mil_qtr_pop - unrel_u15_pop))

####DISABLED POP ACS####
dict_acsTab$acs_noninst_pop_Table$acs_noninst_pop <- dict_acsTab$acs_noninst_pop_Table$B18101_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_noninst_pop_Table[,c("GEOID","acs_noninst_pop")],by="GEOID") 

dict_acsTab$acs_sr_noninst_pop_Table$acs_sr_noninst_pop <- (dict_acsTab$acs_sr_noninst_pop_Table$B18101_015estimate +
                                                              dict_acsTab$acs_sr_noninst_pop_Table$B18101_018estimate +
                                                              dict_acsTab$acs_sr_noninst_pop_Table$B18101_034estimate +
                                                              dict_acsTab$acs_sr_noninst_pop_Table$B18101_037estimate)
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_sr_noninst_pop_Table[,c("GEOID","acs_sr_noninst_pop")],by="GEOID") 

dict_acsTab$acs_disabled_pop_Table$acs_disab_pop <- (dict_acsTab$acs_disabled_pop_Table$B18101_004estimate +
                                                  dict_acsTab$acs_disabled_pop_Table$B18101_007estimate +
                                                  dict_acsTab$acs_disabled_pop_Table$B18101_010estimate +
                                                  dict_acsTab$acs_disabled_pop_Table$B18101_013estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_016estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_019estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_023estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_026estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_029estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_032estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_035estimate +
                                                    dict_acsTab$acs_disabled_pop_Table$B18101_038estimate)
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_disabled_pop_Table[,c("GEOID","acs_disab_pop")],by="GEOID") 

dict_acsTab$acs_sr_disab_pop_Table$acs_sr_disab_pop <- (dict_acsTab$acs_sr_disab_pop_Table$B18101_016estimate +
                                                              dict_acsTab$acs_sr_disab_pop_Table$B18101_019estimate +
                                                              dict_acsTab$acs_sr_disab_pop_Table$B18101_035estimate +
                                                              dict_acsTab$acs_sr_disab_pop_Table$B18101_038estimate)
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_sr_disab_pop_Table[,c("GEOID","acs_sr_disab_pop")],by="GEOID") 

bigTable$acs_disabled_pop_shr <- round(bigTable$acs_disab_pop/bigTable$acs_noninst_pop,5)
bigTable$acs_sr_disab_pop_shr <- round(bigTable$acs_sr_disab_pop/bigTable$acs_sr_noninst_pop,5)

####HOUSEHOLDS - 0VEH AND LOWINC####
dict_acsTab$acs_hh_Table$acs_hh <- dict_acsTab$acs_hh_Table$B25044_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_hh_Table[,c("GEOID","acs_hh")],by="GEOID")

dict_acsTab$acs_0veh_hh_Table$acs_0veh_hh <- dict_acsTab$acs_0veh_hh_Table$B25044_003estimate+dict_acsTab$acs_0veh_hh_Table$B25044_010estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_0veh_hh_Table[,c("GEOID","acs_0veh_hh")],by="GEOID")
bigTable$acs_0veh_hh_shr <- round(bigTable$acs_0veh_hh/bigTable$acs_hh,5)

dict_acsTab$acs_low_inc_hh_Table$acs_tot_hh <- dict_acsTab$acs_low_inc_hh_Table$B19001_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_low_inc_hh_Table[,c("GEOID","acs_tot_hh")],by="GEOID")
dict_acsTab$acs_low_inc_hh_Table$acs_low_inc_hh <- round(dict_acsTab$acs_low_inc_hh_Table$B19001_002estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_003estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_004estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_005estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_006estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_007estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_008estimate+
                                                           dict_acsTab$acs_low_inc_hh_Table$B19001_009estimate+
                                                           (392/5000)*dict_acsTab$acs_low_inc_hh_Table$B19001_010estimate,0)
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_low_inc_hh_Table[,c("GEOID","acs_low_inc_hh")],by="GEOID")
bigTable$acs_low_inc_hh_shr = round(bigTable$acs_low_inc_hh/bigTable$acs_tot_hh,5)

####LEP####
dict_acsTab$acs_pop_5p_Table$acs_pop_5p <- dict_acsTab$acs_pop_5p_Table$B16004_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_pop_5p_Table[,c("GEOID","acs_pop_5p")],by="GEOID")

dict_acsTab$acs_lep_pop_Table$acs_lep_pop <- (dict_acsTab$acs_lep_pop_Table$B16004_001estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_003estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_005estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_010estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_015estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_020estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_025estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_027estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_032estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_037estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_042estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_047estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_049estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_054estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_059estimate-
                                                dict_acsTab$acs_lep_pop_Table$B16004_064estimate) 
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_lep_pop_Table[,c("GEOID","acs_lep_pop")],by="GEOID")

bigTable$acs_lep_pop_shr <- round(bigTable$acs_lep_pop/bigTable$acs_pop_5p,5)

####POV STAT DET & LT 2X POV####
dict_acsTab$pov_stat_det_est_Table$pov_stat_det_est <- dict_acsTab$pov_stat_det_est_Table$C17002_001estimate
dict_acsTab$pov_stat_det_est_Table$pov_stat_det_moe <- dict_acsTab$pov_stat_det_est_Table$C17002_001moe
bigTable <- merge(x=bigTable, y=dict_acsTab$pov_stat_det_est_Table[,c("GEOID","pov_stat_det_est", "pov_stat_det_moe")],by="GEOID")

dict_acsTab$lt_2x_pov_est_Table$lt_2x_pov_est <- dict_acsTab$lt_2x_pov_est_Table$C17002_001estimate - dict_acsTab$lt_2x_pov_est_Table$C17002_008estimate
#std_err_sum (e.g. standard error of a sum or difference)
#std_err_sum =  round(((stderr1^2)+(stderr2^2))*0.5,0)
dict_acsTab$lt_2x_pov_est_Table$lt_2x_pov_moe <- round(((dict_acsTab$lt_2x_pov_est_Table$C17002_001moe^2)+(dict_acsTab$lt_2x_pov_est_Table$C17002_008moe^2))*0.5,0)
bigTable <- merge(x=bigTable, y=dict_acsTab$lt_2x_pov_est_Table[,c("GEOID","lt_2x_pov_est", "lt_2x_pov_moe")],by="GEOID")

bigTable <- bigTable %>% mutate(lt_2x_pov_shr_est = ifelse(pov_stat_det_est == 0, 0, round(lt_2x_pov_est/pov_stat_det_est, 5)))
#std_err_proportion (e.g. standard error of a proportion)
bigTable$lt_2x_pov_shr_moe <- apply(bigTable, 1, std_err_proportion, numerator=bigTable$lt_2x_pov_est,denominator =bigTable$pov_stat_det_est,
                                      std_err_numerator =bigTable$lt_2x_pov_moe, std_err_denominator = bigTable$pov_stat_det_moe)[,1] #delete [,1] when running on one row

####ACS & CENSUS AGE MIN AND HH#####
dict_acsTab$acs_pop16p_Table$acs_pop16p <- dict_acsTab$acs_pop16p_Table$B23001_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_pop16p_Table[,c("GEOID","acs_pop16p")],by="GEOID")

dict_acsTab$acs_workers16p_Table$acs_workers16p <- dict_acsTab$acs_workers16p_Table$B08301_001estimate
bigTable <- merge(x=bigTable, y=dict_acsTab$acs_workers16p_Table[,c("GEOID","acs_workers16p")],by="GEOID")

bigTable <- bigTable %>% mutate(acs_workers16p_shr = ifelse(acs_pop16p == 0, 0, round(acs_workers16p/acs_pop16p,5)))

dict_cenTab$Pop75p_Table$bg_pop75p <- (dict_cenTab$Pop75p_Table$P012023+
                                      dict_cenTab$Pop75p_Table$P012024+
                                      dict_cenTab$Pop75p_Table$P012025+
                                      dict_cenTab$Pop75p_Table$P012047+
                                      dict_cenTab$Pop75p_Table$P012048+
                                      dict_cenTab$Pop75p_Table$P012049)
bigTable <- merge(x=bigTable, y=dict_cenTab$Pop75p_Table[,c("GEOID","bg_pop75p")],by="GEOID")
bigTable$buf_pop75p <- bigTable$area_fraction*bigTable$bg_pop75p

dict_cenTab$MinPop_Table$bg_min_pop <- dict_cenTab$MinPop_Table$P005001 - dict_cenTab$MinPop_Table$P005003
bigTable <- merge(x=bigTable, y=dict_cenTab$MinPop_Table[,c("GEOID","bg_min_pop")],by="GEOID")
bigTable$buf_min_pop <- bigTable$area_fraction*bigTable$bg_min_pop

dict_cenTab$Pop18u_Table$bg_pop18u <- (dict_cenTab$Pop18u_Table$P012027+
                                          dict_cenTab$Pop18u_Table$P012028+
                                          dict_cenTab$Pop18u_Table$P012029+
                                          dict_cenTab$Pop18u_Table$P012030+
                                          dict_cenTab$Pop18u_Table$P012003+
                                          dict_cenTab$Pop18u_Table$P012004+
                                          dict_cenTab$Pop18u_Table$P012005+
                                          dict_cenTab$Pop18u_Table$P012006)
bigTable <- merge(x=bigTable, y=dict_cenTab$Pop18u_Table[,c("GEOID","bg_pop18u")],by="GEOID")
bigTable$buf_pop18u <- bigTable$area_fraction*bigTable$bg_pop18u

dict_cenTab$Pop5p_Table$bg_pop5p <- dict_cenTab$Pop5p_Table$P012001 - dict_cenTab$Pop5p_Table$P012003 - dict_cenTab$Pop5p_Table$P012027
bigTable <- merge(x=bigTable, y=dict_cenTab$Pop5p_Table[,c("GEOID","bg_pop5p")],by="GEOID")
bigTable$buf_pop5p <- bigTable$area_fraction*bigTable$bg_pop5p

dict_cenTab$TotalHH_Table$bg_hh <- dict_cenTab$TotalHH_Table$P018001
bigTable <- merge(x=bigTable, y=dict_cenTab$TotalHH_Table[,c("GEOID","bg_hh")],by="GEOID")
bigTable$buf_hh <- bigTable$area_fraction*bigTable$bg_hh

dict_cenTab$NonInstPop_Table$bg_noninst_pop <- (dict_cenTab$NonInstPop_Table$P016001 +
                                                  dict_cenTab$NonInstPop_Table$P042001-
                                                  dict_cenTab$NonInstPop_Table$P042002-
                                                  dict_cenTab$NonInstPop_Table$P042009)
bigTable <- merge(x=bigTable, y=dict_cenTab$NonInstPop_Table[,c("GEOID","bg_noninst_pop")],by="GEOID")
bigTable$buf_noninst_pop <- bigTable$area_fraction*bigTable$bg_noninst_pop

dict_cenTab$Pop16p_Table$bg_pop16p <- (dict_cenTab$Pop16p_Table$P012001-
                                          dict_cenTab$Pop16p_Table$P012003+
                                          dict_cenTab$Pop16p_Table$P012004+
                                          dict_cenTab$Pop16p_Table$P012005+
                                          dict_cenTab$Pop16p_Table$P012006+
                                          dict_cenTab$Pop16p_Table$P012027+
                                          dict_cenTab$Pop16p_Table$P012028+
                                          dict_cenTab$Pop16p_Table$P012029+
                                          dict_cenTab$Pop16p_Table$P014018+
                                          dict_cenTab$Pop16p_Table$P014039 
                                          )
bigTable <- merge(x=bigTable, y=dict_cenTab$Pop16p_Table[,c("GEOID","bg_pop16p")],by="GEOID")
bigTable$buf_pop16p <- bigTable$area_fraction*bigTable$bg_pop16p #used to be buf_pop16p but shouldn't exist yet

bigTable$buf_pop_pov_stat_det <- round(bigTable$area_fraction*bigTable$pov_stat_det_pop,2)
bigTable$buf_pop <- round(bigTable$area_fraction * bigTable$TotalPop,5)


#FACTORS
bigTable <- bigTable %>% mutate(hh_fct=round(ifelse(bg_hh==0,0,buf_hh/bg_hh)),3)
bigTable <- bigTable %>% mutate(pop5p_fct=round(ifelse(bg_pop5p==0,0,buf_pop5p/bg_pop5p)),3)
bigTable <- bigTable %>% mutate(pop16p_fct=round(ifelse(bg_pop16p==0,0,buf_pop16p/bg_pop16p)),3)
bigTable <- bigTable %>% mutate(noninst_fct=round(ifelse(bg_noninst_pop==0,0,buf_noninst_pop/bg_noninst_pop)),3)
bigTable <- bigTable %>% mutate(pop_pov_stat_det_fct=round(ifelse(pov_stat_det_pop==0,0,buf_pop_pov_stat_det/pov_stat_det_pop)),3)
bigTable <- bigTable %>% mutate(pop_fct=round(ifelse(TotalPop==0,0,buf_pop/TotalPop)),3)

#FINAL FIELDS
bigTable$pop_18u <- round(bigTable$buf_pop18u, 0)
bigTable$Pop75p <- round(bigTable$buf_pop75p, 0)
bigTable$minority_pop <- round(bigTable$buf_min_pop, 0)
bigTable$noninst_pop <- round(bigTable$buf_noninst_pop,0)
bigTable$disabled_pop <- round(bigTable$acs_disabled_pop_shr*bigTable$buf_noninst_pop,0)
bigTable$tot_hh <- round(bigTable$buf_hh,0)
bigTable$veh0_hh <- round(bigTable$buf_hh*bigTable$acs_0veh_hh_shr,0)
bigTable$low_inc_hh <- round(bigTable$buf_hh*bigTable$acs_low_inc_hh_shr,0)
bigTable$total_pop_5p <- round(bigTable$buf_pop5p,0)
bigTable$lep_pop <- round(bigTable$buf_pop5p*bigTable$acs_lep_pop_shr,0)
bigTable$tot_pop_pov_stat_det <- round(bigTable$buf_pop_pov_stat_det,0)
bigTable$pop_lt_2x_pov_stat <- round(bigTable$buf_pop_pov_stat_det*bigTable$lt_2x_pov_shr_est,0)
bigTable$tot_pop16p <- round(bigTable$buf_pop16p,0)
bigTable$tot_workers16p <- round(bigTable$buf_pop16p*bigTable$acs_workers16p_shr,0)

#filter_fields <- c(TotalPop,	Pop75p,	minority_pop,	noninst_pop,	disabled_pop,	total_pop_5p,	lep_pop,
                   #tot_workers16p,	tot_pop_pov_stat_det,	pop_lt_2x_pov_stat,	tot_hh,	low_inc_hh,	veh0_hh,	pop_18u)
bigTable$pop_den <- bigTable$TotalPop/bigTable$land_sqmi#THIS IS FROM 2/27/21

smallTable <- bigTable %>% select(proj_id,TotalPop,	Pop75p,	minority_pop,land_sqmi,pop_den,	noninst_pop,	disabled_pop,	total_pop_5p,	lep_pop,
                                  tot_workers16p,	tot_pop_pov_stat_det,	pop_lt_2x_pov_stat,	tot_hh,	low_inc_hh,	veh0_hh,	pop_18u)

smallTable <- smallTable %>% group_by(proj_id) %>% summarise(TotalPop = sum(TotalPop),	
                                               Pop75p = sum(Pop75p),	
                                               minority_pop = sum(minority_pop),
                                               land_sqmi = sum(land_sqmi),
                                               noninst_pop = sum(noninst_pop),	
                                               disabled_pop = sum(disabled_pop),	
                                               total_pop_5p = sum(total_pop_5p),	
                                               lep_pop = sum(lep_pop),
                                               tot_workers16p = sum(tot_workers16p),	
                                               tot_pop_pov_stat_det = sum(tot_pop_pov_stat_det),	
                                               pop_lt_2x_pov_stat = sum(pop_lt_2x_pov_stat),	
                                               tot_hh = sum(tot_hh),	
                                               low_inc_hh = sum(low_inc_hh),	
                                               veh0_hh = sum(veh0_hh),	
                                               pop_18u = sum(pop_18u))
smallTable$pop_den <- round(smallTable$TotalPop/smallTable$land_sqmi,5)
smallTable$pct_75p <- smallTable$Pop75p/smallTable$TotalPop
smallTable$pct_18u <- smallTable$pop_18u/smallTable$TotalPop
smallTable$minority_pct <- smallTable$minority_pop/smallTable$TotalPop
smallTable$disabled_pct <- smallTable$disabled_pop/smallTable$TotalPop
smallTable$lep_pct <- smallTable$lep_pop/smallTable$TotalPop
smallTable$pct_lt_2x_pov_stat <- smallTable$pop_lt_2x_pov_stat/smallTable$TotalPop
smallTable$pct_low_inc <- smallTable$low_inc_hh/smallTable$tot_hh
smallTable$pct_veh0 <- smallTable$veh0_hh/smallTable$tot_hh

write.csv(smallTable, file = "CC_Sharon_PolygonDemographics.csv")

####SO BROKEN####
bigTable$lt_2x_pov_shr_moe <- apply(bigTable, 1, std_err_proportion, numerator=bigTable$lt_2x_pov_est,denominator =bigTable$pov_stat_det_est,
      std_err_numerator =bigTable$lt_2x_pov_moe, std_err_denominator = bigTable$pov_stat_det_moe)[,1]
