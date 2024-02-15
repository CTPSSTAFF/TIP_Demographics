library(tidycensus)
library(tidyverse)

# UPDATED FOR 2025-2029 TIP 2/15/24 - ACS YEAR UPDATED TO 2022
acs_yr <- 2022

mpo_towns_table <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\needs_assessment_proximity\\mpo_towns.csv")
bluebikes_towns_table <- read_csv("C:\\Users\\sstrumwasser.AD\\Documents\\ArcGIS\\Projects\\needs_assessment_proximity\\Bluebikes_Towns.csv")

# ACS 2022 Fields
# universe
acs_noninst_pop <- c("b18101_001e")

# senior
acs_sr_noninst_pop <- c("b18101_015e","b18101_018e","b18101_034e","b18101_037e")

# disabled
acs_disabled_pop <- c("b18101_004e","b18101_007e","b18101_010e","b18101_013e","b18101_016e","b18101_019e",
                      "b18101_023e","b18101_026e","b18101_029e","b18101_032e","b18101_035e","b18101_038e")

# senior disabled
acs_sr_disab_pop <- c("b18101_016e","b18101_019e","b18101_035e","b18101_038e")

# lep universe
acs_pop_5p <- c("b16004_001e")

# lep populations
acs_lep_pop <- c("b16004_001e","b16004_003e","b16004_005e","b16004_010e","b16004_015e","b16004_020e","b16004_025e","b16004_027e","b16004_032e","b16004_037e",
                 "b16004_042e","b16004_047e","b16004_049e","b16004_054e","b16004_059e","b16004_064e")

# poverty status determined estimate
pov_stat_det_est <- c("c17002_001e")
# poverty status determined margin of error
pov_stat_det_moe <- c("c17002_001m")

# income less than 2x poverty level population
lt_2x_pov_est <- c("c17002_001e","c17002_008e")
lt_2x_pov_moe <- c("c17002_001m","c17002_008m")

# youth and older adults - not used for 2025-29 TIP
acs_pop75p <- c("B01001_023E","B01001_024E","B01001_025E","B01001_047E","B01001_048E","B01001_049E")
acs_pop18u <- c("B01001_001E","B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_027E","B01001_028E","B01001_029E","B01001_030E")


#all acs fields
acs_fields <- c(acs_noninst_pop,acs_sr_noninst_pop,acs_disabled_pop,
                acs_pop_5p,acs_lep_pop,pov_stat_det_est,
                pov_stat_det_moe,lt_2x_pov_est,lt_2x_pov_moe,
                acs_pop75p,acs_pop18u)


# download block group geography and demographic data
acs_towns <- get_acs(geography = 'county subdivision', variables = toupper(acs_fields), year = acs_yr, state = "MA", output = 'wide')

# get decennial data
bg20_towns <- get_decennial(geography = 'county subdivision', 
                            variables = c('P2_001N', 'P2_003N', # white and minority
                                          'P12_001N', 'P12_023N', 'P12_024N', 'P12_025N', 'P12_047N', 
                                          'P12_048N', 'P12_049N', # Older adults
                                          'P12_003N', 'P12_004N', 'P12_005N', 'P12_006N', 'P12_027N',  
                                          'P12_028N', 'P12_029N', 'P12_030N'), # Youth 
                            year = 2020, 
                            sumfile = "dhc", 
                            state = "MA", 
                            output = 'wide')

bg20_towns$MinPop <- bg20_towns$P2_001N - bg20_towns$P2_003N

# Older adults and youth
bg20_towns <- bg20_towns %>% 
  mutate(OldPop = sum(P12_001N, P12_023N, P12_024N, P12_025N, P12_047N, 
                      P12_048N, P12_049N),
         YouthPop = sum(P12_003N, P12_004N, P12_005N, P12_006N, P12_027N,  
                        P12_028N, P12_029N, P12_030N))

# standardize town name capitalization
acs_towns$TOWN <- toupper(sub("([A-Za-z]+).*", "\\1", acs_towns$NAME))
bg20_towns$TOWN <- toupper(sub("([A-Za-z]+).*", "\\1", bg20_towns$NAME))

# fix North Reading
acs_towns$TOWN[grepl('North Reading', acs_towns$NAME)] <- 'NORTH READING'
bg20_towns$TOWN[grepl('North Reading', bg20_towns$NAME)] <- 'NORTH READING'



# ACS processing ---------------------------------------------------------

####low inc####
acs_towns$pov_stat_det_est <- acs_towns$C17002_001E
acs_towns$pov_stat_det_moe <- acs_towns$C17002_001M

acs_towns$lt_2x_pov_est <- acs_towns$C17002_001E - acs_towns$C17002_008E


####LEP####
acs_towns$acs_pop_5p <- acs_towns$B16004_001E

acs_towns$acs_lep_pop <- (acs_towns$B16004_001E - acs_towns$B16004_003E - acs_towns$B16004_005E - 
                         acs_towns$B16004_010E - acs_towns$B16004_015E - acs_towns$B16004_020E - 
                         acs_towns$B16004_025E - acs_towns$B16004_027E - acs_towns$B16004_032E - 
                         acs_towns$B16004_037E - acs_towns$B16004_042E - acs_towns$B16004_047E - 
                         acs_towns$B16004_049E - acs_towns$B16004_054E - acs_towns$B16004_059E - 
                         acs_towns$B16004_064E) 

acs_towns$bg_totpop <- acs_towns$B01001_001E

#### DISAB POP ####
acs_towns$acs_disab_pop <- (acs_towns$B18101_004E + acs_towns$B18101_007E + acs_towns$B18101_010E +  
                            acs_towns$B18101_013E + acs_towns$B18101_016E + acs_towns$B18101_019E +
                            acs_towns$B18101_023E + acs_towns$B18101_026E + acs_towns$B18101_029E +
                            acs_towns$B18101_032E + acs_towns$B18101_035E + acs_towns$B18101_038E)

# join census tables  to towns table
towns_census <- mpo_towns_table %>% 
  left_join(bg20_towns, by='TOWN') %>%
  left_join(acs_towns, by='TOWN')

# summarise for full mpo
mpo_stats <- towns_census %>%
  summarise(
    TotalPop_MPO = sum(P2_001N),
    MinPop_MPO = sum(MinPop),
    NonMinPop_MPO = sum(P2_003N),
    low_inc_pop_MPO = sum(lt_2x_pov_est),
    lep_pop_MPO = sum(acs_lep_pop),
    disab_pop_MPO = sum(acs_disab_pop),
    youth_pop_MPO = sum(YouthPop),
    elderly_pop_MPO = sum(OldPop)
  )

write_csv(mpo_stats, "C:\\Users\\sstrumwasser.AD\\Documents\\GitHub\\TIP_Demographics\\mpo_pops.csv")




