# TIP_Demographics

This script pulls census and ACS data to calculate the numbers and percentages of equity populations within a distance of TIP projects. These data are used for the TIP
Equity Scoring process. The most recent verion (TIP_Equity24.Rmd) uses 2020 census data and 2020 ACS data.

INPUTS
1. Table of area fractions by project and census geometry (blocks, block groups, etc.) -- this is the output from the network buffer analysis process                  (https://github.com/CTPSSTAFF/network-buffer-analysis).
 
OUTPUTS
1. A table with the population and percent of the total population of each equity population for each project
