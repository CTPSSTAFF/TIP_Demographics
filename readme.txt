This takes input from a network analysis of TIP projects. That analysis showed the amount of each census geography covered by one of the TIP projects.

Demographics for the same geography are obtained using the `tidycensus` package in `01_TIP_Equity22.Rmd`. The two files are merged to find the various proprotions of relevant demographics near the TIP projects.

In 02_get_cen20_pop_hh.R we obtain the 1) Total Population, 2) Total Number of *Occupied* Households, and 3) The number of people aged 18 or older at the census block group level. We estimate the total for these values based on the input TIP files.

In 03_join_pop_hh_TIP.R we join the two datasets together to find the number of people/households in each category.

NOTES:
We tried to use 2016-2020 ACS data but the input GEOIDs, and presumably the underlying geography, didn't match up perfectly. Because 4/5 of the data is the same and the schedule pressure meant there wasn't time to reconcile the issue, we chose to continue using the 2014-2019 ACS data. The 

The "workers16p_num" field is the percent of workers 16 plus value multiplied by the population older than 18 from the 2020 US Census. The population of people aged 16+ was not available at time of analysis (available May 2023!).

