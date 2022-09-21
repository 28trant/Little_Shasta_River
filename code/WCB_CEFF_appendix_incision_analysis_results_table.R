
# Code description --------------------------------------------------------

# This code was developed to organize the results of the incision analysis to add as an appendix to the Little Shasta-CEFF case study. The appendix will include a map with labelled cross-sections and a table of cross-section geometry and calculated bankfull flow.


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

incision_results <- readRDS("output/LOI_all_xsxns_and_bankfull_flow.rds")


# Wrangle results ---------------------------------------------------------

#R_wetted_perimeter is a remnant of a previous, incorrect analysis for hydraulic radius; drop this column from the results

incision_results_revised <- incision_results %>% 
  select(xsxn_id, LOI_id, slope, Manning_n, Area, R_hyd_rad, BF_flow_cfs) %>% 
  
  mutate(BF_flow_cfs_integer = round(BF_flow_cfs)) %>%
  mutate(Area_rounded = round(Area, digits = 2)) %>% 
  mutate(R_hyd_rad_rounded = round(R_hyd_rad, digits = 2))
  
incision_results_final <- incision_results_revised %>% 
  select(xsxn_id, LOI_id, slope, Manning_n, Area_rounded, R_hyd_rad_rounded, BF_flow_cfs_integer) %>%
  rename("cross section ID" = xsxn_id, "LOI #" = LOI_id, "channel slope" = slope, "Manning's n" = Manning_n, "Area (sq. ft)" = Area_rounded, "hydraulic radius" = R_hyd_rad_rounded, "bankfull flow (cfs)" = BF_flow_cfs_integer) 


# Save --------------------------------------------------------------------

write_csv(incision_results_final, "output/incision_analysis_appendix_table.csv")

