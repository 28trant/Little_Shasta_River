
# Code description --------------------------------------------------------

# This code is used to run the functional flow calculator on the historical USGS gage on the Little Shasta River (11516900), which has a period of record from October 1, 1957 through September 29, 1978. This will be the surrogate dataset to calculated the unimpaired functional flow metrics and compare those to the ones on rivers.codefornature.org


# Libraries ---------------------------------------------------------------

library(ffcAPIClient)
library(tidyverse)
library(lubridate)

# Load data ---------------------------------------------------------------

#comid and gage id to run functional flow calculator
Ann_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBbm4iLCJsYXN0TmFtZSI6IldpbGxpcyIsImVtYWlsIjoiYXdpbGxpc0B1Y2RhdmlzLmVkdSIsInJvbGUiOiJVU0VSIiwiaWF0IjoxNjA1NjUxMTc4fQ.MzMJ23D6tRmOD9Sr3OcQoCVLLndsV2w5ZKdzwgDOeOM"

LSR_comid <- 3917176
LSR_gage_id <- 11516900


# Run calculator ----------------------------------------------------------

ffc_lsr <- FFCProcessor$new()  # make a new object we can use to run the commands

# configure the object and run CEFF step 1 plus outputs

ffc_lsr$step_one_functional_flow_results(gage_id=LSR_gage_id,
                                     token=Ann_token,
                                     output_folder = "functional_flows")
# Step 2

ffc_lsr$step_two_explore_ecological_flow_criteria()


# Step 3

ffc_lsr$step_three_assess_alteration()
