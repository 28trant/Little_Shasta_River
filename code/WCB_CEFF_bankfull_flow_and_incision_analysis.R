
# Code description --------------------------------------------------------

# This code was developed to calculate the bankfull flows of each cross-section that was drawn for the incision analysis in WCB_CEFF_lidar_xsxn.R. The purpose is to compare calculated bankfull flows with the Peak 2 flood flow. If bankfull flows are generally lower than the Peak 2, then there is no incision. If bankful flows are generally higher, then there is some incision that needs to be considered for the final functional flow recommendations.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr)
library(plotly)
library(data.table) #used to extract minimum points from xsxns
library(sf) #used to load and read spatial data


# Load data ---------------------------------------------------------------

# Load data that has geometry and elevation
# May need to load the tiff; can use the one on the local data folder


# Next steps:
#      For each cross-section:
# Identify bankfull segment
# Calculate area and hydraulic radius
# Calculate flow (Q) using Manning's equation: 
#      If using U.S. units: Q = (1.49/n)*A*R^(2/3)*S^(1/2)
#      If using S.I. units: Q = (1.00/n)*A*R^(2/3)*S^(1/2)