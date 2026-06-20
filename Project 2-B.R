# ADSL, ADAE, ADLBSI, ADTTE, ADEX

library(dplyr)
library(lubridate)
library(haven)
library(stringr)
library(readxl)

# Read in SDTM datasets
rawdata <- list(
    ae = read_xpt("SDTM xpt/AE.xpt"),
    cm = read_xpt("SDTM xpt/CM.xpt"),
    

)

# Read in the ADaM datasets
validation <- list(
    adae = read_xpt("ADaM xpt/ADAE.xpt"),
    adsl = read_xpt("ADaM xpt/ADSL.xpt"),
    adlbsi = read_xpt("ADaM xpt/ADLBSI.xpt"),
    adtte = read_xpt("ADaM xpt/ADTTE.xpt"),
    adex = read_xpt("ADaM xpt/ADEX.xpt"))