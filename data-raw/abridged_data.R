### Header ---------------------------
###
### Title: abridged_data.R
###
### Description: This script creates an abridged version of the anonymized data.
###
### Author: Omkar A. Katta
###
### Notes: The abridged versions contain the price of each car mode and the number
###   of sales by year/month (and city).
###
###

### Preliminaries ---------------------------
library(magrittr)

load(here::here("data", "Beijing_sample.RData"))
load(here::here("data", "Tianjin_sample.RData"))
load(here::here("data", "Shijiazhuang_sample.RData"))

### Prepare Data ---------------------------

Beijing_abridged <- Beijing_sample %>%
  dplyr::select(MSRP, sales, year, month, city)
Tianjin_abridged  <- Tianjin_sample %>%
  dplyr::select(MSRP, sales, year, month, city)
Shijiazhuang_abridged  <- Shijiazhuang_sample %>%
  dplyr::select(MSRP, sales, year, month, city)

### Save Data ---------------------------

save(Beijing_abridged, file = here::here("data", "Beijing_abridged.RData"), compress = "xz")
save(Tianjin_abridged, file = here::here("data", "Tianjin_abridged.RData"), compress = "xz")
save(Shijiazhuang_abridged, file = here::here("data", "Shijiazhuang_abridged.RData"), compress = "xz")
