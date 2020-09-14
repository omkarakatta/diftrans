### Header ---------------------------
###
### Title: cleaned_data.R
###
### Description: Import and clean raw data; Set up default options
###
### Author: Omkar A. Katta
###
### Notes: The original data set cannot be shared, so this document is mainly
###   for observation purposes. This script documents the procedure used to clean
###   the original data sets.
###
###   Note that the defaults were not saved to the package because they only
###   clutter the workspace.
###
###

### Preliminaries ---------------------------

library(magrittr)

# NEED TO CHANGE
rawdata_path <- "/Users/omkar_katta/BFI/3_BMP_GP/data-raw/GuisChineseCities (1).csv"

version <- "original"
MSRP_lowerlim <- 0
MSRP_upperlim <- 5e6
Beijing_treatment_date <- "2011-01-01"
Tianjin_treatment_date <- "2014-01-01"

### Get cleaning functions ---------------------------

source(here::here("data-raw", "cleaning_functions.R"))

### Import Clean Data ---------------------------

source(here::here("data-raw", "cleaning_script.R"))

### Save Data ---------------------------

# name <- paste(version, "Beijing", "cleaned", sep = "_")
# assign(name, Beijing, envir = .GlobalEnv)
#
# name <- paste(version, "Tianjin", "cleaned", sep = "_")
# assign(name, Tianjin, envir = .GlobalEnv)
#
# name <- paste(version, "Shijiazhuang", "cleaned", sep = "_")
# assign(name, Shijiazhuang, envir = .GlobalEnv)

# NEED TO CHANGE depending on version
# save(original_Beijing_cleaned, file = here::here("data", paste(version, "Beijing.RData", sep = "_")), compress = "xz")
# save(original_Tianjin_cleaned, file = here::here("data", paste(version, "Tianjin.RData", sep = "_")), compress = "xz")
# save(original_Shijiazhuang_cleaned, file = here::here("data", paste(version, "Shijiazhuang.RData", sep = "_")), compress = "xz")

Beijing_cleaned <- Beijing
Tianjin_cleaned <- Tianjin
Shijiazhuang_cleaned <- Shijiazhuang

save(Beijing_cleaned, file = here::here(".hidden", "Beijing_cleaned.RData"), compress = "xz")
save(Tianjin_cleaned, file = here::here(".hidden", "Tianjin_cleaned.RData"), compress = "xz")
save(Shijiazhuang_cleaned, file = here::here(".hidden", "Shijiazhuang_cleaned.RData"), compress = "xz")

### Save Defaults ---------------------------

rm(list = ls())
grayscale <- F
temp <- ifelse(grayscale, "grayscale", "color")

fontsize <- 20
fontsizeaxis <- 15
linetype0 <- "solid"
linetype1 <- "dashed"
linetype2 <- "dotted"

default_width <- 7
default_height <- 3

show_fig <- TRUE
show_fig1 <- FALSE
show_fig2 <- FALSE
show_fig3 <- FALSE
show_fig4 <- FALSE
show_fig5 <- FALSE
show_fig6 <- FALSE
show_fig7 <- FALSE
show_fig8 <- FALSE
show_fig9 <- FALSE
show_fig10 <- FALSE
show_fig11 <- FALSE

save_fig <- TRUE
save_fig1 <- FALSE
save_fig2 <- FALSE
save_fig3 <- FALSE
save_fig4 <- FALSE
save_fig5 <- FALSE
save_fig6 <- FALSE
save_fig7 <- FALSE
save_fig8 <- FALSE
save_fig9 <- FALSE
save_fig10 <- FALSE
save_fig11 <- FALSE

show_diff_in_diff <- F

suffix <- "_"

# save.image(here::here("data", "defaults.RData"))
