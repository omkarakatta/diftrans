### Meta ---------------------------
###
### Title: reproduce_results2.R
###
### Description: Reproduce results of Daljord, Pouliot, Hu, and Xiao (2021)
###
### Author: Omkar A. Katta
###
### ---------------------------
###
### Notes: This file will eventually replace data-raw/reproduce_results.R.
###   Due to privacy restrictions, the original data set cannot be distributed.
###   Hence, an anonymized, bootstrapped version of the data set is
###   made available. The restuls of this script will therefore differ slightly
###   from the paper's results.

### Preliminaries ---------------------------

load(here::here(".hidden", "Beijing_cleaned.RData"))
load(here::here(".hidden", "Tianjin_cleaned.RData"))
load(here::here(".hidden", "Shijiazhuang_cleaned.RData"))

Beijing_sample <- Beijing_cleaned
Tianjin_sample <- Tianjin_cleaned
Shijiazhuang_sample <- Shijiazhuang_cleaned

devtools::load_all()
library(magrittr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(stargazer)
library(latex2exp)
source(here::here("data-raw", "prepare_data.R"))
source(here::here("data-raw", "plotting_functions.R"))

version <- "original"

if (version == "original") {

  Beijing <- Beijing_sample
  Tianjin <- Tianjin_sample
  Shijiazhuang <- Shijiazhuang_sample
  seedplus <- 0 # i.e. do not modify seed
  message(paste("Version: ", version, sep = ""))

} else if (version == "v1") {

  # filter out Dec 2010
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12))
  Tianjin <- Tianjin_sample %>%
    filter(!(year == 2010 & month == 12))
  Shijiazhuang <- Shijiazhuang_sample %>%
    filter(!(year == 2010 & month == 12))
  seedplus <- 1
  message(paste("Version: ", version, sep = ""))

} else if (version == "v2") {

  # filter out Dec 2010, Jan 2011, and Feb 2011
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Tianjin <- Tianjin_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Shijiazhuang <- Shijiazhuang_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  seedplus <- 2
  message(paste("Version: ", version, sep = ""))

} else {
  stop("Specify a valid value for `version`.")
}

support_Beijing <- prep_data(Beijing, prep = "support")
support_Tianjin <- prep_data(Tianjin, prep = "support")
pre_Beijing <- prep_data(Beijing, prep = "pmf", support = support_Beijing,
                         lowerdate = "2010-01-01", upperdate = "2011-01-01")
post_Beijing <- prep_data(Beijing, prep = "pmf", support = support_Beijing,
                          lowerdate = "2011-01-01", upperdate = "2012-01-01")
pre_Tianjin <- prep_data(Tianjin, prep = "pmf", support = support_Tianjin,
                         lowerdate = "2010-01-01", upperdate = "2011-01-01")
post_Tianjin <- prep_data(Tianjin, prep = "pmf", support = support_Tianjin,
                          lowerdate = "2011-01-01", upperdate = "2012-01-01")

ba <- diftrans(
  pre_main = pre_Beijing,
  post_main = post_Beijing,
  sims_bandwidth_selection = 500,
  sims_subsampling = 100,
  subsample_pre_main_size = floor(sum(pre_Beijing$count) * 0.75),
  subsample_post_main_size = floor(sum(post_Beijing$count) * 0.75),
  seed = 1,
  conservative = FALSE,
  quietly = FALSE
)

save(ba, file = here::here("data", "ba.RData"))

dit <- diftrans(
  pre_main = pre_Beijing,
  post_main = post_Beijing,
  pre_control = pre_Tianjin,
  post_control = post_Tianjin,
  sims_bandwidth_selection = 500,
  sims_subsampling = 100,
  subsample_pre_main_size = floor(sum(pre_Beijing$count) * 0.75),
  subsample_post_main_size = floor(sum(post_Beijing$count) * 0.75),
  subsample_pre_control_size = floor(sum(pre_Tianjin$count) * 0.75),
  subsample_post_control_size = floor(sum(post_Tianjin$count) * 0.75),
  seed = 2,
  conservative = TRUE,
  quietly = FALSE
)

save(dit, file = here::here("data", "dit.RData"))
