### Header ---------------------------
###
### Title: sample_data.R
###
### Description: Since the original data sets cannot be shared, the purpose of
###   of this file is to create an anonymized version of the data for others to use.
###
### Author: Omkar A. Katta
###
### Notes: The results of the paper cannot be reproduced exactly when using
###   these data, but they should be close.
###
###

### Preliminaries ---------------------------

library(magrittr)
library(dplyr)

Beijing_path <- here::here(".hidden", "Beijing_cleaned.RData")
Tianjin_path <- here::here(".hidden", "Tianjin_cleaned.RData")
Shijiazhuang_path <- here::here(".hidden", "Shijiazhuang_cleaned.RData")

load(Beijing_path)
load(Tianjin_path)
load(Shijiazhuang_path)

### Sample ---------------------------

sample_data <- function(originaldata, count = "sales", seed = 914){
  set.seed(seed)
  newcount <- rmultinom(1, sum(originaldata[count]), unlist(originaldata[count]))
  newdata <- originaldata
  newdata[count] <- as.numeric(newcount)
  return(newdata)
}

Beijing_sample <- sample_data(Beijing_cleaned)
Tianjin_sample <- sample_data(Tianjin_cleaned)
Shijiazhuang_sample <- sample_data(Shijiazhuang_cleaned)

### Save Data ---------------------------

save(Beijing_sample, file = here::here("data", "Beijing_sample.RData"), compress = "xz")
save(Tianjin_sample, file = here::here("data", "Tianjin_sample.RData"), compress = "xz")
save(Shijiazhuang_sample, file = here::here("data", "Shijiazhuang_sample.RData"), compress = "xz")
