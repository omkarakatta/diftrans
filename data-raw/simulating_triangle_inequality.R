### Header ---------------------------
###
### Title: simulating_triangle_inequality.R
###
### Description: R script to try different formulations of OT that satisfy triangle inequality
###
### Author: Omkar A. Katta
###
### Notes: 
###

### Preliminaries ---------------------------
devtools::load_all()

source(here::here("data-raw", "prepare_data.R"))
load(here::here(".hidden/Beijing_cleaned.RData"))
Beijing <- Beijing_cleaned

common_support <- prep_data(Beijing, prep = "support", lowerdate = "2010-01-01", upperdate = "2013-01-01")

pre <- prep_data(Beijing, prep = "pmf",
                 support = common_support,
                 lowerdate = "2010-01-01", upperdate = "2011-01-01")
middle <- prep_data(Beijing, prep = "pmf",
                    support = common_support,
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")
post <- prep_data(Beijing, prep = "pmf",
                  support = common_support,
                  lowerdate = "2012-01-01", upperdate = "2013-01-01")
min_pre <- prep_data(Beijing, prep = "support", lowerdate = "2010-01-01", upperdate = "2011-01-01")
min_middle <- prep_data(Beijing, prep = "support", lowerdate = "2011-01-01", upperdate = "2012-01-01")
min_post <- prep_data(Beijing, prep = "support", lowerdate = "2012-01-01", upperdate = "2013-01-01")

bandwidth_seq <- seq(0, 40000, 1000)

### Cost Matrix: 1(abs(x_1 - x_0) > d)

lhs <- rep(NA_real_, length(bandwidth_seq))
rhs <- rep(NA_real_, length(bandwidth_seq))

pb <- txtProgressBar(min = 0, max = length(bandwidth_seq), initial = 0)
for (i in seq_along(bandwidth_seq)) {
  setTxtProgressBar(pb, i)
  bw <- bandwidth_seq[i]
  lhs[i] <- get_OTcost(pre, post, bandwidth = bw)$prop_bribe
  rhs[i] <- get_OTcost(pre, middle, bandwidth = bw)$prop_bribe + get_OTcost(middle, post, bandwidth = bw)$prop_bribe
}

sum(lhs <= rhs) / length(bandwidth_seq) # = 1

### L-p Wasserstein (without d) with p = 2

bandwidth_seq <- c(0)

lhs <- rep(NA_real_, length(bandwidth_seq))
rhs <- rep(NA_real_, length(bandwidth_seq))

Lpcost <- function(min_pre_support, min_post_support, p) {
  costm <- matrix(NA_real_, nrow = length(min_pre_support), ncol = length(min_post_support))
  for (i in seq_along(min_pre_support)) {
      costm[i, ] <- abs(min_pre_support[i] - min_post_support)^p
  }
  costm
}

common_cost <- Lpcost(common_support, common_support, p = 2)
pre_post <- Lpcost(min_pre, min_post, p = 2)
pre_middle <- Lpcost(min_pre, min_middle, p = 2)
middle_post <- Lpcost(min_middle, min_post, p = 2)

pb <- txtProgressBar(min = 0, max = length(bandwidth_seq), initial = 0)
for (i in seq_along(bandwidth_seq)) {
  setTxtProgressBar(pb, i)
  bw <- bandwidth_seq[i]
  lhs[i] <- (get_OTcost(pre, post, bandwidth = bw, costmat = common_cost, costmat_ref = pre_post)$prop_bribe)^(1/2)
  rhs[i] <- (get_OTcost(pre, middle, bandwidth = bw, costmat = common_cost, costmat_ref = pre_middle)$prop_bribe)^(1/2) + 
    (get_OTcost(middle, post, bandwidth = bw, costmat = common_cost, costmat_ref = middle_post)$prop_bribe)^(1/2)
}

sum(lhs <= rhs) / length(bandwidth_seq) # = 1
