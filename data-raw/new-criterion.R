### Header ---------------------------
###
### Title: new-criterion.R
###
### Description: Implement new criterion for choosing bandwidth for before-and-after estimator
###
### Author: Omkar A. Katta
###
### Notes: See email exchange with Guillaume (Oct 21, 23)
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
post <- prep_data(Beijing, prep = "pmf",
                  support = common_support,
                  lowerdate = "2012-01-01", upperdate = "2013-01-01")
pre_tilde <- data.frame(MSRP = pre$MSRP, count = rmultinom(1, sum(pre$count), pre$count))
post_tilde <- data.frame(MSRP = post$MSRP, count = rmultinom(1, sum(post$count), post$count))

# bandwidth_seq <- seq(0, 40000, 1000)
bw <- 0
epsilon <- 0.1

Lpcost <- function(min_pre_support, min_post_support, p) {
  costm <- matrix(NA_real_, nrow = length(min_pre_support), ncol = length(min_post_support))
  for (i in seq_along(min_pre_support)) {
      costm[i, ] <- abs(min_pre_support[i] - min_post_support)^p
  }
  costm
}

common_cost <- Lpcost(common_support, common_support, p = epsilon)
hat_OT <- transport(
  as.numeric(sum(post$count) / sum(pre$count) * pre$count),
  as.numeric(post$count),
  common_cost
)
tilde_OT <- transport(
  as.numeric(sum(post_tilde$count) / sum(pre_tilde$count) * pre_tilde$count),
  as.numeric(post_tilde$count),
  common_cost
)

pre_support <- unique(pre$MSRP[pre$count != 0 & !is.na(pre$MSRP)])
post_support <- unique(post$MSRP[post$count != 0 & !is.na(post$MSRP)])
pre_tilde_support <- unique(pre_tilde$MSRP[pre_tilde$count != 0 & !is.na(pre_tilde$MSRP)])
post_tilde_support <- unique(post_tilde$MSRP[post_tilde$count != 0 & !is.na(post_tilde$MSRP)])

hat_OT_revised <- hat_OT %>% 
  dplyr::rename(hat_mass = mass) %>% 
  dplyr::mutate(from = pre_support[from]) %>%
  dplyr::mutate(to = post_support[to])
tilde_OT_revised <- tilde_OT %>% 
  dplyr::rename(tilde_mass = mass) %>% 
  dplyr::mutate(from = pre_tilde_support[from]) %>%
  dplyr::mutate(to = post_tilde_support[to])

OT_combine <- dplyr::full_join(hat_OT_revised, tilde_OT_revised, by = c("from", "to"))
support <- unique(sort(c(OT_combine$from, OT_combine$to)))

OT_final <- OT_combine %>%
  dplyr::rowwise() %>%
  dplyr::mutate(from = which(support == from)) %>% 
  dplyr::mutate(to = which(support == to)) %>%
  dplyr::ungroup()

hat_gamma <- matrix(0, nrow = length(support), ncol = length(support))
tilde_gamma <- matrix(0, nrow = length(support), ncol = length(support))
for (i in seq_len(nrow(OT_final))) {
  temprow <- OT_final[i, ]
  hat_gamma[as.numeric(temprow["from"]), as.numeric(temprow["to"])] <- as.numeric(temprow["hat_mass"])
  tilde_gamma[as.numeric(temprow["from"]), as.numeric(temprow["to"])] <- as.numeric(temprow["tilde_mass"])
}
hat_gamma[is.na(hat_gamma)] <- 0
tilde_gamma[is.na(tilde_gamma)] <- 0

gamma <- tilde_gamma - hat_gamma
cost <- build_costmatrix(support, bandwidth = bw)
OT <- sum(cost * gamma)
