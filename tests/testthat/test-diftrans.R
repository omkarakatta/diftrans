context("Diftrans Example")

library(dplyr)
library(magrittr)

set.seed(1)
support_min <- 1
support_max <- 10
support <- seq(support_min, support_max, 1)
pre_treated <- data.frame(x = sample(support, 100, replace = T)) %>%
  group_by(x) %>%
  count() %>%
  ungroup() %>%
  rename(count = n)

shift <- 5
for (i in 1:shift) {
  pre_treated[pre_treated$x == support_max-i+1, "count"] <- 0
}

post_treated <- data.frame(x = support,
                           count = lag(pre_treated$count, shift)) %>%
  tidyr::replace_na(list(count = 0))

out <- diftrans(pre_main = pre_treated, post_main = post_treated,
                bandwidth_seq = seq(0, 10, 1),
                estimator = "tc", var = x, count = count,
                sims_bandwidth_selection = 3,
                sims_subsampling = 3,
                subsample_pre_main_size = 5,
                subsample_post_main_size = 5,
                subsample_pre_control_size = 5,
                subsample_post_control_size = 5,
                seed = 1,
                conservative = TRUE)

tc <- diftrans(pre_main = pre_treated, post_main = post_treated,
                  estimator = "tc", var = x, count = count,
                  suppress_progress_bar = TRUE,
                  bandwidth = 0)

test_that("diftrans works", {
  expect_equal(tc$main, 1)
})
