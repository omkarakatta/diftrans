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
for (i in 1:shift){
  pre_treated[pre_treated$x == support_max-i+1, "count"] <- 0
}

post_treated <- data.frame(x = support,
                           count = lag(pre_treated$count, shift)) %>%
  tidyr::replace_na(list(count = 0))

tc <- diftrans(pre_main = pre_treated, post_main = post_treated,
                  estimator = "tc", var = x,
                  bandwidth = 0)

tc

test_that("diftrans works", {
  expect_equal(tc$main, 1)
})
