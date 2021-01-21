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
for (i in 1:shift){
  pre_treated[pre_treated$x == support_max-i+1, "count"] <- 0
}

post_treated <- data.frame(x = support,
                           count = lag(pre_treated$count, shift)) %>%
  tidyr::replace_na(list(count = 0))

tc <- diftrans(pre_main = pre_treated, post_main = post_treated,
                  estimator = "tc", var = x, count = count,
                  bandwidth = 0)

#~ tc

test_that("diftrans works", {
  expect_equal(tc$main, 1)
})


support <- c(1, 2, 10)
#~ example 1: no overlap
#~ bandwidth = 0 => 100% cost
#~ bandwidth = 3 => 0% cost
disjoint_before <- data.frame(x = support, mass = c(8, 0, 0))
disjoint_after <- data.frame(x = support, mass = c(0, 8, 0))
overlap_before <- data.frame(x = support, mass = c(6, 2, 0))
overlap_after <- data.frame(x = support, mass = c(3, 1, 0))
#~ example 2: complete overlap
mix_before <- data.frame(x = support, mass = c(6, 2, 0))
mix_after <- data.frame(x = support, mass = c(1, 3, 0))

out <- diftrans(disjoint_before, disjoint_after, var = x, count = mass, bandwidth_seq = c(0), suppress_progress_bar = T)
out <- diftrans(overlap_before, overlap_after, var = x, count = mass, bandwidth_seq = c(0), suppress_progress_bar = T)
out

\dontrun{
support <- c(1, 2, 10)
#~ example 1: no overlap
#~ bandwidth = 0 => 100% cost
#~ bandwidth = 3 => 0% cost
disjoint_before <- data.frame(x = support, mass = c(8, 0, 0))
disjoint_after <- data.frame(x = support, mass = c(0, 8, 0))
get_OTcost(disjoint_before,
           disjoint_after,
           var = x,
           count = mass)
get_OTcost(disjoint_before,
           disjoint_after,
           bandwidth = 3,
           var = x,
           count = mass)
overlap_before <- data.frame(x = support, mass = c(6, 2, 0))
overlap_after <- data.frame(x = support, mass = c(3, 1, 0))
#~ example 2: complete overlap
get_OTcost(overlap_before,
           overlap_after,
           var = x,
           count = mass)
get_OTcost(overlap_before,
           overlap_after,
           bandwidth = 3,
           var = x,
           count = mass)
mix_before <- data.frame(x = support, mass = c(6, 2, 0))
mix_after <- data.frame(x = support, mass = c(1, 3, 0))
}
