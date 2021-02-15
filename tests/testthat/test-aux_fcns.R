context("Auxiliary Function Checks")

pre_df <- data.frame(support = c(1, 2, 3), count = c(1, 2, 3))
post_df <- data.frame(support = c(1, 2, 4), count = c(1, 2, 3))

test_that("make sure pre_df and post_df use common support", {
  expect_equal(check_support(pre_df, post_df, support)$status, 1)
})

dist <- c(rep(1, 99), 2) # 99 ones and 1 two
subsample <- organize_subsamples(dist, 50, c(1, 2, 3))
expect_equal(subsample["3"], 0)
expect_equal(subsample["1"] > subsample["2"], TRUE)
