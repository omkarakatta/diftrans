context("Auxiliary Function Checks")

pre_df <- data.frame(support = c(1, 2, 3), count = c(1, 2, 3))
post_df <- data.frame(support = c(1, 2, 4), count = c(1, 2, 3))

test_that("make sure pre_df and post_df use common support", {
  expect_equal(check_support(pre_df, post_df, support)$status, 1)
})
