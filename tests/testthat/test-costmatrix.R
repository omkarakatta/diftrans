context("Cost Matrix")

seed <- as.numeric(Sys.time(), "%d%H%M") # seed is day, hour, and minute
set.seed(seed)

support <- seq(1, sample(10:30, 1), by = 1)
full <- matrix(1, nrow = length(support), ncol = length(support))
diag <- diag(1, nrow = length(support), ncol = length(support))
answer <- full - diag

test_that("bandwidth = 0 is okay.", {
  expect_equal(build_costmatrix(support, bandwidth = 0),
               answer)
  expect_equal(build_costmatrix2(support, support, bandwidth = 0),
               build_costmatrix(support, bandwidth = 0))
})

support <- seq(1, 6, 1)
bw <- 2
answer <- matrix(c(0, 0, 0, 1, 1, 1,
                   0, 0, 0, 0, 1, 1,
                   0, 0, 0, 0, 0, 1,
                   1, 0, 0, 0, 0, 0,
                   1, 1, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0),
                 byrow = T, nrow = length(support), ncol = length(support))
test_that("bandwidth \neq 0 is okay.", {
  expect_equal(build_costmatrix(support, bandwidth = bw),
               answer)
  expect_equal(build_costmatrix2(support, support, bandwidth = bw),
               answer)
})

n <- sample(5:40, 2, replace = TRUE)
bw <- sample(1:min(n), 1)
support <- sort(runif(min(n), 0, 30))

test_that("cost matrix with common support is symmetric.", {
  expect_equal(build_costmatrix(support, bandwidth = bw),
               t(build_costmatrix(support, bandwidth = bw)))
  expect_equal(build_costmatrix2(support, support, bandwidth = bw),
               t(build_costmatrix2(support, support, bandwidth = bw)))
})

test_that("cost matrix has diagonal of 0.", {
  expect_equal(diag(build_costmatrix(support, bandwidth = bw)),
               rep(0, length(support)))
  expect_equal(diag(build_costmatrix2(support, support, bandwidth = bw)),
               rep(0, length(support)))
})

support2 <- sort(c(sample(support, min(n)/2), runif(max(n)-min(n)/2, 0, 30)))

test_that("dimensions of cost matrix are correct.", {
  expect_equal(dim(build_costmatrix(support, bandwidth = bw)),
               c(length(support), length(support)))
  expect_equal(dim(build_costmatrix2(support, support2, bandwidth = bw)),
               c(length(support), length(support2)))
})
