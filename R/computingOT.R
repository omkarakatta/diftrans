### Header ---------------------------
###
### Title: OTfunctions.R
###
### Description: Functions used to construct cost matrix and compute optimal transport costs
###
### Author: Omkar A. Katta
###
### Notes:
###
###

### Cost Matrix ---------------------------

#' Create Cost Matrix
#'
#' Create cost matrix with common \code{support} for both pre- and
#' post-distributions.
#'
#' The \eqn{(i, j)} entries of the cost matrix are given by:
#' \deqn{c_d(x_i, x_j) = 1( | x_i - x_j | > d ),}
#' where \eqn{x_i} and \eqn{x_j} are the i-th and j-th entries in
#' \code{support}, \eqn{1(...)} is the indicator function, and
#' \eqn{d} is \code{bandwidth}.
#'
#' Note that this function has no internal checks.
#' The user should be careful to ensure that:
#' \enumerate{
#'  \item \code{bandwidth} is non-negative.
#'  \item \code{support} contains unique values.
#'}
#'
#' Instead of using the common support of our pre- and 
#' post-distributions, \code{build_costmatrix2} uses the support of
#' the pre-distribution and the support of the
#' post-distribution separately.
#'
#' Since this function uses the common support, the diagonals
#' of the cost matrix will refer to the cost of no movement.
#' For any choice of the \code{bandwidth}, the diagonals
#' will therefore be zero.
#' Another consequence of using the common support is that
#' cost matrix will be symmetric and square.
#'
#' @param support A vector of the common support of pre-
#'   and post-distributions
#' @param bandwidth A non-negative number to ignore small transfers;
#'   defaults to 0
#' @return a symmetric, square matrix of dimension \code{length(support)}
#'
#' @examples
#' \dontrun{
#' build_costmatrix(c(0, 1, 2), 1)
#'}
#'
#' @seealso \code{\link{build_costmatrix2}}
build_costmatrix <- function(support, bandwidth = 0) {
  costmatrix <- matrix(NA_real_, nrow = length(support), ncol = length(support))
  for (i in seq_along(support)) {
    dist <- abs(support[i] - support)
    dist <- ifelse(dist > bandwidth, 1, 0)
    costmatrix[i, ] <- dist
  }
  costmatrix
}

#' Create Cost Matrix with Minimal Supports
#'
#' Create cost matrix with \code{support_pre} for pre-distribution and
#' \code{support_post} for post-distribution.
#'
#' The \eqn{(i, j)} entries of the cost matrix are given by:
#' \deqn{c_d(x_i, y_j) = 1( | x_i - y_j | > d ),}
#' where \eqn{x_i} is the ith entry of \code{support_pre},
#' \eqn{y_j} is the jth entry of \code{support_post},
#' \eqn{1(...)} is the indicator function, and
#' \eqn{d} is \code{bandwidth}.
#'
#' Note that this function has no internal checks.
#' The user should be careful to ensure that:
#' \enumerate{
#'  \item \code{bandwidth} is non-negative.
#'  \item \code{support_pre} contains unique values.
#'  \item \code{support_post} contains unique values.
#'}
#'
#' Instead of using the common support of our pre- and
#' post-distributions as in \code{build_costmatrix}, this function
#' uses the support of the pre- and post-distributions separately.
#' Resultantly, this function need not return a square, symmetric
#' matrix whose diagonals are zero.
#'
#' @param support_pre A vector of the support of the pre-distribution
#' @param support_post A vector of the support of the post-distribution
#' @param bandwidth A non-negative number to ignore small transfers;
#'   defaults to 0
#' @return a matrix of dimension \code{length(support_pre)} by
#'   '\code{length(support_post)}
#'
#' @examples
#' \dontrun{
#' build_costmatrix2(c(0, 1, 2), c(1, 2, 3), 1)
#' }
#'
#' @seealso \code{\link{build_costmatrix}}
build_costmatrix2 <- function(support_pre, support_post, bandwidth = 0) {
  costmatrix <- matrix(NA_real_,
                       nrow = length(support_pre),
                       ncol = length(support_post))
  for (i in seq_along(support_pre)) {
    dist <- abs(support_pre[i] - support_post)
    dist <- ifelse(dist > bandwidth, 1, 0)
    costmatrix[i, ] <- dist
  }
  costmatrix
}

### Compute Transport Cost ---------------------------

#' Compute Optimal Transport Cost
#'
#' Compute the percentage of mass that has moved more than
#' \code{bandwidth} units away on the support of two discrete
#' distributions, \code{pre_df} and \code{post_df}.
#'
#' The pre- and post-distributions given by \code{pre_df}
#' and \code{post_df} need to be \code{data.frame} objects of
#' two columns. The first column whose title should be given by
#' \code{var} contains the common support of the two distributions.
#' In other words, the values in \code{var} in each \code{pre_df}
#' and \code{post_df} need to be unique and the same across the two
#' distributions.
#' The second column named \code{count} provides the mass on the
#' respective value of the support.
#' If the sum of \code{count} in each \code{pre_df} and \code{post_df},
#' then \code{pre_df} and \code{post_df} are probability mass functions.
#'
#' The \code{bandwidth} argument should be a non-negative number.
#' If mass is transferred less than \code{bandwidth}, we ignore these transfers.
#' Otherwise, we place equal weight on the transfers.
#' Resultantly, the output of this function is the percentage of mass
#' that has been tranferred more than \code{bandwidth} units between
#' \code{pre_df} and \code{post_df}.
#'
#' Instead of the default cost function that uses \code{bandwidth},
#' \code{costmat}, and \code{costmat_ref} can be used to specify the
#' cost matrix of choice.
#' The first matrix is fed into \code{\link[transport]{transport}} function from
#' the \code{transport} package and may use the common support of
#' \code{pre_df} and \code{post_df}.
#' The second matrix \code{costmat_ref} is used to interpret the output
#' of the \code{\link[transport]{transport}} function. It uses the
#' the minimal support of \code{pre_df} along the rows and the
#' minimal support of \code{post_df} along the columns.
#'
#' This function does minimal error-checking, but good practices include:
#' \enumerate{
#'   \item non-degenerate pre- and post-distributions
#'   \item non-negative values for \code{bandwidth}
#'   \item if not NULL, \code{costmat} should be a square matrix
#'      whose dimension is the length of the common support
#'   \item if not NULL, \code{costmat_ref} should be a matrix
#'      whose dimension is the length of the support of \code{pre_df}
#'      by the length of the support of \code{post_df}
#' }
#'
#' @param pre_df A two-column \code{data.frame} describing the pre-distribution;
#' @param post_df A two-column \code{data.frame} describing the
#'   post-distribution;
#' @param bandwidth A non-negative number to ignore small transfers;
#'   defaults to 0
#' @param var The title of the column of support values in \code{pre_df}
#'   and \code{post_df}; defaults to \code{MSRP}
#' @param count The title of the column of masses in \code{pre_df}
#'   and \code{post_df}; defaults to \code{count}
#' @param costmat The cost matrix to be fed to
#'   \code{\link[transport]{transport}}
#' @param costmat_ref The cost matrix that is used to reference output of
#'   code{\link[transport]{transport}}
#'
#' @return A list of the total mass transferred (\code{tot_cost}),
#'   the proportion of mass transferred (\code{prop_cost}), and the
#'   \code{bandwidth}
#'
#' @importFrom transport transport
#' @importFrom rlang ensym
#'
#' @examples
#' \dontrun{
#' support <- c(1, 2, 10)
#' #~ Example 1: complete overlap => 0% transport cost for any bandwidth
#' overlap_before <- data.frame(x = support, mass = c(6, 2, 0))
#' overlap_after <- data.frame(x = support, mass = c(3, 1, 0))
#' get_OTcost(overlap_before, overlap_after,
#'            var = x, count = mass)
#' get_OTcost(overlap_before, overlap_after,
#'            var = x, count = mass, bandwidth = 2)
#' #~ Example 2: illustrative example in Daljord et al. (2021)
#' mix_before <- data.frame(x = support, mass = c(6, 2, 0))
#' mix_after <- data.frame(x = support, mass = c(1, 3, 0))
#' get_OTcost(mix_before, mix_after, var = x, count = mass)
#' }
#'
get_OTcost <- function(pre_df,
                       post_df,
                       bandwidth = 0,
                       var = MSRP,
                       count = count,
                       costmat = NULL,
                       costmat_ref = NULL) {

  #~ ensure the supports are unique and the same in pre- and post-distributions
  check <- check_support(pre_df, post_df, var = !!rlang::ensym(var))
  if (check$status == 1) {
    stop(check$message)
  } else {
    support <- check$support
  }

  #~ obtain counts
  pre <- pre_df[[rlang::ensym(count)]]
  post <- post_df[[rlang::ensym(count)]]

  #~ obtain cost matrix with common support
  if (is.null(costmat)) {
    costm <- build_costmatrix(support, bandwidth)
  } else {
    costm <- costmat
  }

  #~ compute and normalize cost
  a <- as.numeric(sum(post) / sum(pre) * pre)
  b <- as.numeric(post)
  OT <- transport::transport(a, b, costm)

  if (is.null(costmat_ref)) {
    #~ construct minimal support for pre-distribution
    support_pre <- pre_df %>%
      dplyr::filter({{count}} != 0) %>%
      dplyr::select({{var}}) %>%
      dplyr::distinct({{var}}) %>%
      dplyr::arrange({{var}}) %>%
      dplyr::filter(!is.na({{var}})) %>%
      unlist()
    #~ construct minimal support for post-distribution
    support_post <- post_df %>%
      dplyr::filter({{count}} != 0) %>%
      dplyr::select({{var}}) %>%
      dplyr::distinct({{var}}) %>%
      dplyr::arrange({{var}}) %>%
      dplyr::filter(!is.na({{var}})) %>%
      unlist()
    #~ construct minimal cost matrix
    costm_ref <- build_costmatrix2(support_pre, support_post, bandwidth)
  } else {
    costm_ref <- costmat_ref
  }

  temp <- as.data.frame(OT) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cost = costm_ref[from, to]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cost = mass * cost)

  tot_cost <- sum(temp$cost)
  prop_cost <- tot_cost / sum(post)
  list("tot_cost" = tot_cost,
       "prop_cost" = prop_cost,
       "bandwidth" = bandwidth)
}


