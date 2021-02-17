### Meta ---------------------------
###
### Title: aux_fcns.R
###
### Description: Auxiliary functions
###
### Author: Omkar A. Katta
###
### ---------------------------
###
### Notes:
###
###

### check_support ---------------------------

#~ check if support is the same
#~ if status is 1, then there is an error
#~ if status is 0, then there is no error; can retrieve support

#' Check Common Support
#'
#' Given two discrete distributions for \code{var}, determine whether
#' \code{var} has the same unique values.
#'
#' The distributions \code{pre_df} and \code{post_df} should be of
#' the following form:
#' \enumerate{
#'   \item A column called \code{var} for the support of \code{var}
#'   \item A column for the mass for each value of \code{var}
#' }
#' Since both distributions are for the same variable, the column
#' \code{var} must contain the same values.
#' This function essentially verifies that this is the case.
#'
#' @param pre_df A two-column \code{data.frame} describing the pre-distribution;
#' @param post_df A two-column \code{data.frame} describing the
#' post-distribution
#' @param var The title of the column of support values in \code{pre_df}
#'   and \code{post_df}
#'
#' @return A named list of three items:
#' \enumerate{
#'   \item \code{status}: if 1, then \code{pre_df} and \code{post_df}
#'      have invalid/different/repeated entries in \code{var} column
#'   \item \code{message}: describes why \code{status} is 1 or 0
#'   \item \code{support}: if \code{status} is 0, then this is the
#'      common support of \code{pre_df} and \code{post_df}
#' }
#'
#' @importFrom rlang ensym
#'
#' @examples
#' \dontrun{
#' pre_df <- data.frame(support = c(1, 2, 3), count = c(1, 2, 3))
#' post_df <- data.frame(support = c(1, 2, 4), count = c(1, 2, 3))
#' check_support(pre_df, post_df, support)
#' }
check_support <- function(pre_df, post_df, var) {

  #~ obtain supports
  pre_support <- pre_df[[rlang::ensym(var)]]
  post_support <- post_df[[rlang::ensym(var)]]

  #~ ensure the supports are unique and the same in pre- and post-distributions
  if (length(pre_support)
      !=
      length(unique(pre_support[!is.na(pre_support)]))) {
    message <- "`pre_df` shouldn't have repeated/invalid values in `var`."
    status <- 1
    support <- NULL
  } else if (length(post_support)
             !=
             length(unique(post_support[!is.na(post_support)]))) {
    message <- "`post_df` shouldn't have repeated/invalid values in `var`."
    status <- 1
    support <- NULL
  } else if (!identical(pre_support, post_support)) {
    message <- "`pre_df` and `post_df` need to have the same `var`."
    status <- 1
    support <- NULL
  } else {
    message <- "Everything looks good."
    status <- 0
    support <- pre_support
  }

  list(message = message, status = status, support = support)

}

### valid_bandwidths ---------------------------

#~ This function will help us provide a notion of stability for our bandwidth selection.
#~ It looks at the values before and after a particular bandwidth and tells us whether enough of these values are below our precision thresshold.
#~ it returns a boolean vector that tells us which bandwidths are such that among the `lag` previous bandwidths and `lead` following bandwidths, at least `accept` of them have costs that are no more than `precision`.
#' importFrom dplyr lag
#' importFrom dplyr lead
valid_bandwidths <- function(vec, lag, lead, accept, precision) {
  lags <- sapply(
    seq_len(lag),
    function(lag_index) {
      lag(vec, lag_index)
    }
  )
  leads <- sapply(
    seq_len(lead),
    function(lead_index){
      lead(vec, lead_index)
    }
  )
  laglead <- cbind(vec, lags, leads)
  no_more_than_precision <- laglead <= precision
  sum_across <- apply(no_more_than_precision, 1, sum, na.rm = TRUE)
  sum_across >= accept
}


### organize_subsamples ---------------------------
#~ create subsample distribution
#~ given a distribution, rewrite it as a pmf (opposite of uncount)
#~ support should contain unique, ordered values,
#~ but no check will be done to ensure this.
organize_subsamples <- function(dist, size, support) {
  subsample_dist <- sample(dist, size, replace = FALSE)
  counts <- sapply(
    seq_along(support),
    function(index) {
      sum(subsample_dist == support[[index]])
    }
  )
  names(counts) <- support
  counts
}

#~ count_across_columns <- function(dt, support) {
#~   t(sapply(
#~     seq_along(support),
#~     function(supp) {
#~       apply(dt, 2, function(x) sum(x == supp))
#~     }
#~   ))
#~ }
