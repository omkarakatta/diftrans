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
