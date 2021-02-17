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
#' @importFrom dplyr lag
#' @importFrom dplyr lead
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

### send_note ---------------------------

#' Send note to console
#'
#' Send a message or warning if \code{quietly} is FALSE
#'
#' The intended values of \code{fcn} are \code{\link[base]{message}} and
#' \code{\link[base]{warning}}.
#' If \code{quietly} is FALSE, then send the message to the console according
#' to \code{fcn}. Otherwise, do not send anything to the console.
#'
#' @param note A string that will be sent to the console
#' @param quietly If FALSE, message will be sent to console
#' @param fcn A function, e.g., \code{\link[base]{message}}
#'  or \code{\link[base]{warning}}
#'
#' @return A note in the console if \code{quietly} is FALSE
#'
#' @examples
#' \dontrun{
#' send_note("This is a warning.", FALSE, warning)
#' }
#'
#' @family debugging
send_note <- function(note, quietly, fcn) {
  if (!quietly) fcn(note)
}


### collect_msg ---------------------------

#' Conditionally add object to list
#'
#' Add an entry to a list under a specified condition
#'
#' Before this function, error messages would be sent one at a time.
#' Resultantly, users would spend time in a cycle of: run code, debug code,
#' and repeat. However, it would be more efficient if the user received all
#' possible error messages, rather than just the first error message, and then
#' start debugging. This would ease the debugging process and perhaps later
#' errors would provide insight into fixing the current error.
#'
#' This function was created with the intention of collecting error messages
#' in a list, and then printing all the error messages at the end.
#' The hope was that users can debug all at one go, rather than debug
#' incrementally.
#'
#' The goal is to pass this list to \code{stop}, \code{warning}, etc.
#' To print each message on its own line, set \code{newline} to TRUE
#'
#' \code{msg_list} must be a list; \code{new_msg} must be a string; and
#' \code{dontadd} and \code{newline} must be a boolean.
#'
#' @param msg_list A list of messages
#' @param new_msg A new message to add to \code{msg_list}
#' @param dontadd If FALSE, then add \code{new_msg} to \code{msg_list}; else,
#'  \code{msg_list} remains unchanged
#' @param newline If TRUE, then add a new line to \code{new_msg} before
#'  appending to \code{msg_list}
#'
#' @return If \code{add} is TRUE, then the result will be all the messages in
#'  \code{msg_list}. Else, the function returns \code{msg_list}.
#'
#' @family debugging
#'
#' @seealso \code{\link{send_note}}, \code{\link{preliminaries}}
collect_msg <- function(msg_list, new_msg, dontadd, newline = TRUE) {
  if (!dontadd) {
    if (newline) new_msg <- paste0("\n", new_msg)
    msg_list[[length(msg_list) + 1]] <- new_msg
  }
  msg_list
}

### preliminaries ---------------------------

#~ error checks
#~ get estimator and counts
preliminaries <- function(pre_main,
                          post_main,
                          pre_control,
                          post_control,
                          var,
                          count,
                          bandwidth_vec,
                          minimum_bandwidth,
                          maximum_bandwidth,
                          estimator,
                          sims_bandwidth_selection,
                          precision,
                          sensitivity_lag,
                          sensitivity_lead,
                          sensitivity_accept,
                          sims_subsampling,
                          pre_main_subsample_size,
                          post_main_subsample_size,
                          pre_control_subsample_size,
                          post_control_subsample_size,
                          seed,
                          conservative,
                          quietly,
                          show_progress) {

# Preliminary Error Checks ----------

  msg_list <- list()
  msg <- "`pre_main` needs to be a data.frame object."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.data.frame(pre_main)
  )
  msg <- "`post_main` needs to be a data.frame object."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.data.frame(pre_main)
  )
  msg <- "`bandwidth_vec` cannot be an empty vector."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(bandwidth_vec) > 0
  )
  msg <- "Bandwidths in `bandwidth_vec` need to be non-negative numbers."
  msg_list <- collect_msg(
    msg_list,
    msg,
    all(is.numeric(bandwidth_vec))
    & all(bandwidth_vec >= 0)
  )
  msg <- "`minimum_bandwidth` must be numbers."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.numeric(minimum_bandwidth)
  )
  msg <- "`maximum_bandwidth` must be numbers."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.numeric(maximum_bandwidth)
  )
  msg <- "`sims_bandwidth_selection` needs to be a non-negative integer."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(sims_bandwidth_selection) == 1
    & is.numeric(sims_bandwidth_selection)
    && round(sims_bandwidth_selection) == sims_bandwidth_selection
    & sims_bandwidth_selection >= 0
  )
  msg <- "`sims_subsampling` needs to be a non-negative integer."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(sims_subsampling) == 1
    & is.numeric(sims_subsampling)
    && round(sims_subsampling) == sims_subsampling
    & sims_subsampling >= 0
  )
  msg <- "`precision` needs to be a non-negative number"
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(precision) == 1
    & is.numeric(precision)
    & precision >= 0
  )
  msg <- "`sensitivity_lag` needs to be a non-negative integer."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(sensitivity_lag) == 1
    & is.numeric(sensitivity_lag)
    && round(sensitivity_lag) == sensitivity_lag
    & sensitivity_lag >= 0
  )
  msg <- "`sensitivity_lead` needs to be a non-negative integer."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(sensitivity_lead) == 1
    & is.numeric(sensitivity_lead)
    && round(sensitivity_lead) == sensitivity_lead
    & sensitivity_lead >= 0
  )
  msg <- "`sensitivity_lag` needs to be a non-negative integer."
  msg_list <- collect_msg(
    msg_list,
    msg,
    length(sensitivity_lag) == 1
    & is.numeric(sensitivity_lag)
    && round(sensitivity_lag) == sensitivity_lag
    & sensitivity_lag >= 0
  )
  msg <- "`seed` is invalid."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.numeric(seed)
  )
  msg <- "`conservative` needs to be TRUE or FALSE."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.logical(conservative)
  )
  msg <- "`quietly` needs to be TRUE or FALSE."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.logical(quietly)
  )
  msg <- "`show_progress` needs to be TRUE or FALSE."
  msg_list <- collect_msg(
    msg_list,
    msg,
    is.logical(show_progress)
  )

  if (length(msg_list) > 0) {
    stop(msg_list)
  }

# Get Estimator ----------

  out <- list()

  msg_list <- collect_msg(
    msg_list,
    "`var` is not in `pre_main`.",
    any(rlang::as_name(rlang::enquo(var)) %in% names(pre_main))
  )
  msg_list <- collect_msg(
    msg_list,
    "`count` is not in `pre_main`.",
    any(rlang::as_name(rlang::enquo(count)) %in% names(pre_main))
  )
  msg_list <- collect_msg(
    msg_list,
    "`var` is not in `post_main`.",
    any(rlang::as_name(rlang::enquo(var)) %in% names(post_main))
  )
  msg_list <- collect_msg(
    msg_list,
    "`count` is not in `post_main`.",
    any(rlang::as_name(rlang::enquo(count)) %in% names(post_main))
  )

  ba_messages <- c("tc", "ba", "before-and-after", "transport costs")
  dit_messages <- c("dit", "differences-in-transports", "diff-in-transports")
  estimator <- tolower(estimator)
  if (estimator %in% ba_messages) {
    est <- "ba"
    if (!is.null(pre_control)) {
      message("`pre_control` will be ignored for before-and-after estimation.")
    }
    if (!is.null(post_control)) {
      message("`post_control` will be ignored for before-and-after estimation.")
    }
    if (conservative) {
      message("Setting `conservative` to FALSE")
      conservative <- FALSE
    }
  } else if (estimator %in% dit_messages) {
    est <- "dit"
    if (is.null(pre_control)) {
      msg <- "`pre_control` is missing for differences-in-transports estimation."
      msg_list <- collect_msg(
        msg_list,
        msg,
        FALSE
      )
    } else {
      msg_list <- collect_msg(
        msg_list,
        "`var` is not in `pre_control`.",
        any(rlang::as_name(rlang::enquo(var)) %in% names(pre_control))
      )
      msg_list <- collect_msg(
        msg_list,
        "`count` is not in `pre_control`.",
        any(rlang::as_name(rlang::enquo(count)) %in% names(pre_control))
      )
    }
    if (is.null(post_control)) {
      msg <- "`post_control` is missing for differences-in-transports estimation."
      msg_list <- collect_msg(
        msg_list,
        msg,
        FALSE
      )
    } else {
      msg_list <- collect_msg(
        msg_list,
        "`var` is not in `post_control`.",
        any(rlang::as_name(rlang::enquo(var)) %in% names(post_control))
      )
      msg_list <- collect_msg(
        msg_list,
        "`count` is not in `post_control`.",
        any(rlang::as_name(rlang::enquo(count)) %in% names(post_control))
      )
    }
  }
  out$est <- est
  out$conservative <- conservative

  if (length(msg_list) > 0) {
    stop(msg_list)
  }

# Get Counts ----------

  pre_main_count <- pre_main[[rlang::ensym(count)]]
  post_main_count <- post_main[[rlang::ensym(count)]]
  pre_main_total <- sum(pre_main_count)
  post_main_total <- sum(post_main_count)

  if (est == "dit") {
    pre_control_count <- pre_control[[rlang::ensym(count)]]
    post_control_count <- post_control[[rlang::ensym(count)]]
    pre_control_total <- sum(pre_control_count)
    post_control_total <- sum(post_control_count)
  } else {
    pre_control_count <- 0
    post_control_count <- 0
    pre_control_total <- 0
    post_control_total <- 0
  }

  out$pre_main_count <- pre_main_count
  out$post_main_count <- post_main_count
  out$pre_control_count <- pre_control_count
  out$post_control_count <- post_control_count
  out$pre_main_total <- pre_main_total
  out$post_main_total <- post_main_total
  out$pre_control_total <- pre_control_total
  out$post_control_total <- post_control_total

# Check Subsample Sizes ----------

  if (sims_subsampling > 0) {
    msg <- "`pre_main_subsample_size` needs to be a positive integer less than the total count in `pre_main`."
    msg_list <- collect_msg(
      msg_list,
      msg,
      length(pre_main_subsample_size) == 1
      & is.numeric(pre_main_subsample_size)
      && round(pre_main_subsample_size) == pre_main_subsample_size
      & pre_main_subsample_size > 0
      & pre_main_subsample_size < pre_main_total
    )
    msg <- "`post_main_subsample_size` needs to be a positive integer less than the total count in `post_main`."
    msg_list <- collect_msg(
      msg_list,
      msg,
      length(post_main_subsample_size) == 1
      & is.numeric(post_main_subsample_size)
      && round(post_main_subsample_size) == post_main_subsample_size
      & post_main_subsample_size > 0
      & post_main_subsample_size < post_main_total
    )
    out$pre_main_subsample_size <- pre_main_subsample_size
    out$post_main_subsample_size <- post_main_subsample_size
    if (est == "ba") {
      if (!is.null(pre_control_subsample_size)) {
        msg <- "`pre_control_subsample_size` will be ignored for before-and-after estimation."
        send_note(msg, pre_control_subsample_size == 0, message)
      }
      if (!is.null(post_control_subsample_size)) {
        msg <- "`post_control_subsample_size` will be ignored for before-and-after estimation."
        send_note(msg, post_control_subsample_size == 0, message)
      }
      out$pre_control_subsample_size <- 0
      out$post_control_subsample_size <- 0
    } else if (est == "dit") {
      msg <- "`pre_control_subsample_size` needs to be a positive integer less than the total count in `pre_control`."
      msg_list <- collect_msg(
        msg_list,
        msg,
        length(pre_control_subsample_size) == 1
        & is.numeric(pre_control_subsample_size)
        && round(pre_control_subsample_size) == pre_control_subsample_size
        & pre_control_subsample_size > 0
        & pre_control_subsample_size < pre_control_total
      )
      out$pre_control_subsample_size <- pre_control_subsample_size
      msg <- "`post_control_subsample_size` needs to be a positive integer less than the total count in `post_control`."
      msg_list <- collect_msg(
        msg_list,
        msg,
        length(post_control_subsample_size) == 1
        & is.numeric(post_control_subsample_size)
        && round(post_control_subsample_size) == post_control_subsample_size
        & post_control_subsample_size > 0
        & post_control_subsample_size < post_control_total
      )
      out$pre_control_subsample_size <- pre_control_subsample_size
    }
  } else if (sims_subsampling == 0) {
    out$pre_main_subsample_size <- 0
    out$post_main_subsample_size <- 0
    out$pre_control_subsample_size <- 0
    out$post_control_subsample_size <- 0
  }

  if (length(msg_list) > 0) {
    stop(msg_list)
  } else {
    return(out)
  }
}

### whitespace ---------------------------

whitespace <- function(num) {
  paste(rep(" ", num), collapse = "")
}
