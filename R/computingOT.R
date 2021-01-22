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

  #~ obtain supports
  pre_support <- pre_df[[rlang::ensym(var)]]
  post_support <- post_df[[rlang::ensym(var)]]

  #~ ensure the supports are unique and the same in pre- and post-distributions
  if (length(pre_support)
      !=
      length(unique(pre_support[!is.na(pre_support)]))) {
    stop("`pre_df` should not have repeated/invalid values in `var` column.")
  }
  if (length(post_support)
      !=
      length(unique(post_support[!is.na(post_support)]))) {
    stop("`post_df` should not have repeated/invalid values in `var` column.")
  }
  if (!identical(pre_support, post_support)) {
    stop("`pre_df` and `post_df` need to have the same `var` column.")
  }

  support <- pre_support

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

### Compute Results ---------------------------

#' Obtain Transport Costs and Differences-in-Transports Estimator
#'
#' Given the pre and post probability mass functions as well as a vector of
#' bandwidths, this function returns the associated transport costs.
#' If another set of pre and post probability mass functions are given for the
#' control group, then the differences-in-transports estimator is returned.
#'
#' The \code{pre_main}, \code{post_main}, \code{pre_control}, and
#' \code{post_control} variables are all probability mass functions.
#' That is, they are a tibble with two columns:
#' \itemize{
#'   \item column 1 contains the full support of \code{var}, and
#'   \item column 2 contains the corresponding mass of each value
#'          in the support.
#' }
#' Since column 1 contains the full support of \code{var} and all these
#' distributions are of \code{var}, column 1 must be the same for all
#' distributions.
#'
#' The cost matrices specified by \code{costm} should use a common support of
#' the respective distributions.
#' However, \code{costm_ref} matrices should use the minimal support of the
#' respective pre and post distributions.
#'
#' @param pre_main probability mass function (see "Details") for \code{var} of the
#'     treated group before treatment occurs
#' @param post_main probability mass function (see "Details") for \code{var} of the
#'     treated group after treatment occurs
#' @param pre_control probability mass function (see "Details") for \code{var} of the
#'     control group before treatment occurs; only required for the computing the
#'     differences-in-transports estimator
#' @param post_control probability mass function (see "Details") for \code{var} of the
#'     treated group after treatment occurs; only required for the computing the
#'     differences-in-transports estimator
#' @param var the title of the first column of \code{pre_main}, \code{post_main},
#'     \code{pre_control}, and \code{post_control}; default is \code{MSRP}
#'     (see Daljord et al. (2021))
#' @param count the title of the second column of \code{pre_main}, \code{post_main},
#'     \code{pre_control}, and \code{post_control}; default is \code{count}
#'     (see Daljord et al. (2021))
#' @param bandwidth_seq a vector of bandwidth values to try; default is \code{seq(0, 40000, 1000)}
#' @param estimator a string that takes on the value of "dit" for
#'     differences-in-transports estimator or "tc" for the transport cost;
#'     if \code{pre_control} and \code{post_control} are specified, default is "dit";
#'     otherwise, default is "tc"
#' @param conservative if \code{TRUE}, then the bandwidth sequence will be
#'     multiplied by 2 to provide a conservative estimate of the transport costs/
#'     difference-in-transports estimator; default is \code{FALSE}
#' @param quietly if \code{TRUE}, some results and will be suppressed from printing; default is \code{FALSE}
#' @param suppress_progress_bar if \code{TRUE}, the progress bar will be suppressed; default is \code{FALSE}
#' @param save_dit if \code{TRUE}, the differences-in-transports estimator as
#'     well as the associated bandwidth will be returned
#' @param costm_main if \code{NULL}, the cost matrix with common support will be such that if the transport 
#'     distance is greater than what is specified in \code{bandwidth_seq}, cost is 1 and 0 otherwise.
#' @param costm_ref_main if \code{NULL}, the cost matrix referenced by \code{transport::transport} will be 
#'     using the minimal support of main distributions
#' @param costm_control if \code{NULL}, the cost matrix with common support will be such that if the transport 
#'     distance is greater than what is specified in \code{bandwidth_seq}, cost is 1 and 0 otherwise.
#' @param costm_ref_control if \code{NULL}, the cost matrix referenced by \code{transport::transport} will be 
#'     using the minimal support of control distributions
#'
#' @return a data.frame with the transport costs associated with each value of \code{bandwidth_seq}.
#' \itemize{
#'   \item \code{bandwidth}: same as \code{bandwidth_seq}
#'   \item \code{main}: transport costs associated with main distributions
#'   \item \code{main2d}: transport costs associated with main distributions using twice the bandwidth;
#'                        appears only if \code{conservative = TRUE}
#'   \item \code{control}: transport costs associated with the control distributions;
#'                        appears only if \code{pre_control} and \code{post_control}
#'                        are specified
#'   \item \code{diff}: \code{main - control}
#'   \item \code{diff2d}: \code{main2d - control}
#' }
#'
#' If \code{save_dit = TRUE}, then a list is returned, with the first element
#' (labeled \code{out}) being the data.frame described above.
#' The second element (labeled \code{dit}) is the differences-in-transports
#' estimator, and the third and final element (labeled \code{optimal_bandwidth})
#' is the bandwidth associated with the estimator.
#'
#' @export
#' @importFrom rlang ensym
#' @examples
#' # Find conservative transport cost of MSRP in Beijing between 2010 and 2011 using bandwidth = 0
#' # # step 1: find support
#' support_Beijing <- Beijing_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2012-01-01") %>%
#'   dplyr::select(MSRP) %>%
#'   dplyr::distinct() %>%
#'   dplyr::arrange(MSRP) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   unlist()
#' temp <- data.frame(MSRP = support_Beijing)
#' # # step 2: prepare probability mass functions
#' pre_Beijing <- Beijing_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2011-01-01") %>%
#'   dplyr::group_by(dplyr::across(c(MSRP))) %>%
#'   dplyr::summarise(count = sum(sales)) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   dplyr::left_join(temp, .) %>%
#'   dplyr::select(MSRP, count) %>%
#'   tidyr::replace_na(list(count = 0)) %>%
#'   tibble::as_tibble()
#' post_Beijing <- Beijing_sample %>%
#'   dplyr::filter(ym >= as.Date("2011-01-01") & ym < "2012-01-01") %>%
#'   dplyr::group_by(dplyr::across(c(MSRP))) %>%
#'   dplyr::summarise(count = sum(sales)) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   dplyr::left_join(temp, .) %>%
#'   dplyr::select(MSRP, count) %>%
#'   tidyr::replace_na(list(count = 0)) %>%
#'   tibble::as_tibble()
#' # # step 3: compute results
#' tc <- diftrans(pre_Beijing, post_Beijing, conservative = TRUE, bandwidth = 0)
#' tc$main2d
#'
#' # Find transport cost of MSRP in Beijing between 2010 and 2011 using bandwidth = 10000
#' # tc_10000 <- diftrans(pre_Beijing, post_Beijing, bandwidth = 10000)# tc_10000$main
#' # Find conservative differences-in-transport estimator using Tianjin as a control
#' # # step 1: find support
#' support_Tianjin <- Tianjin_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2012-01-01") %>%
#'   dplyr::select(MSRP) %>%
#'   dplyr::distinct() %>%
#'   dplyr::arrange(MSRP) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   unlist()
#' temp <- data.frame(MSRP = support_Tianjin)
#' # # step 2: prepare probability mass functions
#' pre_Tianjin <- Tianjin_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2011-01-01") %>%
#'   dplyr::group_by(dplyr::across(c(MSRP))) %>%
#'   dplyr::summarise(count = sum(sales)) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   dplyr::left_join(temp, .) %>%
#'   dplyr::select(MSRP, count) %>%
#'   tidyr::replace_na(list(count = 0)) %>%
#'   tibble::as_tibble()
#' post_Tianjin <- Tianjin_sample %>%
#'   dplyr::filter(ym >= as.Date("2011-01-01") & ym < "2012-01-01") %>%
#'   dplyr::group_by(dplyr::across(c(MSRP))) %>%
#'   dplyr::summarise(count = sum(sales)) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   dplyr::left_join(temp, .) %>%
#'   dplyr::select(MSRP, count) %>%
#'   tidyr::replace_na(list(count = 0)) %>%
#'   tibble::as_tibble()
#' # # step 3: compute results
#' dit <- diftrans(pre_Beijing, post_Beijing, pre_Tianjin, post_Tianjin,
#'                    conservative = TRUE, bandwidth = seq(0, 40000, 1000),
#'                    save_dit = TRUE)
#' dit$optimal_bandwidth
#' dit$dit
diftrans <- function(pre_main = NULL, post_main = NULL,
                     pre_control = NULL, post_control = NULL,
                     var = MSRP,
                     count = count,
                     bandwidth_seq = seq(0, 40000, 1000),
                     estimator = ifelse(!is.null(pre_control) & !is.null(post_control), "dit", "tc"),
                     conservative = F,
                     quietly = F,
                     suppress_progress_bar = F,
                     save_dit = F,
                     costm_main = NULL, costm_ref_main = NULL,
                     costm_control = NULL, costm_ref_control = NULL){

  # error checking
  if (is.null(pre_main) | is.null(post_main)){
    message("`pre_main` and/or `post_main` is mising.")
  }
  estimator <- tolower(estimator)
  if (estimator == "tc"){
    if (!is.null(pre_control) | !is.null(post_control)){
      message("`pre_control` and/or `post_control` will be ignored.")
    }
    est_message <- "Computing Transport Costs..."
    est <- "tc"
    if (!suppress_progress_bar) message(est_message)
  } else if (estimator == "dit" | estimator == "differences-in-transports"){
    if (is.null(pre_control) | is.null(post_control)){
      message("`pre_control` and/or `post_control` is mising.")
    }
    est_message <- "Computing Differences-in-Transports Estimator..."
    est <- "dit"
    if (!suppress_progress_bar) message(est_message)
  } else {
    stop("Invalid estimator. Choose 'tc' or 'dit' or double check inputs.")
  }

  if (conservative & !quietly){
    message("Note: you are using `conservative = T`.")
  }

  if (est != "dit" & save_dit){
    warning("The differences-in-transports estimator is not being computed so `save_dit = TRUE` is being ignored.")
  }

  # initialization
  main_prop <- rep(NA_real_, length(bandwidth_seq))
  if (conservative) maincons_prop <- rep(NA_real_, length(bandwidth_seq))
  if (est == "dit") control_prop <- rep(NA_real_, length(bandwidth_seq))

  # computation
  if (!suppress_progress_bar) pb <- utils::txtProgressBar(min = 0, max = length(bandwidth_seq), initial = 0)
  for (i in seq_along(bandwidth_seq)){
    if (!suppress_progress_bar) utils::setTxtProgressBar(pb, i)

    bandwidth <- bandwidth_seq[i]
    main_cost <- get_OTcost(pre_main,
                            post_main,
                            bandwidth = bandwidth,
                            var = !!rlang::ensym(var),
                            count = !!rlang::ensym(count),
                            costmat = costm_main,
                            costmat_ref = costm_ref_main)
    if (conservative) maincons_cost <- get_OTcost(pre_main,
                                                  post_main,
                                                  bandwidth = 2*bandwidth,
                                                  var = !!rlang::ensym(var),
                                                  count = !!rlang::ensym(count),
                                                  costmat = costm_main,
                                                  costmat_ref = costm_ref_main)
    if (est == "dit") control_cost <- get_OTcost(pre_control,
                                                 post_control,
                                                 bandwidth = bandwidth,
                                                 var = !!rlang::ensym(var),
                                                 count = !!rlang::ensym(count),
                                                 costmat = costm_control,
                                                 costmat_ref = costm_ref_control)

    main_prop[i] <- main_cost$prop_cost
    if (conservative) maincons_prop[i] <- maincons_cost$prop_cost
    if (est == "dit") control_prop[i] <- control_cost$prop_cost
  }

  if (!suppress_progress_bar) cat("\n")

  # compile results
  if (est == "dit"){
    diffprop <- main_prop - control_prop

    if (conservative){
      diffprop2d <- maincons_prop - control_prop
      out <- data.frame(bandwidth = bandwidth_seq,
                        main = main_prop,
                        main2d = maincons_prop,
                        control = control_prop,
                        diff = diffprop,
                        diff2d = diffprop2d)
      whichmax <- which.max(diffprop2d)
      dit <- diffprop2d[whichmax]
      dstar <- bandwidth_seq[whichmax]
      if (!quietly) message(paste("The conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
    } else {
      out <- data.frame(bandwidth = bandwidth_seq,
                        main = main_prop,
                        control = control_prop,
                        diff = diffprop)
      whichmax <- which.max(diffprop)
      dit <- diffprop[whichmax]
      dstar <- bandwidth_seq[whichmax]
      if (!quietly) message(paste("The non-conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
    }
    if (save_dit) out <- list(out = out, dit = dit, optimal_bandwidth = dstar)
  }

  if (est == "tc"){
    out <- data.frame(bandwidth = bandwidth_seq,
                      main = main_prop)
    if (conservative) out <- cbind(out, main2d = maincons_prop)
    if (!quietly) message(paste("The transport cost for the specified bandwidths have been computed."))
  }

  return(invisible(out))
}
