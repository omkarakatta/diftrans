### Meta ---------------------------
###
### Title: diftrans.R
###
### Description: Main function to compute before-and-after and
###   differences-in-transports estimator
###
### Author: Omkar A. Katta
###
### ---------------------------
###
### Notes:
###
###

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
