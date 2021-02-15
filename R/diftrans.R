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
#' @param save_result if \code{TRUE}, the estimator as
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
#' If \code{save_result = TRUE}, then a list is returned, with the first element
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
#'                    save_result = TRUE)
#' dit$optimal_bandwidth
#' dit$dit
#~ if sims_bandwidth_selection is 0, skip bandwidth selection procedure
#~ sims_bandwidth_selection > 0 => choose appropriate d_star among bandwidth_seq!
diftrans <- function(pre_main = NULL, post_main = NULL,
                     pre_control = NULL, post_control = NULL,
                     var = MSRP,
                     count = count,
                     bandwidth_seq = seq(0, 40000, 1000),
                     minimum_bandwidth = 0, #~ TODO: document this, particularly for post/pre-trends
                     maximum_bandwidth = Inf, #~ TODO: document this
                     estimator = ifelse(!is.null(pre_control)
                                        &
                                        !is.null(post_control),
                                        "dit",
                                        "tc"),
                     sims_bandwidth_selection = 0,
                     precision = 0.0005,
                     sensitivity_lag = 5,
                     sensitivity_lead = 5,
                     sensitivity_accept = 5,
                     sims_subsampling = 0,
                     subsample_pre_main_size = NULL,
                     subsample_post_main_size = NULL,
                     subsample_pre_control_size = NULL,
                     subsample_post_control_size = NULL,
                     seed = 1,
                     conservative = FALSE, #~ TODO: only valid for dit
                     quietly = FALSE,
                     suppress_progress_bar = FALSE,
                     save_result = FALSE,
                     costm_main = NULL, costm_ref_main = NULL,
                     costm_control = NULL, costm_ref_control = NULL) {

  #~ initialize output
  out <- list()

  #~ error checking
  if (is.null(pre_main) || is.null(post_main)) {
    stop("`pre_main` and/or `post_main` is missing.")
  }
  if (round(sims_bandwidth_selection) != sims_bandwidth_selection
      || sims_bandwidth_selection < 0) {
    stop("`sims_bandwidth_selection` needs to be a non-negative integer.")
  }
  if (round(sims_subsampling) != sims_subsampling
      || sims_subsampling < 0) {
    stop("`sims_subsampling` needs to be a non-negative integer.")
  }
  if (sims_subsampling > 0) {
    if (round(subsample_pre_main_size) != subsample_pre_main_size
        || subsample_pre_main_size <= 0) {
      stop("`subsample_pre_main_size` needs to be positive integer.")
    }
    if (round(subsample_post_main_size) != subsample_post_main_size
        || subsample_post_main_size <= 0) {
      stop("`subsample_post_main_size` needs to be positive integer.")
    }
    if (round(subsample_pre_control_size) != subsample_pre_control_size
        || subsample_pre_control_size <= 0) {
      stop("`subsample_pre_control_size` needs to be positive integer.")
    }
    if (round(subsample_post_control_size) != subsample_post_control_size
        || subsample_post_control_size <= 0) {
      stop("`subsample_post_control_size` needs to be positive integer.")
    }
  }
  if (!is.numeric(seed)) {
    stop("`seed` must be numeric.")
  }


  out$seed <- seed

  #~ set seed for reproducibility
  set.seed(seed)
  if (!quietly) message(paste("seed has been set:", seed))

  #~ TODO: bandwidths > 0 error check
  #~ TODO: check what happens when no bandwidth is given but cost matrices are given
  #~ TODO: send a warning that if conservative = T and est != "dit", then we ignore conservative = T; instead, request the user to use twice the bandwidth in bandwidth_seq; i.e., tell the users that conservative = T only applies when est != "dit"
  #~ TODO: allow for placebo matrix to be an argument


  # error checking / get estimator
  estimator <- tolower(estimator)
  tc_messages <- c("tc", "ba", "before-and-after")
  dit_messages <- c("dit", "differences-in-transports", "diff-in-transports")
  if (estimator %in% tc_messages) {
    if (!is.null(pre_control) || !is.null(post_control)) {
      message("`pre_control` and/or `post_control` will be ignored.")
    }
    #~ est_message <- "Computing Transport Costs..."
    est <- "ba"
    if (conservative) {
      message("Setting `conservative` to FALSE")
      conservative <- FALSE
    }
    #~ if (!suppress_progress_bar & !quietly) message(est_message)
  } else if (estimator %in% dit_messages) {
    if (is.null(pre_control) || is.null(post_control)) {
      message("`pre_control` and/or `post_control` is mising.")
    }
    #~ est_message <- "Computing Differences-in-Transports Estimator..."
    est <- "dit"
    #~ if (!suppress_progress_bar & !quietly) message(est_message)
  } else {
    stop("Invalid estimator. Double-check inputs.")
  }

  out$estimator <- est

  if (!quietly) message(paste("estimator has been set:", estimator))

  #~ check support
  check_main <- check_support(pre_main, post_main,
                              var = !!rlang::ensym(var))
  if (check_main$status == 1) {
    stop(check_main$message)
  } else {
    main_support <- check_main$support
  }
  out$main_support <- main_support

  if (est == "dit") {
    check_control <- check_support(pre_control, post_control,
                                   var = !!rlang::ensym(var))
    if (check_control$status == 1) {
      stop(check_control$message)
    } else {
      control_support <- check_control$support
      out$control_support <- control_support
    }
  }

  if (!quietly) message(paste("supports have been computed."))

  pre_main_count <- pre_main[[rlang::ensym(count)]]
  post_main_count <- post_main[[rlang::ensym(count)]]
  pre_main_total <- sum(pre_main_count)
  post_main_total <- sum(post_main_count)
  if (est == "dit") {
    pre_control_count <- pre_control[[rlang::ensym(count)]]
    post_control_count <- post_control[[rlang::ensym(count)]]
    pre_control_total <- sum(pre_control_count)
    post_control_total <- sum(post_control_count)
  }

  if (!quietly) message(paste("counts have been computed."))

  #~ select appropriate bandwidth -- d_star
  if (sims_bandwidth_selection > 0) {

    #~ get placebo distributions for main
    pre_main_placebo <- rmultinom(n = sims_bandwidth_selection,
                                  size = pre_main_total,
                                  prob = pre_main_count)
    post_main_placebo <- rmultinom(n = sims_bandwidth_selection,
                                   size = post_main_total,
                                   prob = pre_main_count)

    if (est == "ba") {
      placebo <- sapply(
        seq_len(sims_bandwidth_selection),
        function(sim) {
          if (!quietly) print(paste("placebo simulation:", sim))
          pre_count <- pre_main_placebo[seq_along(main_support), sim]
          post_count <- post_main_placebo[seq_along(main_support), sim]
          pre_placebo <- data.frame(x = main_support,
                                    y = pre_count)
          post_placebo <- data.frame(x = main_support,
                                     y = post_count)
          sapply(
            seq_along(bandwidth_seq),
            function(bw_index) {
              bw <- bandwidth_seq[bw_index]
              placebo_result <- get_OTcost(pre_placebo,
                                           post_placebo,
                                           bw,
                                           var = x,
                                           count = y,
                                           costmat = costm_main,
                                           costmat_ref = costm_ref_main)
              placebo_result$prop_cost
            }
          )
        }
      )
    }

    if (est == "dit") {
      #~ get placebo distributions for control
      pre_control_placebo <- rmultinom(n = sims_bandwidth_selection,
                                    size = pre_control_total,
                                    prob = pre_control_count)
      post_control_placebo <- rmultinom(n = sims_bandwidth_selection,
                                     size = post_control_total,
                                     prob = pre_control_count)

      placebo <- sapply(
        seq_len(sims_bandwidth_selection),
        function(sim) {
          if (!quietly) print(paste("placebo simulation:", sim))
          pre_count <- pre_main_placebo[seq_along(main_support), sim]
          post_count <- post_main_placebo[seq_along(main_support), sim]
          pre_main_placebo <- data.frame(x = main_support,
                                         y = pre_count)
          post_main_placebo <- data.frame(x = main_support,
                                          y = post_count)

          pre_count <- pre_control_placebo[seq_along(control_support), sim]
          post_count <- post_control_placebo[seq_along(control_support), sim]
          pre_control_placebo <- data.frame(x = control_support,
                                    y = pre_count)
          post_control_placebo <- data.frame(x = control_support,
                                     y = post_count)

          sapply(
            seq_along(bandwidth_seq),
            function(bw_index) {
              bw <- bandwidth_seq[bw_index]
              main_bw <- ifelse(conservative, 2 * bw, bw)
              control_bw <- bw
              placebo_main <- get_OTcost(pre_main_placebo,
                                         post_main_placebo,
                                         main_bw,
                                         var = x,
                                         count = y,
                                         costmat = costm_main,
                                         costmat_ref = costm_ref_main)
              placebo_control <- get_OTcost(pre_control_placebo,
                                            post_control_placebo,
                                            control_bw,
                                            var = x,
                                            count = y,
                                            costmat = costm_control,
                                            costmat_ref = costm_ref_control)
              abs(placebo_main$prop_cost - placebo_control$prop_cost)
            }
          )
        }
      )
    }
    #~ dim(placebo): length(bandwidth_seq) x sims_bandwidth_selection
    colnames(placebo) <- paste0("sim", seq_len(sims_bandwidth_selection))
    placebo_cleaned <- cbind(bandwidth = bandwidth_seq, as.data.frame(placebo))
    placebo_summary <- placebo_cleaned %>%
      dplyr::rowwise(bandwidth) %>%
      dplyr::summarize(mean = mean(c_across()),
                       quantile0.90 = quantile(c_across(), prob = 0.90),
                       quantile0.95 = quantile(c_across(), prob = 0.95),
                       quantile0.99 = quantile(c_across(), prob = 0.99))

    #~ TODO: send placebo_cleaned and placebo_summary to user
    out$placebo <- placebo_cleaned
    out$placebo_summary <- placebo_summary

    if (!quietly) message(paste("Placebo summary has been created."))

    valid_d_index <- valid_bandwidths(placebo_summary$mean,
                                      sensitivity_lag,
                                      sensitivity_lead,
                                      sensitivity_accept,
                                      precision)
    valid_d <- bandwidth_seq[valid_d_index &
                             bandwidth_seq >= minimum_bandwidth &
                             bandwidth_seq <= maximum_bandwidth]

    if (est == "ba") {
      d_star <- min(valid_d)
    }
    if (est == "dit") {
      d_star <- valid_d
    }

    #~ TODO: send d_star to user; d_star = all the valid bandwidths
  } else {
    valid_d <- bandwidth_seq[bandwidth_seq >= minimum_bandwidth &
                             bandwidth_seq <= maximum_bandwidth]
    d_star <- valid_d
  }

  out$acceptable_bandwidths <- valid_d

  if (!quietly) message(paste("candidate bandwidths:"))
  if (!quietly) print(valid_d)

  #~ evaluate real/empirical optimal transport at all values in d_star
  main_bw <- ifelse(conservative, 2 * d_star, d_star)
  real_main <- sapply(
    seq_along(main_bw),
    function(bw_index) {
      cost <- get_OTcost(pre_main,
                         post_main,
                         bandwidth = main_bw[bw_index],
                         var = !!rlang::ensym(var),
                         count = !!rlang::ensym(count),
                         costmat = costm_main,
                         costmat_ref = costm_ref_main)
      cost$prop_cost
    }
  )

  if (est == "dit") {
    control_bw <- d_star
    real_control <- sapply(
      seq_along(control_bw),
      function(bw_index) {
        cost <- get_OTcost(pre_control,
                           post_control,
                           bandwidth = control_bw[bw_index],
                           var = !!rlang::ensym(var),
                           count = !!rlang::ensym(count),
                           costmat = costm_main,
                           costmat_ref = costm_ref_main)
        cost$prop_cost
      }
    )
    real_cost <- real_main - real_control

    real <- data.frame(bandwidths = d_star,
                       main = real_main,
                       control = real_control,
                       diff = real_cost)
  }
  if (est == "ba") {
    real_cost <- real_main

    real <- data.frame(bandwidth = d_star,
                       main = real_main)
  }

  result_index <- which.max(real$main)
  result <- real_cost[result_index]
  d <- d_star[result_index]

  out$d_star <- d
  out$empirical_cost <- result
  out$empirical_table <- real

  if (!quietly) message(paste("empirical cost:", result))
  if (!quietly) message(paste("bandwidth:", d))

  #~ TODO: print: The (conservative) ba/dit estimate is result (in %) at bw d
  #~ TODO: send result, d, and real to user

  if (sims_subsampling > 0) {
#~     pre_main_count <- pre_main[[rlang::ensym(count)]]
#~     post_main_count <- post_main[[rlang::ensym(count)]]
#~     pre_main_subsamples <- rmultinom(n = sims_bandwidth_selection,
#~                                      size = subsample_main_pre_size,
#~                                      prop = pre_main_count)
#~     post_main_subsamples <- rmultinom(n = sims_bandwidth_selection,
#~                                       size = subsample_main_post_size,
#~                                       prop = post_main_count)
    pre_main_dist <- pre_main %>%
      tidyr::uncount({{count}}) %>%
      .[[rlang::ensym(var)]]
    post_main_dist <- post_main %>%
      tidyr::uncount({{count}}) %>%
      .[[rlang::ensym(var)]]
    pre_main_subsamples <- sapply(
      seq_len(sims_subsampling),
      function(sim) {
        organize_subsamples(pre_main_dist,
                            subsample_pre_main_size,
                            main_support)
      }
    )
    post_main_subsamples <- sapply(
      seq_len(sims_subsampling),
      function(sim) {
        organize_subsamples(post_main_dist,
                            subsample_post_main_size,
                            main_support)
      }
    )

    if (!quietly) message(paste("subsamples have been generated."))

    stopifnot(ncol(pre_main_subsamples) == sims_subsampling) #~ check dimensions

    if (est == "ba") {
      subsample <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          if (!quietly) message(paste("subsamples:", sim))
          pre_count <- pre_main_subsamples[seq_along(main_support), sim]
          post_count <- post_main_subsamples[seq_along(main_support), sim]
          pre_subsample <- data.frame(x = main_support,
                                      y = pre_count)
          post_subsample <- data.frame(x = main_support,
                                       y = post_count)
          subsample_result <- get_OTcost(pre_subsample,
                                         post_subsample,
                                         d,
                                         var = x,
                                         count = y,
                                         costmat = costm_main,
                                         costmat_ref = costm_ref_main,
                                         scale_pre = "subsample",
                                         scale_post = "subsample",
                                         total = post_main_total)
          subsample_result$prop_cost
        }
      )
    }
    if (est == "dit") {
      pre_control_dist <- pre_control %>%
        tidyr::uncount({{count}}) %>%
        .[[rlang::ensym(var)]]
      post_control_dist <- post_control %>%
        tidyr::uncount({{count}}) %>%
        .[[rlang::ensym(var)]]
      pre_control_subsamples <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          organize_subsamples(pre_control_dist,
                              subsample_pre_control_size,
                              control_support)
        }
      )
      post_control_subsamples <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          organize_subsamples(post_control_dist,
                              subsample_post_control_size,
                              control_support)
        }
      )
      main_d <- ifelse(conservative, 2 * d, d)
      control_d <- d
      subsample <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          if (!quietly) message(paste("subsamples:", sim))
          pre_count <- pre_main_subsamples[seq_along(main_support), sim]
          post_count <- post_main_subsamples[seq_along(main_support), sim]
          pre_main_subsample <- data.frame(x = main_support,
                                      y = pre_count)
          post_main_subsample <- data.frame(x = main_support,
                                       y = post_count)

          pre_count <- pre_control_subsamples[seq_along(control_support), sim]
          post_count <- post_control_subsamples[seq_along(control_support), sim]
          pre_control_subsample <- data.frame(x = control_support,
                                              y = pre_count)
          post_control_subsample <- data.frame(x = control_support,
                                               y = post_count)

          subsample_real <- get_OTcost(pre_main_subsample,
                                       post_main_subsample,
                                       main_d,
                                       var = x,
                                       count = y,
                                       costmat = costm_main,
                                       costmat_ref = costm_ref_main,
                                       scale_pre = "subsample",
                                       scale_post = "subsample",
                                       total = post_main_total)

          subsample_control <- get_OTcost(pre_control_subsample,
                                          post_control_subsample,
                                          control_d,
                                          var = x,
                                          count = y,
                                          costmat = costm_control,
                                          costmat_ref = costm_ref_control,
                                          scale_pre = "subsample",
                                          scale_post = "subsample",
                                          total = post_control_total)

          subsample_real$prop_cost - subsample_control$prop_cost
        }
      )
    }
    out$subsample <- subsample
  }

  #~ TODO: send subsample to user

  message("DONE")

  return(out)

#~   # initialization
#~   main_prop <- rep(NA_real_, length(bandwidth_seq))
#~   if (conservative) maincons_prop <- rep(NA_real_, length(bandwidth_seq))
#~   if (est == "dit") control_prop <- rep(NA_real_, length(bandwidth_seq))
#~ 
#~   # computation
#~   if (!suppress_progress_bar) {
#~     pb <- utils::txtProgressBar(min = 0, max = length(bandwidth_seq), initial = 0)
#~   }
#~   for (i in seq_along(bandwidth_seq)) {
#~     if (!suppress_progress_bar) utils::setTxtProgressBar(pb, i)
#~ 
#~     bandwidth <- bandwidth_seq[i]
#~     main_cost <- get_OTcost(pre_main,
#~                             post_main,
#~                             bandwidth = bandwidth,
#~                             var = !!rlang::ensym(var),
#~                             count = !!rlang::ensym(count),
#~                             costmat = costm_main,
#~                             costmat_ref = costm_ref_main)
#~     if (conservative) maincons_cost <- get_OTcost(pre_main,
#~                                                   post_main,
#~                                                   bandwidth = 2*bandwidth,
#~                                                   var = !!rlang::ensym(var),
#~                                                   count = !!rlang::ensym(count),
#~                                                   costmat = costm_main,
#~                                                   costmat_ref = costm_ref_main)
#~     if (est == "dit") control_cost <- get_OTcost(pre_control,
#~                                                  post_control,
#~                                                  bandwidth = bandwidth,
#~                                                  var = !!rlang::ensym(var),
#~                                                  count = !!rlang::ensym(count),
#~                                                  costmat = costm_control,
#~                                                  costmat_ref = costm_ref_control)
#~ 
#~     main_prop[i] <- main_cost$prop_cost
#~     if (conservative) maincons_prop[i] <- maincons_cost$prop_cost
#~     if (est == "dit") control_prop[i] <- control_cost$prop_cost
#~   }
#~ 
#~   cat("\n")
#~ 
#~   # compile results
#~   if (est == "dit") {
#~     diffprop <- main_prop - control_prop
#~ 
#~     if (conservative) {
#~       diffprop2d <- maincons_prop - control_prop
#~       out <- data.frame(bandwidth = bandwidth_seq,
#~                         main = main_prop,
#~                         main2d = maincons_prop,
#~                         control = control_prop,
#~                         diff = diffprop,
#~                         diff2d = diffprop2d)
#~       whichmax <- which.max(diffprop2d)
#~       dit <- diffprop2d[whichmax]
#~       dstar <- bandwidth_seq[whichmax]
#~       if (!quietly) message(paste("The conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
#~     } else {
#~       out <- data.frame(bandwidth = bandwidth_seq,
#~                         main = main_prop,
#~                         control = control_prop,
#~                         diff = diffprop)
#~       whichmax <- which.max(diffprop)
#~       dit <- diffprop[whichmax]
#~       dstar <- bandwidth_seq[whichmax]
#~       if (!quietly) message(paste("The non-conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
#~     }
#~     if (save_result) out <- list(out = out, dit = dit, optimal_bandwidth = dstar)
#~   }
#~ 
#~   if (est == "tc"){
#~     out <- data.frame(bandwidth = bandwidth_seq,
#~                       main = main_prop)
#~     if (conservative) out <- cbind(out, main2d = maincons_prop)
#~     if (!quietly) message(paste("The transport cost for the specified bandwidths have been computed."))
#~   }
#~ 
#~   return(invisible(out))
}
