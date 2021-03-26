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

### diftrans ---------------------------

#' Compute Before-and-After Estimator and Differences-in-Transports Estimator
#'
#' This function implements the before-and-after estimation procedure and the
#' differences-in-transports estimator described in Daljord et al. (2021).
#'
#' The pre- and post-distributions given by \code{pre_main}
#' and \code{post_main} need to be \code{data.frame} objects of
#' two columns. The first column, whose title should be given by
#' \code{var}, contains the common support of the two distributions.
#' In other words, the values in \code{var} in each \code{pre_df}
#' and \code{post_df} need to be unique and the same across the two
#' distributions.
#' The second column named \code{count} provides the mass on the
#' respective value of the support.
#' If the sum of \code{count} in each \code{pre_df} and \code{post_df} is 1,
#' then \code{pre_df} and \code{post_df} are probability mass functions.
#'
#' The other pair of pre- and post-distributions given by \code{pre_control}
#' and \code{post_control} follow the same conventions.
#' In particular, their column names must also be \code{var} and \code{count}.
#'
#' If \code{pre_main} and \code{post_main} are specified but \code{pre_control}
#' and \code{post_control} are not, then the function computes the
#' before-and-after estimator between \code{pre_main} and \code{post_main}.
#' If \code{pre_control} and \code{post_control} are also specified, then
#' the function computes the differences-in-transports estimator.
#'
#' When \code{conservative} is set to TRUE, the differences-in-transports
#' estimator uses twice the bandwidth when computing the optimal transport cost
#' between the main distributions.
#'
#' Each entry of \code{bandwidth_vec} should be a non-negative number.
#' If mass is transferred less than this number, we ignore these transfers.
#' Otherwise, we place equal weight on the transfers.
#' As a result, the transport costs to be used by our estimators can be viewed
#' as the percentage of mass that has been transferred by more than a
#' a bandwidth's distance away.
#'
#' The bandwidth selection process chooses a bandwidth among
#' \code{bandwidth_vec} to filter out sampling variation.
#' The selection process differs by the estimator, but for both estimators,
#' placebo costs are computed for each bandwidth in every simulation.
#' We identify bandwidths whose mean placebo costs across all simulations
#' is less than \code{precision}.
#' For each such bandwidth, we ask whether \code{sensitivity_accept} of the
#' following \code{sensitivity_lead} and preceding \code{sensitivity_lag}
#' bandwidths in \code{bandwidth_vec} are also less than \code{precision}.
#' If this is the case, then we consider such a bandwidth as a candidate
#' for the estimator.
#' Note that we also require candidate bandwidths to be at least
#' \code{minimum_bandwidth} and no more than \code{maximum_bandwidth}.
#'
#' For the before-and-after estimator, we choose the smallest of the candidate
#' bandwidths.
#' For the differences-in-transports estimator, we choose the candidate
#' bandwidth with the largest value of the estimator.
#'
#' For the subsampling procedure, we sample \code{pre_main_subsample_size}
#' observations from the pre-distribution of the treated units
#' without replacement.
#' Similarly, we sample \code{post_main_subsample_size} observations from
#' the post-distribution without replacement
#' If we are computing the differences-in-transports estimator, we do the
#' the same with the utreated analogues.
#'
#' @param pre_main A two-column \code{data.frame} describing the
#'  pre-distribution of the treated observations
#' @param post_main A two-column \code{data.frame} describing the
#'  post-distribution of the treated observations
#' @param pre_control A two-column \code{data.frame} describing the
#'  pre-distribution of the untreated observations; only required for the
#'  differences-in-transports estimator
#' @param post_control A two-column \code{data.frame} describing the
#'  post-distribution of the untreated observations; only required for the
#'  differences in transports estimator
#' @param var The title of the common support columns of \code{pre_main},
#'  \code{post_main}, \code{pre_control}, and \code{post_control};
#'  defaults to \code{MSRP}
#' @param count The title of the second column of \code{pre_main},
#'  \code{post_main}, \code{pre_control}, and \code{post_control};
#'  defaults to \code{count}
#' @param bandwidth_vec A vector of non-negative bandwidth values;
#'  defaults to \code{seq(0, 40000, 1000)}
#' @param minimum_bandwidth Minimum bandwidth to consider for the estimator;
#'  defaults to 0
#' @param maximum_bandwidth Maximum bandwidth to consider for the estimator;
#'  defaults to infinity
#' @param estimator A string that takes on the value of "dit" for
#'  differences-in-transports estimator or "tc" for the transport cost;
#'  if \code{pre_control} and \code{post_control} are specified,
#'  default is "dit";
#'  otherwise, default is "tc"
#' @param sims_bandwidth_selection Number of simulations for the bandwidth
#'  selection process; defaults to 0
#' @param precision Threshold to choose the bandwidth; defaults to 0.0005, i.e.,
#'  0.05 percent.
#' @param sensitivity_lag,sensitivity_lead,sensitivity_accept
#'  See "Details" to understand how these parameters influence bandwidth
#'  selection
#' @param sims_subsampling Number of subsampling simulations
#' @param pre_main_subsample_size,post_main_subsample_size,pre_control_subsample_size,post_control_subsample_size
#'  Size of subsample distributions
#' @param seed Seed for reproducibility; see \code{\link{set.seed}}
#' @param conservative If TRUE, then the bandwidth sequence will be
#'  multiplied by 2 to provide a conservative estimate of the
#'  difference-in-transports estimator; default is FALSE, only valid
#'  for difference-in-transports estimator
#' @param quietly If TRUE, some messages will be suppressed from printing;
#'  defaults FALSE
#' @param show_progress If \code{TRUE}, placebo and subsampling simulations
#'  will show progress; defaults to FALSE
#'
#' @return an object of the "diftrans" class with the following information:
#' \itemize{
#'  \item \code{bandwidth_vec}: sorted vector of unique bandwidth values used
#'    for evaluating ground cost
#'  \item \code{est}: either "ba" for the before-and-after estimator
#'    or "dit" for the differences-in-transports estimator
#'  \item \code{pre_main_total}: total count in \code{pre_main}
#'  \item \code{post_main_total}: total count in \code{post_main}
#'  \item \code{pre_control_total}: total count in \code{pre_control}
#'  \item \code{post_control_total}: total count in \code{post_control}
#'  \item \code{sims_bandwidth_selection}: number of simulations to run for
#'    bandwidth selection
#'  \item \code{pre_main_subsample_size},
#'        \code{post_main_subsample_size},
#'        \code{post_control_subsample_size},
#'        \code{post_control_subsample_size}: size of subsample distributions
#'  \item \code{conservative}: TRUE or FALSE, depending on whether we use
#'    twice the bandwidth for the treated distributions in the
#'    differences-transports-estimator
#'  \item \code{seed}: seed (see \code{\link{set.seed}})
#'  \item \code{main_support}: common support of the treated distributions
#'  \item \code{control_support}: common support of the untreated distributions
#'  \item \code{empirical_table}: data frame of transport costs at each
#'    bandwidth in \code{bandwidth_vec} as well as indicators for the
#'    candidate bandwidths and the final bandwidth chosen for the estimate
#'    (\code{optimal_bandwidth})
#'  \item \code{candidate_bandwidths}: vector of candidate bandwidths that
#'    survive the bandwidth selection
#'  \item \code{optimal_bandwidth}: final bandwidth for estimator
#'  \item \code{empirical_cost}: value of estimator at \code{optimal_bandwidth}
#'  \item \code{subsample}: vector of costs that arise from subsampling
#'  \item \code{call}: function call to produce the object of class "diftrans"
#' }
#'
#' @export
#' @importFrom rlang ensym
#' @importFrom rlang enquo
#' @importFrom stats quantile
#' @importFrom stats rmultinom
#' @importFrom stats sd
#' @examples
#' # Compute transport cost between Beijing 2010 and Beijing 2011
#' # with bandwidth = 0
#' ## step 1: find support
#' support_Beijing <- Beijing_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2012-01-01") %>%
#'   dplyr::select(MSRP) %>%
#'   dplyr::distinct() %>%
#'   dplyr::arrange(MSRP) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   unlist()
#' temp <- data.frame(MSRP = support_Beijing)
#' ## step 2: prepare distributions
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
#' ## step 3: compute results
#' tc <- diftrans(pre_Beijing, post_Beijing, conservative = TRUE, bandwidth = 0)
#' tc$empirical_cost
#'
#' # Find conservative differences-in-transport estimator using Tianjin
#' # as a control
#' ## step 1: find support
#' support_Tianjin <- Tianjin_sample %>%
#'   dplyr::filter(ym >= as.Date("2010-01-01") & ym < "2012-01-01") %>%
#'   dplyr::select(MSRP) %>%
#'   dplyr::distinct() %>%
#'   dplyr::arrange(MSRP) %>%
#'   dplyr::filter(!is.na(MSRP)) %>%
#'   unlist()
#' temp <- data.frame(MSRP = support_Tianjin)
#' ## step 2: prepare distributions
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
#' ## step 3: compute results
#' dit <- diftrans(pre_Beijing, post_Beijing, pre_Tianjin, post_Tianjin,
#'                    conservative = TRUE, bandwidth = seq(0, 40000, 1000))
#' dit$optimal_bandwidth
#' dit$empirical_cost
diftrans <- function(pre_main = NULL,
                     post_main = NULL,
                     pre_control = NULL,
                     post_control = NULL,
                     var = MSRP,
                     count = count,
                     bandwidth_vec = seq(0, 40000, 1000),
                     minimum_bandwidth = 0,
                     maximum_bandwidth = Inf,
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
                     pre_main_subsample_size = NULL,
                     post_main_subsample_size = NULL,
                     pre_control_subsample_size = NULL,
                     post_control_subsample_size = NULL,
                     seed = 1,
                     conservative = FALSE,
                     quietly = TRUE,
                     show_progress = FALSE) {

  #~ initialize output
  out <- list()

# Preliminaries ----------

  prelim <- preliminaries(
    pre_main = pre_main,
    post_main = post_main,
    pre_control = pre_control,
    post_control = post_control,
    var = !!rlang::enquo(var),
    count = !!rlang::enquo(count),
    bandwidth_vec = bandwidth_vec,
    minimum_bandwidth = minimum_bandwidth,
    maximum_bandwidth = maximum_bandwidth,
    estimator = estimator,
    sims_bandwidth_selection = sims_bandwidth_selection,
    precision = precision,
    sensitivity_lag = sensitivity_lag,
    sensitivity_lead = sensitivity_lead,
    sensitivity_accept = sensitivity_accept,
    sims_subsampling = sims_subsampling,
    pre_main_subsample_size = pre_main_subsample_size,
    post_main_subsample_size = post_main_subsample_size,
    pre_control_subsample_size = pre_control_subsample_size,
    post_control_subsample_size = post_control_subsample_size,
    seed = seed,
    conservative = conservative,
    quietly = quietly,
    show_progress = show_progress
  )

  bandwidth_vec <- sort(unique(bandwidth_vec))
  out$bandwidth_vec <- bandwidth_vec

  est <- prelim$est
  pre_main_total <- prelim$pre_main_total
  post_main_total <- prelim$post_main_total
  pre_control_total <- prelim$pre_control_total
  post_control_total <- prelim$post_control_total
  pre_main_count <- prelim$pre_main_count
  post_main_count <- prelim$post_main_count
  pre_control_count <- prelim$pre_control_count
  post_control_count <- prelim$post_control_count

  out$est <- prelim$est
  out$pre_main_total <- prelim$pre_main_total
  out$post_main_total <- prelim$post_main_total
  out$pre_control_total <- prelim$pre_control_total
  out$post_control_total <- prelim$post_control_total

  out$sims_bandwidth_selection <- sims_bandwidth_selection
  out$sims_subsampling <- sims_subsampling
  out$pre_main_subsample_size <- prelim$pre_main_subsample_size
  out$post_main_subsample_size <- prelim$post_main_subsample_size
  out$pre_control_subsample_size <- prelim$pre_control_subsample_size
  out$post_control_subsample_size <- prelim$post_control_subsample_size

  conservative <- prelim$conservative
  out$conservative <- prelim$conservative

  out$precision <- precision
  out$sensitivity_lag <- sensitivity_lag
  out$sensitivity_lead <- sensitivity_lead

  if (est == "ba") {
    message("Before-and-After Estimation...")
  } else if (est == "dit") {
    message("Differences-in-Transports Estimation...")
  }

  out$seed <- seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  msg <- paste("Seed has been set to", seed)
  send_note(msg, quietly, message)


  #~ TODO: allow for placebo matrix to be an argument


# Check Support ----------

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

  msg <- "Supports have been computed."
  send_note(msg, quietly, message)

  pre_main_copy <- data.frame(support = main_support,
                              count = pre_main_count)
  post_main_copy <- data.frame(support = main_support,
                               count = post_main_count)
  out$pre_main <- pre_main_copy
  out$post_main <- post_main_copy
  if (est == "dit") {
    pre_control_copy <- data.frame(support = control_support,
                                   count = pre_control_count)
    post_control_copy <- data.frame(support = control_support,
                                    count = post_control_count)
    out$pre_control <- pre_control_copy
    out$post_control <- post_control_copy
  }

# Compute Empirical Costs ----------

  #~ evaluate real/empirical optimal transport at all values in acc_bw
  main_bw <- if (conservative) 2 * bandwidth_vec else bandwidth_vec
  main_cost <- sapply(
    seq_along(main_bw),
    function(bw_index) {
      cost <- get_OTcost(pre_main,
                         post_main,
                         bandwidth = main_bw[bw_index],
                         var = !!rlang::ensym(var),
                         count = !!rlang::ensym(count))
      cost$prop_cost
    }
  )

  if (est == "ba") {
    empirical_cost <- main_cost
    real <- data.frame(bandwidth = bandwidth_vec,
                       result = empirical_cost)
  } else if (est == "dit") {
    control_bw <- bandwidth_vec
    control_cost <- sapply(
      seq_along(control_bw),
      function(bw_index) {
        cost <- get_OTcost(pre_control,
                           post_control,
                           bandwidth = control_bw[bw_index],
                           var = !!rlang::ensym(var),
                           count = !!rlang::ensym(count))
        cost$prop_cost
      }
    )
    empirical_cost <- main_cost - control_cost

    real <- data.frame(bandwidth = bandwidth_vec,
                       main = main_cost,
                       control = control_cost,
                       result = empirical_cost)
  }

  out$empirical_table <- real

  msg <- "Empirical costs have been computed."
  send_note(msg, quietly, message)


# Bandwidth Selection ----------

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
          progress <- paste("Bandwidth Selection:",
                            "Running Simulation",
                            sim, "/", sims_bandwidth_selection)
          send_note(progress, !show_progress, print)
          pre_count <- pre_main_placebo[seq_along(main_support), sim]
          post_count <- post_main_placebo[seq_along(main_support), sim]
          pre_placebo <- data.frame(x = main_support,
                                    y = pre_count)
          post_placebo <- data.frame(x = main_support,
                                     y = post_count)
          sapply(
            seq_along(bandwidth_vec),
            function(bw_index) {
              bw <- bandwidth_vec[bw_index]
              placebo_result <- get_OTcost(pre_placebo,
                                           post_placebo,
                                           bw,
                                           var = x,
                                           count = y)
              placebo_result$prop_cost
            }
          )
        }
      )
    } else if (est == "dit") {
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
          progress <- paste("Bandwidth Selection:",
                            "Running Simulation",
                            sim, "/", sims_bandwidth_selection)
          send_note(progress, !show_progress, print)
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
            seq_along(bandwidth_vec),
            function(bw_index) {
              bw <- bandwidth_vec[bw_index]
              main_bw <- ifelse(conservative, 2 * bw, bw)
              control_bw <- bw
              placebo_main <- get_OTcost(pre_main_placebo,
                                         post_main_placebo,
                                         main_bw,
                                         var = x,
                                         count = y)
              placebo_control <- get_OTcost(pre_control_placebo,
                                            post_control_placebo,
                                            control_bw,
                                            var = x,
                                            count = y)
              abs(placebo_main$prop_cost - placebo_control$prop_cost)
            }
          )
        }
      )
    }

    #~ dim(placebo): length(bandwidth_vec) x sims_bandwidth_selection
    stopifnot(dim(placebo) == c(length(bandwidth_vec),
                                sims_bandwidth_selection))
    colnames(placebo) <- paste0("sim", seq_len(sims_bandwidth_selection))
    placebo_cleaned <- cbind(bandwidth = bandwidth_vec, as.data.frame(placebo))
    placebo_summary <- placebo_cleaned %>%
      dplyr::rowwise(bandwidth) %>%
      dplyr::summarize(mean = mean(dplyr::c_across()),
                       quantile0.90 = quantile(dplyr::c_across(), prob = 0.90),
                       quantile0.95 = quantile(dplyr::c_across(), prob = 0.95),
                       quantile0.99 = quantile(dplyr::c_across(), prob = 0.99),
                       sd = sd(dplyr::c_across()))

    out$placebo <- placebo_cleaned
    out$placebo_summary <- placebo_summary

    if (!quietly) message(paste("Placebo summary has been created."))

    cand_bw_index <- valid_bandwidths(
      placebo_summary$mean,
      sensitivity_lag,
      sensitivity_lead,
      sensitivity_accept,
      precision
    ) & bandwidth_vec >= minimum_bandwidth & bandwidth_vec <= maximum_bandwidth
    cand_bw <- bandwidth_vec[cand_bw_index]

    if (est == "ba") {
      acc_bw <- min(cand_bw)
    } else if (est == "dit") {
      acc_bw <- cand_bw
    }

  } else {
    cand_bw_index <- bandwidth_vec >= minimum_bandwidth &
      bandwidth_vec <= maximum_bandwidth
    cand_bw <- bandwidth_vec[bandwidth_vec >= minimum_bandwidth &
                             bandwidth_vec <= maximum_bandwidth]
    acc_bw <- cand_bw
  }

  if (length(cand_bw) == 0) {
    message("Error: There are no bandwidths in `bandwidth_vec` that work.")
    return(out)
  }

  out$candidate_bandwidths <- cand_bw

  if (!quietly) message(paste("candidate bandwidths:"))
  if (!quietly) print(cand_bw)

# Choosing Empirical Cost ----------

  cand_real <- cbind(real, "candidates" = cand_bw_index)
  filtered_real <- real[cand_bw_index, ]

  max_result <- max(filtered_real$result)
  result_index <- min(which(cand_real$result == max_result))
  result_column <- rep("-", nrow(cand_real))
  result_column[result_index] <- "*"
  result_real <- cbind(cand_real, "estimate" = result_column)
  result <- result_real[result_index, "result"]
  d_star <- result_real[result_index, "bandwidth"]

  out$optimal_bandwidth <- d_star
  out$empirical_cost <- result
  out$empirical_table <- result_real

  if (!quietly) message(paste("empirical cost:", result))
  if (!quietly) message(paste("bandwidth:", d_star))

  #~ TODO: print: The (conservative) ba/dit estimate is result (in %) at bw d


# Subsampling ----------

  if (sims_subsampling > 0) {
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
                            pre_main_subsample_size,
                            main_support)
      }
    )
    post_main_subsamples <- sapply(
      seq_len(sims_subsampling),
      function(sim) {
        organize_subsamples(post_main_dist,
                            post_main_subsample_size,
                            main_support)
      }
    )
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
                              pre_control_subsample_size,
                              control_support)
        }
      )
      post_control_subsamples <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          organize_subsamples(post_control_dist,
                              post_control_subsample_size,
                              control_support)
        }
      )
    }

    if (!quietly) message(paste("subsamples have been generated."))

    stopifnot(ncol(pre_main_subsamples) == sims_subsampling) #~ check dimensions

    if (est == "ba") {
      subsample <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          progress <- paste("Subsampling:",
                            "Running Simulation",
                            sim, "/", sims_subsampling)
          send_note(progress, !show_progress, print)
          pre_count <- pre_main_subsamples[seq_along(main_support), sim]
          post_count <- post_main_subsamples[seq_along(main_support), sim]
          pre_subsample <- data.frame(x = main_support,
                                      y = pre_count)
          post_subsample <- data.frame(x = main_support,
                                       y = post_count)
          subsample_result <- get_OTcost(pre_subsample,
                                         post_subsample,
                                         d_star,
                                         var = x,
                                         count = y,
                                         scale_pre = "subsample",
                                         scale_post = "subsample",
                                         total = post_main_total)
          subsample_result$prop_cost
        }
      )
    } else if (est == "dit") {
      main_d <- ifelse(conservative, 2 * d_star, d_star)
      control_d <- d_star
      subsample <- sapply(
        seq_len(sims_subsampling),
        function(sim) {
          progress <- paste("Subsampling:",
                            "Running Simulation",
                            sim, "/", sims_subsampling)
          send_note(progress, !show_progress, print)
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
                                       scale_pre = "subsample",
                                       scale_post = "subsample",
                                       total = post_main_total)

          subsample_control <- get_OTcost(pre_control_subsample,
                                          post_control_subsample,
                                          control_d,
                                          var = x,
                                          count = y,
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

# Return Results ----------

  message("DONE")

  out$call <- match.call()
  class(out) <- "diftrans"
  return(out)
}

### is.diftrans ---------------------------

#' Check if object is of class "diftrans"
#'
#' @param x An object
#'
#' @return TRUE if \code{x} is a "diftrans" object; FALSE otherwise
#'
#' @export
is.diftrans <- function(x) {
  inherits(x, "diftrans")
}

### print.diftrans ---------------------------

#' @importFrom scales percent
#' @export
print.diftrans <- function(x, accuracy = 0.01, ...) {
  if (x$est == "ba") {
    estimator <- "Before-and-After Estimate of "
  } else if (x$est == "dit" & x$conservative) {
    estimator <- "Conservative Differences-in-Transports estimate of "
  } else if (x$est == "dit" & !x$conservative) {
    estimator <- "Differences-in-Transports estimate of "
  }

  # cat("\n")
  cat(
    paste0(
      estimator,
      scales::percent(x$empirical_cost, accuracy = accuracy),
      " at bandwidth ",
      x$optimal_bandwidth,
      "."
    )
  )
  cat("\n")
}

### summary.diftrans ---------------------------

#' @importFrom tibble as_tibble
#' @importFrom scales percent
#' @importFrom stats sd
#' @export
summary.diftrans <- function(object, accuracy = 0.01, ...) {
  align <- 35
  header <- 60
  round <- nchar(accuracy)
  skipped <- "Skipped"
  if (object$est == "ba") {
    estimator <- "Before-and-After"
  } else if (object$est == "dit") {
    estimator <- "Differences-in-Transports"
  }

  if (object$sims_bandwidth_selection == 0) {
    cand_bw <- skipped
    placebo_skipped <- TRUE
  } else {
    cand_bw <- object$candidate_bandwidths
    placebo_skipped <- FALSE
    placebo_summary <- object$placebo_summary
    names(placebo_summary) <- c(
      "bandwidth",
      "mean",
      "90%ile",
      "95%ile",
      "99%ile",
      "std. dev."
    )
  }

  if (object$sims_subsampling == 0) {
    subsample_skipped <- TRUE
  } else {
    subsample_skipped <- FALSE
    subsample_summary <- c(
      "mean" = round(mean(object$subsample), 2),
      "std. dev." = round(sd(object$subsample), 2),
      "min" = round(min(object$subsample), round),
      "25%ile" = round(quantile(object$subsample, probs = 0.25, names = FALSE),
                             round),
      "median" = round(quantile(object$subsample, probs = 0.5, names = FALSE),
                       round),
      "75%ile" = round(quantile(object$subsample, probs = 0.75, names = FALSE),
                             round),
      "max" = round(min(object$subsample), round)
    )
  }

  cat(whitespace(header, "Call: diftrans", "-"))
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Estimator:"),
      estimator
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Conservative:"),
      object$conservative
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Estimate:"),
      scales::percent(object$empirical_cost, accuracy = accuracy)
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Seed:"),
      object$seed
    )
  )
  cat("\n")
  cat("\n")
  cat(whitespace(header, "Bandwidth Selection", "-"))
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Simulations:"),
      object$sims_bandwidth_selection
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Optimal Bandwidth:"),
      object$optimal_bandwidth
    )
  )
  cat("\n")
  if (!placebo_skipped) {
    cat(
      paste0(
        whitespace(align, "Criteria:")
      )
    )
    cat("\n")
    cat(
      paste0(
        whitespace(5),
        whitespace(align - 6, "Precision:"),
        object$precision
      )
    )
    cat("\n")
    cat(
      paste0(
        whitespace(5),
        whitespace(align - 6, "Lag:"),
        object$sensitivity_lag
      )
    )
    cat("\n")
    cat(
      paste0(
        whitespace(5),
        whitespace(align  - 6, "Lead:"),
        object$sensitivity_lead
      )
    )
    cat("\n")
    cat(
      paste0(
        whitespace(5),
        whitespace(align  - 6, "Accept:"),
        object$sensitivity_lead
      )
    )
    cat("\n")
  }
  cat(
    paste0(
      whitespace(align, "Candidate Bandwidths:"),
      paste(cand_bw, collapse = " ")
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Summary of Placebo Results:"),
      ifelse(placebo_skipped, skipped, " ")
    )
  )
  cat("\n")
  if (!placebo_skipped) {
    print(placebo_summary)
  }
  cat("\n")
  cat("\n")
  cat(whitespace(header, "Empirical Costs", "-"))
  cat("\n")
  print(tibble::as_tibble(object$empirical_table))
  cat("\n")
  cat(whitespace(header, "Subsampling", "-"))
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Simulations:"),
      object$sims_subsampling
    )
  )
  cat("\n")
  cat(
    paste0(
      whitespace(align, "Summary of Subsampling Results:"),
      ifelse(subsample_skipped, skipped, " ")
    )
  )
  cat("\n")
  if (!subsample_skipped) {
    print(subsample_summary)
  }
}

### plot_main ---------------------------

plot_main <- function(x, ...) {
  UseMethod("plot_main")
}
#' @importFrom tidyr uncount
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @export
plot_main.diftrans <- function(x,
                               grayscale = FALSE,
                               binwidth = NULL,
                               pre_fill = ifelse(grayscale,
                                                 "#0c0c0c",
                                                 "#E69F00"),
                               pre_color = ifelse(grayscale,
                                                  "#0c0c0c",
                                                  "#E69F00"),
                               post_fill = ifelse(grayscale,
                                                 "#dbd9d9",
                                                 "#56B4E9"),
                               post_color = ifelse(grayscale,
                                                  "#dbd9d9",
                                                  "#56B4E9"),
                               alpha = ifelse(grayscale, 0.8, 0.35),
                               pre_label = "Pre-Distribution",
                               post_label = "Post-Distribution",
                               FCN = function(x) {x},
                               ...) {
  pre <- x$pre_main
  post <- x$post_main
  pre_dist <- tidyr::uncount(pre, count)$support
  post_dist <- tidyr::uncount(post, count)$support

  if (is.null(binwidth)) {
    binwidth <- 30
    message("`binwidth` not specified; setting `binwidth` to 30")
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = FCN(pre_dist),
        y = ..density..,
        fill = "0",
        color = "0",
      ),
      alpha = alpha,
      binwidth = binwidth,
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = FCN(post_dist),
        y = ..density..,
        fill = "1",
        color = "1",
      ),
      alpha = alpha,
      binwidth = binwidth,
    ) +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_color_manual(
      values = c(pre_color, post_color),
      labels = c(pre_label, post_label),
      name = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(pre_fill, post_fill),
      labels = c(pre_label, post_label),
      name = ""
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("")

  return(plot)
}

### plot_control ---------------------------

plot_control <- function(x, ...) {
  UseMethod("plot_control")
}
#' @importFrom tidyr uncount
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_color_manual
#' @export
plot_control.diftrans <- function(x,
                               grayscale = FALSE,
                               binwidth = NULL,
                               pre_fill = ifelse(grayscale,
                                                 "#0c0c0c",
                                                 "#E69F00"),
                               pre_color = ifelse(grayscale,
                                                  "#0c0c0c",
                                                  "#E69F00"),
                               post_fill = ifelse(grayscale,
                                                 "#dbd9d9",
                                                 "#56B4E9"),
                               post_color = ifelse(grayscale,
                                                  "#dbd9d9",
                                                  "#56B4E9"),
                               alpha = ifelse(grayscale, 0.8, 0.35),
                               pre_label = "Pre-Distribution",
                               post_label = "Post-Distribution",
                               FCN = function(x) {x},
                               ...) {
  if (x$est == "ba") {
    stop("This method only works with difference-in-transports.")
  }
  pre <- x$pre_control
  post <- x$post_control
  pre_dist <- tidyr::uncount(pre, count)$support
  post_dist <- tidyr::uncount(post, count)$support

  if (is.null(binwidth)) {
    binwidth <- 30
    message("`binwidth` not specified; setting `binwidth` to 30")
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = FCN(pre_dist),
        y = ..density..,
        fill = "0",
        color = "0",
      ),
      alpha = alpha,
      binwidth = binwidth,
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = FCN(post_dist),
        y = ..density..,
        fill = "1",
        color = "1",
      ),
      alpha = alpha,
      binwidth = binwidth,
    ) +
    ggplot2::guides(color = FALSE) +
    ggplot2::scale_color_manual(
      values = c(pre_color, post_color),
      labels = c(pre_label, post_label),
      name = ""
    ) +
    ggplot2::scale_fill_manual(
      values = c(pre_fill, post_fill),
      labels = c(pre_label, post_label),
      name = ""
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("")

  return(plot)
}

### plot_subsample ---------------------------

plot_subsample <- function(x, ...) {
  UseMethod("plot_subsample")
}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @export
plot_subsample.diftrans <- function(x,
                                    FCN = function(x) {x},
                                    binwidth = NULL,
                                    fill = "#000000",
                                    color = "#000000",
                                    alpha = 0.35,
                                    title = NULL) {
  subsample <- x$subsample
  sims_subsampling <- x$sims_subsampling
  optimal_bw <- x$optimal_bandwidth
  if (sims_subsampling == 0) {
    stop("There are no subsampling results to plot.")
  }

  if (is.null(binwidth)) {
    binwidth <- 30
    message("`binwidth` not specified; setting `binwidth` to 30")
  }
  if (is.null(title)) {
    title <- paste("Subsampling Results; Bandwidth =", optimal_bw)
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(
        x = FCN(subsample),
        y = ..density..,
      ),
      binwidth = binwidth,
      fill = fill,
      color = color,
      alpha = alpha
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::ggtitle(title)

  return(plot)

}


### plot_empirical_placebo ---------------------------

plot_empirical_placebo <- function(x, ...) {
  UseMethod("plot_empirical_placebo")
}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_hline
#' @export
plot_empirical_placebo.diftrans <- function(x,
                                   grayscale = FALSE,
                                   empirical_color = ifelse(grayscale,
                                                            "#0c0c0c",
                                                            "#E69F00"),
                                   empirical_linetype = "solid",
                                   placebo_color = ifelse(grayscale,
                                                          "#dbd9d9",
                                                          "#56B4E9"),
                                   placebo_linetype = "dashed",
                                   empirical_label = NULL,
                                   placebo_label = NULL,
                                   xFCN = function(x) {x},
                                   empiricalFCN = function(x) {x},
                                   placeboFCN = function(x) {x},
                                   optimal_bandwidth_color = "#000000",
                                   optimal_bandwidth_linetype = "longdash",
                                   precision_color = "#000000",
                                   precision_linetype = "longdash",
                                   title = NULL,
                                   ...) {
  sims_bandwidth_selection <- x$sims_bandwidth_selection
  if (sims_bandwidth_selection == 0) {
    stop("There are no bandwidth selection results to plot.")
  }
  optimal_bandwidth <- x$optimal_bandwidth
  precision <- x$precision
  placebo_cost <- x$placebo_summary["mean"]
  empirical_cost <- x$empirical_table["result"]
  bandwidth <- x$bandwidth_vec
  plot_df <- data.frame(bandwidth = xFCN(bandwidth),
                        empirical = empiricalFCN(empirical_cost),
                        placebo = placeboFCN(placebo_cost))
  names(plot_df) <- c("bandwidth", "empirical", "placebo")

  if (is.null(empirical_label)) {
    empirical_label <- "Empirical Costs"
  }
  if (is.null(placebo_label)) {
    placebo_label <- paste0("Placebo Costs (",
                            sims_bandwidth_selection,
                            " simulations)")
  }
  if (is.null(title)) {
    title <- paste0("Optimal Bandwidth: ",
                    optimal_bandwidth,
                    "   Precision: ",
                    precision)
  }

  plot <- ggplot2::ggplot(data = plot_df,
                  ggplot2::aes(x = bandwidth)) +
    ggplot2::geom_line(
      data = plot_df,
      ggplot2::aes(
        y = empirical,
        color = "0",
        linetype = "0"
      )
    ) +
    ggplot2::geom_line(
      data = plot_df,
      ggplot2::aes(
        y = .data$placebo,
        color = "1",
        linetype = "1"
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(empirical_color, placebo_color),
      labels = c(empirical_label,
                 placebo_label),
      name = ""
    ) +
    ggplot2::scale_linetype_manual(
      values = c(empirical_linetype, placebo_linetype),
      labels = c(empirical_label,
                 placebo_label),
      name = ""
    ) +
    ggplot2::geom_vline(
      xintercept = optimal_bandwidth,
      color = optimal_bandwidth_color,
      linetype = optimal_bandwidth_linetype
    ) +
    ggplot2::geom_hline(
      yintercept = precision,
      color = precision_color,
      linetype = precision_linetype
    ) +
    ggplot2::ggtitle(title)

  return(plot)

}

### plot_empirical ---------------------------

plot_empirical <- function(x, ...) {
  UseMethod("plot_empirical")
}
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 theme
#' @importFrom scales percent
#' @export
plot_empirical.diftrans <- function(x,
                           grayscale = FALSE,
                           result_color = ifelse(grayscale,
                                                 "#0c0c0c",
                                                 "#E69F00"),
                           result_linetype = "solid",
                           main_color = ifelse(grayscale,
                                               "#383838",
                                               "#56B4E9"),
                           main_linetype = "dashed",
                           control_color = ifelse(grayscale,
                                            "#757575",
                                            "#009E73"),
                           control_linetype = "dotted",
                           result_label = "Difference",
                           main_label = "Treated",
                           control_label = "Control",
                           optimal_bandwidth_color = "#000000",
                           optimal_bandwidth_linetype = "longdash",
                           title = NULL,
                           accuracy = 0.01,
                           ...) {
  empirical_table <- x$empirical_table
  optimal_bandwidth <- x$optimal_bandwidth
  empirical_cost <- scales::percent(x$empirical_cost, accuracy = accuracy)
  if (is.null(title)) {
    estimator <- ifelse(x$est == "ba",
                        "Before-and-After Estimator of ",
                        "Difference-in-Transports Estimator of ")
    title <- paste0(estimator,
                    empirical_cost,
                    " at Bandwidth ",
                    optimal_bandwidth)
  }
  plot <- ggplot2::ggplot(data = empirical_table,
                          ggplot2::aes(x = bandwidth)) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = result,
        color = "0",
        linetype = "0"
      ),
    ) +
    ggplot2::geom_vline(
      xintercept = optimal_bandwidth,
      linetype = optimal_bandwidth_linetype
    ) +
    ggplot2::ggtitle(title)
  if (x$est == "dit") {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(
          y = main,
          color = "1",
          linetype = "1"
        )
      ) +
      ggplot2::geom_line(
        ggplot2::aes(
          y = control,
          color = "2",
          linetype = "2"
        )
      ) +
      ggplot2::scale_color_manual(
        values = c(result_color, main_color, control_color),
        labels = c(result_label, main_label, control_label),
        name = ""
      ) +
      ggplot2::guides(linetype = FALSE)
  } else if (x$est == "ba") {
    plot <- plot +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_color_manual(
        values = c(result_color)
      )
  }
  return(plot)
}
