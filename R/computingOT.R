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

build_costmatrix <- function(support, bandwidth = 0){
  # create cost matrix using the common support provided
  costmatrix <- matrix(NA_real_, nrow = length(support), ncol = length(support))
  for (i in seq_along(support)){
    dist <- abs(support[i] - support)
    dist <- ifelse(dist > bandwidth, 1, 0)
    costmatrix[i, ] <- dist
  }
  costmatrix
}

build_costmatrix2 <- function(support_pre, support_post, bandwidth = 0){
  # create cost matrix given the minimal support of both the pre-data and post-data
  costmatrix <- matrix(NA_real_, nrow = length(support_pre), ncol = length(support_post))
  for (i in seq_along(support_pre)){
    dist <- abs(support_pre[i] - support_post)
    dist <- ifelse(dist > bandwidth, 1, 0)
    costmatrix[i, ] <- dist
  }
  costmatrix
}

### Compute Transport Cost ---------------------------

#' @importFrom transport transport
#' @export
get_OTcost <- function(pre_df, post_df, support = NULL, bandwidth = 0, var = MSRP){
  # given pre-data and post-data, compute optimal transport cost given bandwidth
  pre <- pre_df$count
  post <- post_df$count
  pre_support <- pre_df %>% dplyr::select({{var}}) %>% unlist()
  post_support <- post_df %>% dplyr::select({{var}}) %>% unlist()

  if (!identical(pre_support, post_support)){
    stop("`pre_df` and `post_df` need to have the same support")
  }

  if (is.null(support)){
    support <- pre_df %>% dplyr::select({{var}}) %>% unlist()
  }

  if (!identical(support, pre_support)){
    stop("`support` is different from `pre_support` and `post_support`")
  }
  if (!identical(support, post_support)){
    stop("`support` is different from `pre_support` and `post_support`")
  }

  costm <- build_costmatrix(support, bandwidth)
  OT <- transport(
    as.numeric(sum(post) / sum(pre) * pre),
    as.numeric(post),
    costm
  )

  support_pre <- pre_df %>%
    dplyr::filter(count != 0) %>%
    dplyr::select({{var}}) %>%
    dplyr::distinct({{var}}) %>%
    dplyr::arrange({{var}}) %>%
    dplyr::filter(!is.na({{var}})) %>%
    unlist()

  support_post <- post_df %>%
    dplyr::filter(count != 0) %>%
    dplyr::select({{var}}) %>%
    dplyr::distinct({{var}}) %>%
    dplyr::arrange({{var}}) %>%
    dplyr::filter(!is.na({{var}})) %>%
    unlist()
  costm_ref <- build_costmatrix2(support_pre, support_post, bandwidth)

  temp <- as.data.frame(OT) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cost = costm_ref[from, to]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cost = mass * cost)

  tot_cost <- sum(temp$cost)
  prop_cost <- tot_cost / sum(post)
  list("num_bribe" = tot_cost, "prop_bribe" = prop_cost, "bandwidth" = bandwidth)
}

### Compute Results ---------------------------

#' @export
get_results <- function(pre_main = NULL, post_main = NULL,
                        pre_control = NULL, post_control = NULL,
                        bandwidth_seq = seq(0, 40000, 1000),
                        estimator = ifelse(!is.null(pre_control) & !is.null(post_control), "dit", "tc"),
                        conservative = F){

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
    message(est_message)
  } else if (estimator == "dit" | estimator == "differences-in-transports"){
    if (is.null(pre_control) | is.null(post_control)){
      message("`pre_control` and/or `post_control` is mising.")
    }
    est_message <- "Computing Differences-in-Transports Estimator..."
    est <- "dit"
    message(est_message)
  } else {
    stop("Invalid estimator. Choose 'ba' or 'dit' or double check inputs.")
  }

  if (conservative){
    message("Note: you are using `conservative = T`.")
  }

  # initialization
  main_prop <- rep(NA_real_, length(bandwidth_seq))
  if (conservative) maincons_prop <- rep(NA_real_, length(bandwidth_seq))
  if (est == "dit") control_prop <- rep(NA_real_, length(bandwidth_seq))

  # computation
  pb <- utils::txtProgressBar(min = 0, max = length(bandwidth_seq), initial = 0)
  for (i in seq_along(bandwidth_seq)){
    utils::setTxtProgressBar(pb, i)

    bandwidth <- bandwidth_seq[i]
    main_cost <- get_OTcost(pre_main, post_main, bandwidth = bandwidth)
    if (conservative) maincons_cost <- get_OTcost(pre_main, post_main, bandwidth = 2*bandwidth)
    if (est == "dit") control_cost <- get_OTcost(pre_control, post_control, bandwidth = bandwidth)

    main_prop[i] <- main_cost$prop_bribe
    if (conservative) maincons_prop[i] <- maincons_cost$prop_bribe
    if (est == "dit") control_prop[i] <- control_cost$prop_bribe
  }

  cat("\n")

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
      message(paste("The conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
    } else {
      out <- data.frame(bandwidth = bandwidth_seq,
                        main = main_prop,
                        control = control_prop,
                        diff = diffprop)
      whichmax <- which.max(diffprop)
      dit <- diffprop[whichmax]
      dstar <- bandwidth_seq[whichmax]
      message(paste("The non-conservative diff-in-transports estimator is ", dit, " at d = ", dstar, sep = ""))
    }
  }

  if (est == "tc"){
    out <- data.frame(bandwidth = bandwidth_seq,
                      main = main_prop)
    if (conservative) out <- cbind(out, maincons_prop)
    message(paste("The transport cost for the specified bandwidths have been computed."))
  }

  return(invisible(out))
}
