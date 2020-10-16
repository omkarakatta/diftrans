context("get_OTcost")

filter_date <- function(data, datevar, lowerdate, upperdate, format = "%Y-%m-%d"){
  data %>%
    dplyr::filter({{datevar}} >= as.Date(lowerdate, format) & {{datevar}} < as.Date(upperdate, format))
}
prep_data <- function(data, prep,
                      var = MSRP,
                      support = NULL,
                      count = sales,
                      datevar = ym,
                      lowerdate = as.Date(-Inf), upperdate = as.Date(Inf), format = "%Y-%m-%d"){
  # error checking
  if (!prep %in% c("support", "pmf", "dist")){
    stop("`prep` must be one of three values: 'support', 'pmf', or 'dist'.")
  }
  if (prep == "support" & !is.null(support)){
    stop("`support` is already specified in function. Are you sure you want `prep = 'support'`?")
  }

  # get support
  if (prep %in% c("support", "pmf", "dist")){
    if ((all(is.na(support)))){ # if there is no support provided
      datevar = rlang::enquo(datevar)
      support <- data %>%
        filter_date(datevar = !!datevar,
                    lowerdate = lowerdate,
                    upperdate = upperdate,
                    format = format) %>%
        dplyr::select({{var}}) %>%
        dplyr::distinct({{var}}) %>%
        dplyr::arrange({{var}}) %>%
        dplyr::filter(!is.na({{var}})) %>%
        unlist()
      message("`support` has been created.")
    } else {
      oksupport <- complete.cases(support)
      isna <- sum(!oksupport)
      if (isna > 0) message("NAs removed from `support`.")
      support <- support[oksupport]
    }

    if (prep == "support"){
      message("`count` will be ignored.")
      return(support)
    }
  }

  # get counts
  if (prep %in% c("pmf", "dist")){
    datevar <- rlang::enquo(datevar)
    counts <- data %>%
      filter_date(datevar = !!datevar,
                  lowerdate = lowerdate,
                  upperdate = upperdate,
                  format = format) %>%
      dplyr::group_by(dplyr::across(c({{var}}))) %>%
      dplyr::summarise(count = sum({{count}})) %>%
      dplyr::filter(!is.na({{var}}))
  }

  # get pmf
  if (prep %in% c("pmf", "dist")){
    support <- data.frame(temp = support) %>%
      dplyr::rename("{{var}}" := temp)
    pmf <- dplyr::left_join(support, counts) %>%
      dplyr::select({{var}}, count) %>%
      tidyr::replace_na(list(count = 0)) %>%
      tibble::as_tibble()
    if (prep == "pmf"){
      return(pmf)
    }
  }

  if (prep %in% c("dist")){
    pmf %>%
      tidyr::uncount(count)
  }

}

Beijing <- Beijing_sample

common_support <- prep_data(Beijing, prep = "support", lowerdate = "2010-01-01", upperdate = "2012-01-01")

pre <- prep_data(Beijing, prep = "pmf",
                 support = common_support,
                 lowerdate = "2010-01-01", upperdate = "2011-01-01")
post <- prep_data(Beijing, prep = "pmf",
                  support = common_support,
                  lowerdate = "2011-01-01", upperdate = "2012-01-01")
pre_support <- unique(pre$MSRP[pre$count != 0 & !is.na(pre$MSRP)])
post_support <- unique(post$MSRP[post$count != 0 & !is.na(post$MSRP)])

costm <- build_costmatrix(common_support, 0)
costm_ref <- build_costmatrix2(pre_support, post_support, 0)

test_that("manually specifying cost works", {
  expect_equal(get_OTcost(pre, post, bandwidth = 0, costmat = costm, costmat_ref = costm_ref), 
               get_OTcost(pre, post, bandwidth = 0))
})




