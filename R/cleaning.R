### Header ---------------------------
###
### Title: cleaning.R
###
### Description: Functions for cleaning data
###
### Author: Omkar A. Katta
###
### Notes:
###
###

### aux functions ---------------------------

filter_date <- function(data, datevar, lowerdate, upperdate, format = "%Y-%m-%d"){
  data %>%
    dplyr::filter({{datevar}} >= as.Date(lowerdate, format) & {{datevar}} < as.Date(upperdate, format))
}

### get_analysis: all-in-one ---------------------------

#' @importFrom tidyr uncount
#' @importFrom tidyr replace_na
#' @importFrom tibble as_tibble
#' @importFrom rlang enquo
get_analysis <- function(data, analysis,
                         var = MSRP,
                         support = NA,
                         count = sales,
                         datevar = ym,
                         lowerdate = as.Date(-Inf), upperdate = as.Date(Inf), format = "%Y-%m-%d"){



  if (analysis %in% c("support", "pmf", "dist")){
    if ((all(is.na(support)))){ # if there is no support provided
      datevar = enquo(datevar)
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
    } else {
      support = support
    }

    if (analysis == "support"){
      return(support)
    }
  }

  if (analysis %in% c("counts", "pmf", "dist")){
    datevar <- enquo(datevar)
    counts <- data %>%
      filter_date(datevar = !!datevar,
                  lowerdate = lowerdate,
                  upperdate = upperdate,
                  format = format) %>%
      dplyr::group_by(dplyr::across(c({{var}}))) %>%
      dplyr::summarise(count = sum({{count}})) %>%
      dplyr::filter(!is.na({{var}}))
    if (analysis == "counts"){
      return(counts)
    }
  }

  if (analysis %in% c("pmf", "dist")){
    support <- data.frame(temp = support) %>%
      dplyr::rename("{{var}}" := temp)
    pmf <- dplyr::left_join(support, counts) %>%
      dplyr::select({{var}}, count) %>%
      replace_na(list(count = 0)) %>%
      as_tibble()
    if (analysis == "pmf"){
      return(pmf)
    }
  }

  if (analysis %in% c("dist")){
    pmf %>%
      uncount(count)
  }

}
