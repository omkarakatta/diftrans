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

### prep_data: all-in-one ---------------------------

#' Prepare Data
#'
#' Retrieve the support, the probability mass function, and the distribution
#' of a variable.
#'
#' The tibble \code{data} must have a variable of interest \code{var} and the number of
#' occurrences of each value of \code{var} in some column \code{count}.
#' Note that values of \code{var} can be repeated within the data set. To obtain
#' the unique values in \code{var}, use \code{prep = "support"}.
#'
#' Using \code{prep = "pmf"} can sum the values of \code{count} by the
#' unique values of \code{var}. As a result, we obtain a table where each row contains
#' a unique value of \code{var} and the *total* number of times that it appears in \code{data}.
#'
#' The optional argument \code{support} is a vector that specifies all the possible
#' values of \code{var}. Using this with \code{prep = "pmf"} will give you table
#' of counts for each value of \code{support}. Thus, the difference between using
#' \code{prep = "pmf"} and using \code{prep = "pmf"} with \code{support} is that
#' it is possible for the output of the latter to contain counts of 0.
#'
#' To go one step further, \code{prep = "dist"} takes repeats each value of the
#' \code{support} according to the number of times it appears in \code{data}.
#'
#' @param data a tibble with counts (\code{count}) of the variable of interest (\code{var})
#' @param prep specifies type of preparation: "support", "counts", "pmf", "dist"
#' @param var variable of interest; default is \code{MSRP} (see Daljord et al. (2020))
#' @param support optionally define support of \code{var}
#' @param count number of occurrences of \code{var}; default is \code{sales} (see Daljord et al. (2020))
#' @param datevar variable of class "Date"
#' @param lowerdate an inclusive lower bound on \code{datevar} given as a string; default is -Inf
#' @param upperdate an exclusive upper bound on \code{datevar} given as a string; default is +Inf
#' @param format specifies the format of \code{lowerdate} and \code{upperdate}, default is "%Y-%m-%d"
#'
#' @return depends on \code{prep}
#'
#' \itemize{
#'   \item If \code{prep == "support"}, then the output will be a vector of
#'       unique values of \code{var} in ascending order
#'   \item If \code{prep == "pmf"}, then the output will be a tibble with two colums:
#'       column 1 contains the values specified in \code{support} and column 2
#'       contains the sum of \code{counts} in \code{data} corresponding to the value
#'       in \code{support}.
#'   \item If \code{prep = "dist"}, then the output will be vector whose values
#'       are repetitions of the \code{var} according to \code{counts}.
#'   }
#'
#' @examples
#' # Obtain unique values of MSRP between 2010 and 2011 in Beijing and in Tianjin:
#' support_Beijing <- prep_data(Beijing_cleaned, "support",
#'                              lowerdate = "2010-01-01", upperdate = "2011-01-01")
#' support_Beijing
#'
#' # Aggregate total sales for each value of MSRP between 2010 and 2011 in Tianjin:
#' count <- prep_data(Tianjin_cleaned, "pmf", lowerdate = "2010-01-01", upperdate = "2011-01-01")
#' count
#'
#' # Obtain PMF of MSRP values between 2010 and 2011 in Tianjin:
#' pmf <- prep_data(Tianjin_cleaned, "pmf", lowerdate = "2010-01-01", upperdate = "2011-01-01")
#' pmf
#' # Note: \code{pmf} should be same as \code{count} because support is the same.
#' all.equal(pmf, count)
#'
#' # Obtain PMF of MSRP values between 2010 and 2011 in Tianjin using full support:
#' full_support <- prep_data(Tianjin_cleaned, "support")
#' pmf2 <- prep_data(Tianjin_cleaned, "pmf", support = full_support,
#'                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
#' # Since support is different between \code{pmf2} and \code{count}, there will
#' # be some values of MSRP whose \code{count} is 0.
#'
#' # Obtain the distribution of MSRP values between 2010 and 2011 in Tianjin:
#' dist <- prep_data(Tianjin_cleaned, "dist", lowerdate = "2010-01-01", upperdate = "2011-01-01")
#' dist
#' # Compare \code{dist} with \code{count}
#'
#' @importFrom tidyr uncount
#' @importFrom tidyr replace_na
#' @importFrom tibble as_tibble
#' @importFrom rlang enquo
#' @importFrom rlang enexpr
#' @importFrom stats complete.cases
#' @export
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
    datevar <- enquo(datevar)
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
      replace_na(list(count = 0)) %>%
      as_tibble()
    if (prep == "pmf"){
      return(pmf)
    }
  }

  if (prep %in% c("dist")){
    pmf %>%
      uncount(count)
  }

}
