### Header ---------------------------
###
### Title: cleaning.R
###
### Description: Functions for cleaning data sets
###
### Author: Omkar A. Katta
###
### Contents:
###           SECTION 1: Preliminaries
###           SECTION 2: Import Raw Data
###           SECTION 3: Clean Raw Data
###               - create id
###               - filter cities
###               - select variables
###               - clean date
###               - identify treatment
###               - identify postCity
###           SECTION 4: Prepare MSRP Data
###               - goal: pre_*, post_*; * = global, Beijing, Tianjin, Shijiazhuang
###               - find unique support with upper/lower limits
###               - count MSRP
###               - get MSRPpmf (merge count MSRP with support; pre and post separately)
###               - uncount MSRP
###
###
### ---------------------------
###
### Notes:
###
###

### SECTION 1: Preliminaries ---------------------------

### SECTION 2: Import Raw Data ---------------------------

#' Import Raw Data
#'
#' Specify the director of the raw data and import it with data.table::fread.
#'
#' @param rawdata path to the raw csv file
#'
#' @return data.table
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' import_rawdata(rawdata)
import_rawdata <- function(rawdata){
  data.table::fread(rawdata)
}

check_data <- function(varnames = c("city"), data){
  temp <- lapply(varnames, function(x)
    if (!(x %in% names(data))){
      stop(paste("'", x, "' is not in data", sep = ""))
    })
  if (is.null(unlist(temp))) {message("All good")}
}

### SECTION 3: Clean Raw Data ---------------------------

create_id <- function(data){
  dplyr::mutate(data, id = seq_len(nrow(data)))
}

#' Filter Cities
#'
#' Choose which cities to keep for further analysis.
#'
#' @param data data.frame
#' @param cities character vector of cities to keep
#' @param create_dummy if TRUE, output will have dummy indicator columns for each
#'   valid city in \code{cities}
#'
#' @return An object of the same type as \type{data} but with fewer rows
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' filter_cities(data = bmp_raw)
filter_cities <- function(data,
                          cities = c("Beijing", "Tianjin", "Shijiazhuang"),
                          create_dummy = TRUE){
  if (!all(c("city") %in% names(data))) {
    stop("`data` must contain `city` columns")
  }
  if (!all(cities %in% unique(data$city))){
    warning("Some cities are not in `city` variable of `data`")
  }

  temp <- data %>%
    dplyr::filter(city %in% cities)
  if (create_dummy){
    temp <- temp %>%
      dplyr::mutate(city2 = city) %>%
      dplyr::mutate(unity = 1) %>%
      tidyr::pivot_wider(names_from = city2,
                         values_from = unity,
                         values_fill = 0)
  }
  temp
}


#' Select Variables
#'
#' Choose which variables to keep for further analysis. This is helpful to limit the memory and computational power needed to handle the data set.
#'
#' @param varnames character vector of variables names to keep
#' @param data data.frame or tibble
#'
#' @return An object of the same type as \code{data} but with fewer columns
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' select_vars(data = bmp_raw)
select_vars <- function(data,
                        varnames = c("id",
                                     "date",
                                     "city",
                                     "gdp_per_capita",
                                     "total_license",
                                     "lottery_license",
                                     "totsales",
                                     "sales",
                                     "lottery_bidding",
                                     "year",
                                     "month",
                                     "wtprice",
                                     "swtprice",
                                     "MSRP",
                                     "noticenum",
                                     "color",
                                     "policy_dummy",
                                     "transmission",
                                     "category",
                                     "gear")){
  if (!all(varnames %in% names(data))){
    stop("Some variables in `varnames` are not in `data`")
  }
  data %>%
    dplyr::select({{varnames}})
}


#' Clean Date
#'
#' @param data data.frame or tibble with 4-digit \code{year} and 2-digit \code{month} columns
#'
#' @return An object of the same type as \code{data} with one additional column \code{ym}
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' clean_date(data = bmp_raw)
clean_date <- function(data){
  if (!all(c("year", "month") %in% names(data))){
    stop("Either `year` or `month` or both variables are not in `data`")
  }
  if (!all(as.numeric(data$year) == as.integer(data$year))){
    stop("The values in `year` should be integers")
  }
  if (!all(as.numeric(data$month) == as.integer(data$month))){
    stop("The values in `month` should be integers")
  }
  if (!all(as.numeric(data$year) > 2000 & as.numeric(data$year) < data.table::year(Sys.Date()))){
    stop(paste("The values in `year` are not within 2000 and ", data.table::year(Sys.Date()), sep = ""))
  }
  if (!all(as.numeric(data$month) >= 1 & as.numeric(data$month) <= 12)){
    stop("The values in `month` are not within 1 and 12")
  }
  data %>%
    # dplyr::mutate(temp = dplyr::if_else(month < 10,
    #                                     paste("0", month, sep = ""),
    #                                     as.character(month))) %>%
    # dplyr::mutate(temp = paste(year, month, "01", sep = "-"))  %>%
    dplyr::mutate(ym = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d"))

}


#' Character Indicator for treatment
#'
#' Create a character vector that specifies "pre-lottery" when \code{policy_dummy}
#' is zero and "post-lottery" when \code{policy_dummy} is one.
#'
#' @param data data.table or tibble with indicator variable called \code{policy_dummy}
#'
#' @return An object of the same type as \code{data} with additional column `postBeijing`
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' identify_treatment(data = bmp_raw)
identify_treatment <- function(data){
  if (!("policy_dummy" %in% names(data))){
    stop("The variable `policy_dummy` is not in `data`")
  }
  if (all(sort(unique(data$policy_dummy)) != c(0, 1))){
    stop("The variable `policy_dummy` needs to be a binary variable with numeric values of 0 and 1")
  }
  dplyr::mutate(data,
                post = dplyr::if_else(policy_dummy == 0, "pre-lottery", "post-lottery"))
}


identify_postCity <- function(data, treatment_date, newname){
  # if (!("policy_dummy" %in% names(data))){
  #   stop("The variable `policy_dummy` is not in `data`")
  # }
  # if (all(sort(unique(data$policy_dummy)) != c(0, 1))){
  #   stop("The variable `policy_dummy` needs to be a binary variable with numeric values of 0 and 1")
  # }
  data %>%
    dplyr::mutate(temp = dplyr::if_else(ym >= as.Date(treatment_date), 1, 0)) %>%
    dplyr::rename({{newname}} := temp)
}

### SECTION 4: Prepare MSRP Data ---------------------------

#' Get support of numeric vector
#'
#' Obtain the values of a numeric vector for which there is more than one
#' observation in the data set. You can also truncate the result with \code{lowerlim}
#' and \code{upperlim}.
#'
#' @param data data.table or tibble with the numeric variable \code{var}, e.g., MSRP
#' @param var the string name of the variable for which we need the support
#' @param lowerlim lower limit of the support
#' @param upperlim upper limit of the support
#'
#' @return A numeric vector whose values are greater than \code{lowerlim} and no
#'   more than \code{upperlim}
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' get_support(rawdata, var = MSRP, lowerlim = 0, upperlim = 5e6)
get_support <- function(data, var = "MSRP", lowerlim = 0, upperlim = Inf){
  if (!(var %in% names(data))){
    stop("The variable `var` is not in `data`")
  }
  temp <- data %>% .[[var]]
  if (!(is.numeric(temp))){
    stop("The variable `var` is not numeric")
  }
  if (sum(is.na(temp)) > 0){
    warning("The variable `var` has NA values")
  }
  temp %>%
    unique %>%
    sort %>%
    .[. > lowerlim & .< upperlim]
}


#' Get counts for a variable
#'
#' Given a data set that contains the variable \code{var} of interest and its count \code{count},
#' \code{get_counts} provides the total count for each instance of \code{var}.
#' This is particularly useful when there are multiple instances of \code{var}
#' in the data set.
#' The intention is that this function is used to create the PMF of a variable.
#'
#' @param data data.frame or tibble with \code{var}
#' @param var variable in \code{data} for which we want the total counts
#' @param count a numeric column in \code{data} with the counts
#' @param indvar a binary column that indicates treatment status
#' @param lowerlim a numeric scalar, defaults to 0
#' @param upperlim a numeric scalar, defaults to Inf
#'
#' @return a list of two data sets, each containing the counts of \code{var} by \code{indvar},
#'   one for pre-treatment and another for post-treatment
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' get_counts(rawdata, upperlim = 5e6)
get_counts <- function(data, var = "MSRP", count = "sales", indvar = "policy_dummy",
                       lowerlim = 0, upperlim = Inf, na.rm = T){
  if (!all(c(var, count, indvar) %in% names(data))){
    stop("The `data` does not contain `var`, `count`, and `indvar`")
  }
  if (!is.numeric(data %>% .[[count]])){
    stop("The `count` variable needs to be numeric")
  }
  if (all(data %>% .[[indvar]] %>% unique %>% sort != c(0, 1))){
    stop("The variable `policy_dummy` needs to be a binary variable with values of 0 and 1")
  }
  if (!all(data %>% .[[indvar]] %>% is.numeric)) {
    message("By the way, `indvar` is not numeric")
  }
  temp <- data %>% .[[var]]
  if (sum(is.na(temp)) > 0){
    warning("The variable `var` has NA values")
  }
  temp <- data %>%
    dplyr::group_by(dplyr::across(c({{var}}, {{indvar}}))) %>%
    dplyr::summarise_at({{count}}, sum) %>%
    dplyr::rename(count = {{count}}) %>%
    .[as.vector(.[[var]] < upperlim & .[[var]] > lowerlim), ]
  if (na.rm){
    # remove NA and separate into pre-lottery and post-lottery
    pre <- temp[as.vector(temp[, indvar] == 0 & (!is.na(temp[, var]))), ]
    post <- temp[as.vector(temp[, indvar] == 1 & (!is.na(temp[, var]))), ]
  } else {
    # separate into pre-lottery and post-lottery
    pre <- temp[as.vector(temp[, indvar] == 0), ]
    post <- temp[as.vector(temp[, indvar] == 1), ]
  }
  list(pre, post)
}


#' Get MSRP PMF
#'
#' Comparing the support of MSRP and the counts for each nontrivial instance of
#' MSRP in the pre- and post-treatment data sets, \code{get_MSRPpmf} creates the
#' probability mass function of MSRP.
#'
#' @param support Numeric vector with the support of MSRP; see \code{get_support}
#' @param counts List of pre- and post-treatment counts of MSRP; see \code{get_counts}
#'
#' @return List of pre- and post-treatment MSRP data sets with the domain and range of PMF
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' support <- get_support(rawdata, var = MSRP, lowerlim = 0, upperlim = 5e6)
#' counts <- get_counts(rawdata, var = MSRP, lowerlim = 0, upperlim = 5e6)
#' get_MSRPpmf(support, counts)
get_MSRPpmf <- function(support, counts){
  if (!is.numeric(support)){
    stop("Make sure `support` is a numeric vector")
  }
  if (length(counts) != 2){
    stop("Make sure `counts` has two entries")
  }
  if (!all(c("MSRP", "count") %in% names(counts[[1]]))){
    stop("Make sure `count[[1]]` has `MSRP` and `count` columns")
  }
  if (!all(c("MSRP", "count") %in% names(counts[[2]]))){
    stop("Make sure `count[[2]]` has `MSRP` and `count` columns")
  }
  lapply(counts,
         function(x) dplyr::left_join(data.frame(MSRP = support),
                                      x) %>%
           dplyr::select(MSRP, count) %>%
           tidyr::replace_na(list(count = 0)) %>%
           tibble::as_tibble())
}

#' Create a univariate distribution of MSRP
#'
#' From the PMF of MSRP, \code{uncount_MSRPpmf} creates a vector where each
#' instance of MSRP is repeated as many times as dictated by the PMF.
#'
#' @param MSRPpmf a list of two data.frames, each with the domain and range of
#'   the PMF of MSRP
#'
#' @return a list of pre- and post-treatment data.frames/tibbles
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' support <- get_support(rawdata, var = MSRP, lowerlim = 0, upperlim = 5e6)
#' counts <- get_counts(rawdata, var = MSRP, lowerlim = 0, upperlim = 5e6)
#' MSRPpmf <- get_MSRPpmf(support, counts)
#' MSRPdist <- uncount_MSRPpmf(MSPRpmf)
uncount_MSRPpmf <- function(MSRPpmf){
  if (length(MSRPpmf) != 2){
    stop("Make sure `counts` has two entries")
  }
  if (!all(c("count") %in% names(MSRPpmf[[1]]))){
    stop("Make sure `count[[1]]` has `count` columns")
  }
  if (!all(c("count") %in% names(MSRPpmf[[2]]))){
    stop("Make sure `count[[2]]` has `count` columns")
  }
  lapply(MSRPpmf,
         function(x) x %>% tidyr::uncount(count))
}



separate_list <- function(list){
  for (i in seq_along(list)){
    name <- names(list)[[i]]
    newname <- paste("dist", name, "2", sep = "_")
    assign(newname,
           Reduce(rbind,
                  lapply(seq_along(list[[i]]),
                         function(x) dplyr::mutate(list[[i]][[x]], policy_dummy = x - 1))),
           envir = .GlobalEnv)
  }
}

retrieve <- function(list, name, time){
  if (time == "pre"){
    index <- 1
  } else if (time == "post"){
    index <- 2
  }
  list[[name]][[index]]
}

### SECTION 5: Prepare MSRP Data ALT ---------------------------

#' Filter Data by Variable
#'
#' \code{filter_var} filters the data so that the values of a variable \code{var}
#' are at least \code{lowerlim} but no more than \code{upperlim}.
#' This is a generalization of \code{filter_date}.
#'
#' @param data data.frame or tibble with \code{var}
#' @param var a column in \code{data}
#' @param lowerlim an inclusive lower bound on \code{var}
#' @param upperlim an inclusive lower bound on \code{var}
#'
#' @return
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' bmp_date <- clean_var(bmp_raw, var = MSRP, lowerlim = 0, upperlim = 5e6)
filter_var <- function(data, var, lowerlim = 0, upperlim = Inf){
  data %>%
    dplyr::filter({{var}} >= lowerlim & {{var}} < upperlim)
}

#' Filter Data by Date
#'
#' \code{filter_date} helps us focus analysis to a particular time frame, e.g.,
#' before the treatment or after the treatment. This is a particular example of
#' \code{clean_var} that is specific to dates.
#'
#' @param data data.frame or tibble with \code{datevar}
#' @param datevar a column in \code{data} of class "Date"
#' @param lowerdate an inclusive lower bound on \code{datevar}
#' @param upperdate an exclusive upper bound on \code{datevar}
#' @param format describes the date format. Default is yyyy-mm-dd, i.e., "%Y-%m-%d"
#'
#' @return An object of the same type as \code{data} with fewer observations whose
#'   \code{datevar} values are on and after \code{lower_date} but before \code{upper_date}.
#' @export
#'
#' @examples
#' rawdata <- "./data-raw/GuisChineseCities (1).csv"
#' bmp_raw <- import_rawdata(rawdata)
#' bmp_date <- clean_date(bmp_raw)
#' filter_dates(bmp_date, datevar = ym, lower_date = "2010-01-01", upper_date = "2011-01-01")
filter_date <- function(data, datevar, lowerdate, upperdate, format = "%Y-%m-%d"){
  data %>%
    dplyr::filter({{datevar}} >= as.Date(lowerdate, format) & {{datevar}} < as.Date(upperdate, format))
}



#' Analyze Data
#'
#' This workhorse function retrieves the support of a variable, the mass of a variable
#' that has an associated count variable, and the resulting univariate, empirical distribution.
#'
#' @param data a data.frame or tibble
#' @param analysis a character string ("support", "counts", "pmf", "dist") that
#'   specifies what kind of analysis you want.
#' @param var the variable of interest; defaults to MSRP
#' @param support a vector of unique values that is the support of \code{var}.
#'   If NA, then support is computed by using a cleaned up version of \code{data}
#' @param count a numeric variable specifying how many instances exist of the value in \code{var}
#' @param datevar a column in \code{data} of class "Date"
#' @param lowerdate an inclusive lower bound on \code{datevar}; defaults to -Inf
#' @param upperdate an exclusive upper bound on \code{datevar}; defaults to Inf
#' @param format describes the date format. Default is yyyy-mm-dd, i.e., "%Y-%m-%d"
#'
#' @return depends on the \code{analysis} desired
#' @export
#'
#' @examples
#' # After obtaining Beijing data, if we want to find the distribution of MSRP
#' # between 2010 and 2011:
#' get_analysis(Beijing, "dist", lowerdate = "2010-01-01", upperdate = "2011-01-01")
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
      tidyr::replace_na(list(count = 0)) %>%
      tibble::as_tibble()
    if (analysis == "pmf"){
      return(pmf)
    }
  }

  if (analysis %in% c("dist")){
    pmf %>%
      tidyr::uncount(count)
  }

}



