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
###           SECTION 4: Prepare MSRP Data
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

### SECTION 4: Prepare Data ---------------------------

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


