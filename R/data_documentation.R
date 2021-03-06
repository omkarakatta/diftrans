#' Beijing car prices from 2010 to 2015
#'
#' A dataset containing the manufacturer's suggested retail price (MSRP) of car
#' models and how many of these models were sold in Beijing between 2010 and 2015
#' as well as other information.
#'
#' Due to privacy restrictions, we cannot distribute the original data sets.
#' This data set is an anonymized version of the original.
#'
#' @format a tibble with 217155 rows and 16 variables
#' \describe{
#'   \item{id}{a number that uniquely identifies each Beijing car model by year}
#'   \item{year}{year, as an integer between 2010 and 2015}
#'   \item{month}{month, as an integer between 1 and 12}
#'   \item{ym}{A Date-class variable of the format yyyy-mm-01}
#'   \item{city}{"Beijing"}
#'   \item{MSRP}{manufacturer's suggested retail price, in RMB}
#'   \item{sales}{number of car models sold in that particular year}
#'   \item{swtprice}{price, in RMB 1000}
#'   \item{color, noticenum}{uniquely identify car model}
#'   \item{postBeijing}{1 if cars were sold after Beijing lottery in 2011, and 0 otherwise}
#'   \item{postTianjin}{1 if cars were sold after Tianjin lottery/auction in 2014, and 0 otherwise}
#'   \item{Beijing}{1 if cars were sold in Beijing, which is every car in this dataset}
#'   \item{Tianjin}{1 if cars were sold in Tianjin, which is no car in this dataset}
#'   \item{Shijiazhuang}{1 if cars were sold in Shijiazhuang, which is no car in this dataset}
#'   \item{policy_dummy}{1 if cars were sold after lottery in 2011, same as postBeijing}
#' }
#'
#' @source This data set mimics the properties of the data set used in Daljord et al. (2021).
#'
"Beijing_sample"

#' Tianjin car prices from 2010 to 2015
#'
#' A dataset containing the manufacturer's suggested retail price (MSRP) of car
#' models and how many of these models were sold in Tianjin between 2010 and 2015
#' as well as other information.
#'
#' Due to privacy restrictions, we cannot distribute the original data sets.
#' This data set is an anonymized version of the original.
#'
#' @format a tibble with 194468 rows and 16 variables
#' \describe{
#'   \item{id}{a number that uniquely identifies each Tianjin car model by year}
#'   \item{year}{year, as an integer between 2010 and 2015}
#'   \item{month}{month, as an integer between 1 and 12}
#'   \item{ym}{A Date-class variable of the format yyyy-mm-01}
#'   \item{city}{"Tianjin"}
#'   \item{MSRP}{manufacturer's suggested retail price, in RMB}
#'   \item{sales}{number of car models sold in that particular year}
#'   \item{swtprice}{price, in RMB 1000}
#'   \item{color, noticenum}{uniquely identify car model}
#'   \item{postBeijing}{1 if cars were sold after Beijing lottery in 2011, and 0 otherwise}
#'   \item{postTianjin}{1 if cars were sold after Tianjin lottery/auction in 2014, and 0 otherwise}
#'   \item{Beijing}{1 if cars were sold in Beijing, which is no car in this dataset}
#'   \item{Tianjin}{1 if cars were sold in Tianjin, which is every car in this dataset}
#'   \item{Shijiazhuang}{1 if cars were sold in Shijiazhuang, which is no car in this dataset}
#'   \item{policy_dummy}{1 if cars were sold after lottery/auction in 2014, same as postTianjin}
#' }
#'
#' @source This data set mimics the properties of the data set used in Daljord et al. (2021).
#'
"Tianjin_sample"

#' Shijiazhuang car prices from 2010 to 2015
#'
#' A dataset containing the manufacturer's suggested retail price (MSRP) of car
#' models and how many of these models were sold in Shijiazhuang between 2010 and 2015
#' as well as other information.
#'
#' Due to privacy restrictions, we cannot distribute the original data sets.
#' This data set is an anonymized version of the original.
#'
#' @format a tibble with 194468 rows and 16 variables
#' \describe{
#'   \item{id}{a number that uniquely identifies each Shijiazhuang car model by year}
#'   \item{year}{year, as an integer between 2010 and 2015}
#'   \item{month}{month, as an integer between 1 and 12}
#'   \item{ym}{A Date-class variable of the format yyyy-mm-01}
#'   \item{city}{"Shijiazhuang"}
#'   \item{MSRP}{manufacturer's suggested retail price, in RMB}
#'   \item{sales}{number of car models sold in that particular year}
#'   \item{swtprice}{price, in RMB 1000}
#'   \item{color, noticenum}{uniquely identify car model}
#'   \item{postBeijing}{1 if cars were sold after Beijing lottery in 2011, and 0 otherwise}
#'   \item{postTianjin}{1 if cars were sold after Tianjin lottery/auction in 2014, and 0 otherwise}
#'   \item{Beijing}{1 if cars were sold in Beijing, which is no car in this dataset}
#'   \item{Tianjin}{1 if cars were sold in Tianjin, which is no car in this dataset}
#'   \item{Shijiazhuang}{1 if cars were sold in Shijiazhuang, which is every car in this dataset}
#'   \item{policy_dummy}{0 because there is no policy enacted in Shijiazhuang between 2010 and 2015}
#' }
#'
#' @source This data set mimics the properties of the data set used in Daljord et al. (2021).
#'
"Shijiazhuang_sample"

