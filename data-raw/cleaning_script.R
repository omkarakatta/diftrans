### Header ---------------------------
###
### Title: cleaning_script.R
###
### Description: Apply the cleaning_functions with the prepared data from cleaned_data.R
###
### Author: Omkar A. Katta
###
###
### Notes: The prepared data and specifications from cleaned_data.R are the input
###   to this script. Details about each step is provided above the respective
###   code. The result of this script is cleaned datasets labeled:
###   - bmp_cleaned
###   - global
###   - BTS
###   - Beijing
###   - Tianjin
###   - Shijiazhuang
###
###

# import raw data
bmp_raw <- import_rawdata(rawdata_path)

# prepare data depending on version
temp_nrow <- nrow(bmp_raw)
if (version == "v1"){
  bmp_raw <- bmp_raw %>% # remove Dec 2010
    dplyr::filter(!(year == 2010 & month == 12))
  stopifnot(nrow(bmp_raw) < temp_nrow)
}
if (version == "v2"){
  bmp_raw <- bmp_raw %>% # remove Dec 2010, Jan 2011, Feb 2011
    dplyr::filter(!(year == 2010 & month == 12)) %>%
    dplyr::filter(!(year == 2011 & month %in% c(1, 2)))
  stopifnot(nrow(bmp_raw) < temp_nrow)
}


# filter `MSRP` to be within limits
# create unique identifier
# format `year` and `month` to create the date variable `ym`
bmp_cleaned <- bmp_raw %>%
  filter_var(var = MSRP, lowerlim = MSRP_lowerlim, upperlim = MSRP_upperlim) %>%
  create_id() %>%
  clean_date()

message("`bmp_raw` is filtered and cleaned. The resulting object is `bmp_cleaned`")

# identify if observation is pre- or post-lottery with respect to Beijing/Tianjin
global <- bmp_cleaned %>%
  identify_postCity(treatment_date = Beijing_treatment_date, "postBeijing") %>%
  identify_postCity(treatment_date = Tianjin_treatment_date, "postTianjin")

message("`global` is created.")

# choose the important cities and create dummy variables for them
BTS <- filter_cities(global,
                     cities = c("Beijing", "Tianjin", "Shijiazhuang"),
                     create_dummy = T)

message("`BTS` is created.")

# create city-specific data sets

Beijing <- filter_cities(BTS,
                         cities = c("Beijing"),
                         create_dummy = F)

message("`Beijing` is created.")

Tianjin <- filter_cities(BTS,
                         cities = c("Tianjin"),
                         create_dummy = F)

message("`Tianjin` is created.")

Shijiazhuang <- filter_cities(BTS,
                              cities = c("Shijiazhuang"),
                              create_dummy = F)

message("`Shijiazhuang` is created.")
