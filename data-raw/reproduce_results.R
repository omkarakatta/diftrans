### Header ---------------------------
###
### Title: reproduce_results.R
###
### Description: One-click R script to reproduce results
###
### Author: Omkar A. Katta
###
### Notes: User can change information in preliminaries as needed to reproduce the
###       analysis in Daljord et al. (2020).
###
###

### Preliminaries ---------------------------

# Install Packages

library(diftrans)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)

# What data do you want to use?
# Option 1: "original" i.e. use entire data set
# Option 2: "v1" i.e. exclude December 2010
# Option 3: "v2" i.e. exclude December 2010, January 2011, and February 2012

version <- "v2" # options are: "original" "v1" "v2"

# Do you want to see figures?
show_fig <- TRUE
show_fig1 <- FALSE
show_fig2 <- FALSE
show_fig3 <- FALSE
show_fig4 <- FALSE
show_fig5 <- FALSE
show_fig6 <- FALSE
show_fig7 <- FALSE
show_fig8 <- FALSE
show_fig9 <- FALSE
show_fig10 <- FALSE
show_fig11 <- FALSE

# Do you want to save figures?
save_fig <- FALSE
save_fig1 <- FALSE
save_fig2 <- FALSE
save_fig3 <- FALSE
save_fig4 <- FALSE
save_fig5 <- FALSE
save_fig6 <- FALSE
save_fig7 <- FALSE
save_fig8 <- FALSE
save_fig9 <- FALSE
save_fig10 <- FALSE
save_fig11 <- FALSE

# Do you want to run diff-in-diff analysis
show_diff_in_diff <- F

# Plotting Preferences

grayscale <- FALSE # toggle to TRUE for black-and-white plots
temp <- ifelse(grayscale, "grayscale", "color")

fontsize <- 20 # change font size of plot, axis, and legend titles
fontsizeaxis <- 15 # change font size of axis labels
linetype0 <- "solid" # main line type
linetype1 <- "dashed" # secondary line type
linetype2 <- "dotted" # tertiary line type

# Where will you store plots?
# img_path <- ... # uncomment and replace ... with destination file path
# suffix <- "_"  # naming convention

# How large do you want your plots to be? (inches)
# default_width <- 7
# default_height <- 3

### Prepare Data ---------------------------

if (version == "original"){
  Beijing <- Beijing_cleaned
  Tianjin <- Tianjin_cleaned
  Shijiazhuang <- Shijiazhuang_cleaned
  seedplus <- 0 # i.e. do not modify seed
  message(paste("Version: ", version, sep = ""))
} else if (version == "v1"){
  # filter out Dec 2010
  Beijing <- Beijing_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12))
  Tianjin <- Tianjin_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12))
  Shijiazhuang <- Shijiazhuang_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12))
  seedplus <- 1
  message(paste("Version: ", version, sep = ""))
} else if (version == "v2"){
  # filter out Dec 2010, Jan 2011, and Feb 2011
  Beijing <- Beijing_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12)) %>%
    dplyr::filter(!(year == 2011 & month %in% c(1, 2)))
  Tianjin <- Tianjin_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12)) %>%
    dplyr::filter(!(year == 2011 & month %in% c(1, 2)))
  Shijiazhuang <- Shijiazhuang_cleaned %>%
    dplyr::filter(!(year == 2010 & month == 12)) %>%
    dplyr::filter(!(year == 2011 & month %in% c(1, 2)))
  seedplus <- 2
  message(paste("Version: ", version, sep = ""))
} else {
  stop("Specify a valid value for `version`.")
}

### Figure 1 ---------------------------
fignum <- 1
if (show_fig | show_fig1){
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  fig1_prep <- BTS %>%
    select(ym, city, swtprice) %>%
    group_by(ym, city) %>%
    summarize(swtprice = mean(swtprice, na.rm = T)) %>%
    ungroup()
  fig1_plot <- ggplot() +
    bmp_vline(xint = as.Date("2011-01-01")) +
    bmp_vline(xint = as.Date("2014-01-01")) +
    bmp_point(x = ym,
              y = swtprice,
              data = fig1_prep,
              color = factor(city, levels = c("Beijing", "Tianjin", "Shijiazhuang")),
              size = 1.5,
              alpha = 0.5,
              show.legend = F) +
    bmp_plot(data = fig1_prep,
             color = city,
             size = 0.5,
             xlab = "Date (months)", ylab = "Price (RMB 1000)",
             xtype = "ym",
             ytype = "continuous",
             ybreaks = seq(100, 225, 20),
             ylabels = seq(100, 225, 20),
             legend.position = c(0.1, 0.85),
             legend.direction = "vertical",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5)) +
    geom_line(data = fig1_prep, aes(x = ym, y = swtprice,
                                    color = factor(city, levels = c("Beijing", "Tianjin", "Shijiazhuang")),
                                    linetype = factor(city, levels = c("Beijing", "Tianjin", "Shijiazhuang"))),
              size = 0.5) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          name = "")
  if (save_fig | save_fig1){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }
}
message(paste("Figure ", fignum, " is complete.", sep = ""))

### Figure 2 ---------------------------
fignum <- 2
if (show_fig | show_fig2){
  pre <- prep_data(Beijing, prep = "counts",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- Beijing %>% # find prices of new cars in 2011
    filter(ym < as.Date("2012-01-01")) %>%
    pivot_wider(id_cols = c(month, color, noticenum),
                       names_from = year,
                       values_from = MSRP) %>%
    filter(is.na(`2010`)) %>%
    rename(MSRP = `2011`) %>%
    select(MSRP)
  fig2_plot <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                x = log(MSRP), scale = 1, binwidth = 1/6) +
    stat_function(data = pre,
                  mapping = aes(x = log(MSRP)),
                  color = get_color_palette(2, grayscale)[1],
                  fun = dnorm,
                  args = list(mean = mean(log(pre$MSRP)),
                              sd = sd(log(pre$MSRP)))) +
    stat_function(data = post,
                  mapping = aes(x = log(MSRP)),
                  color = get_color_palette(2, grayscale)[2],
                  fun = dnorm,
                  args = list(mean = mean(log(post$MSRP)),
                              sd = sd(log(post$MSRP)))) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xlab = "log MSRP (log RMB)",
             ylab = "Density",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  if (save_fig | save_fig2){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }
}
message(paste("Figure ", fignum, " is complete.", sep = ""))

### Miscellaneous ---------------------------
