### Header ---------------------------
###
### Title: reproduce_results.R
###
### Description: R script to reproduce results of Daljord et al. (2020).
###
### Author: Omkar A. Katta
###
### Notes: User must change the information in the Preliminaries Section
###       as needed. In particular, the `version` variable, the file paths,
###       and the plotting parameters can be changed to fit the user's preferences.
###
###

### Preliminaries ---------------------------

# Install Packages (as needed)
if (!require(diftrans, quietly = T)){
  if (!require(devtools, quietly = T)){
    install.packages("devtools")
  }
  devtools::install_github("omkarakatta/diftrans")
}
if (!require(magrittr, quietly = T)){
  install.packages("magrittr")
}
if (!require(ggplot2, quietly = T)){
  install.packages("ggplot2")
}
if (!require(gridExtra, quietly = T)){
  install.packages("gridExtra")
}
if (!require(dplyr, quietly = T)){
  install.packages("dplyr")
}
if (!require(tidyr, quietly = T)){
  install.packages("tidyr")
}
if (!require(stargazer, quietly = T)){
  install.packages("stargazer")
}

library(diftrans)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(stargazer)

# What data do you want to use?
# Option 1: "original" i.e. use entire data set
# Option 2: "v1" i.e. exclude December 2010
# Option 3: "v2" i.e. exclude December 2010, January 2011, and February 2012

version <- "original" # options are: "original" "v1" "v2". See comments above.

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

# Do you want to run diff-in-diff analysis
show_diff_in_diff <- FALSE

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
    filter(!(year == 2010 & month == 12))
  Tianjin <- Tianjin_cleaned %>%
    filter(!(year == 2010 & month == 12))
  Shijiazhuang <- Shijiazhuang_cleaned %>%
    filter(!(year == 2010 & month == 12))
  seedplus <- 1
  message(paste("Version: ", version, sep = ""))
} else if (version == "v2"){
  # filter out Dec 2010, Jan 2011, and Feb 2011
  Beijing <- Beijing_cleaned %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Tianjin <- Tianjin_cleaned %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Shijiazhuang <- Shijiazhuang_cleaned %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
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

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

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

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 3 ---------------------------
fignum <- 3
if (show_fig | show_fig3){
  pre <- prep_data(Beijing, prep = "dist",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Beijing, prep = "dist",
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")
  fig3_plot <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                scale = 1000,
                x = MSRP, binwidth = 20) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, 1400, by = 100),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.002),
             xlab = "MSRP (RMB 1000)",
             ylab = "Density",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  if (save_fig | save_fig3){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 4 ---------------------------
fignum <- 4
if (show_fig | show_fig4){
  pre <- prep_data(Beijing, prep = "pmf",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Beijing, prep = "pmf",
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")

  scalex <- 1000

  color_fig4a <- ifelse(grayscale,
                        get_color_palette(2, grayscale)[[1]],
                        get_color_palette(2, grayscale)[[1]])
  fig4a_OK <- ggplot() +
    geom_segment(data = pre,
                 mapping = aes(x = MSRP / scalex, xend = MSRP / scalex,
                               y = 0, yend = count),
                 alpha = 0.5,
                 color = color_fig4a) +
    bmp_plot(data = Beijing,
             xtype = "continuous",
             xbreaks = seq(0, max(Beijing$MSRP, na.rm = T)/scalex, by = 200000/scalex),
             ytype = "continuous",
             ybreaks = seq(0, max(pre$count, post$count), by = 5000),
             ylims = c(0, max(pre$count, post$count)),
             ylab = "Probability Mass Function",
             xlab = "MSRP (RMB 1000)",
             ggtitle = "Pre-Lottery",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  color_fig4b <- ifelse(grayscale,
                        get_color_palette(3, grayscale)[[2]],
                        get_color_palette(2, grayscale)[[2]])
  fig4b_OK <- ggplot() +
    geom_segment(data = post,
                 mapping = aes(x = MSRP / scalex, xend = MSRP / scalex,
                               y = 0, yend = count),
                 alpha = 0.5,
                 color = color_fig4b) +
    bmp_plot(data = Beijing,
             xtype = "continuous",
             xbreaks = seq(0, max(Beijing$MSRP, na.rm = T)/scalex, by = 200000/scalex),
             ytype = "continuous",
             ybreaks = seq(0, max(pre$count, post$count), by = 5000),
             ylims = c(0, max(pre$count, post$count)),
             # ylab = "Probability Mass Function",
             xlab = "MSRP (RMB 1000)",
             ggtitle = "Post-Lottery",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  fig4_plot_show <- grid.arrange(fig4a_OK, fig4b_OK, ncol = 2)
  fig4_plot <- arrangeGrob(fig4a_OK, fig4b_OK, ncol = 2)
  if (save_fig | save_fig4){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), fig4_plot, path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 5 ---------------------------
fignum <- 5
if (show_fig | show_fig5){
  pre <- prep_data(Beijing, prep = "dist",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Beijing, prep = "dist",
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")

  upperxlim <- 1400000
  scale <- 1000

  da <- 30000 / scale
  fig5a_OK <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                x = MSRP, scale = scale, binwidth = da) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, upperxlim / scale, by = 100000 / scale),
             xlims = c(0, upperxlim/scale),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.002),
             # xlab = "MSRP (RMB 1000)",
             ylab = "Density",
             xangle = 45,
             ggtitle = paste("d = ", da*scale, sep = ""),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  # fig5a_OK

  db <- 50000 / scale
  fig5b_OK <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                x = MSRP, scale = scale, binwidth = db) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, upperxlim / scale, by = 100000 / scale),
             xlims = c(0, upperxlim/scale),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.002),
             # xlab = "MSRP (RMB 1000)",
             # ylab = "Density",
             xangle = 45,
             ggtitle = paste("d = ", db*scale, sep = ""),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  # fig5b_OK

  dc <- 70000 / scale
  fig5c_OK <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                x = MSRP, scale = scale, binwidth = dc) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, upperxlim / scale, by = 100000 / scale),
             xlims = c(0, upperxlim/scale),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.001),
             xlab = "MSRP (RMB 1000)",
             ylab = "Density",
             xangle = 45,
             ggtitle = paste("d = ", dc*scale, sep = ""),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  # fig5c_OK

  dd <- 90000 / scale
  fig5d_OK <- ggplot() +
    bmp_twohist(data1 = pre, data2 = post,
                x = MSRP, scale = scale, binwidth = dd) +
    bmp_plot(data = Beijing,
             color = policy_dummy,
             fill = policy_dummy,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, upperxlim / scale, by = 100000 / scale),
             xlims = c(0, upperxlim/scale),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.001),
             xlab = "MSRP (RMB 1000)",
             # ylab = "Density",
             xangle = 45,
             ggtitle = paste("d = ", dd*scale, sep = ""),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))
  # fig5d_OK
  fig5_plot_show <- grid.arrange(fig5a_OK, fig5b_OK,
                                 fig5c_OK, fig5d_OK,
                                 nrow = 2)
  fig5_plot <- arrangeGrob(fig5a_OK, fig5b_OK,
                           fig5c_OK, fig5d_OK,
                           nrow = 2)

  if (save_fig | save_fig5){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), fig5_plot, path = img_path,
           width = default_width, height = default_height+5, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 6 ---------------------------
fignum <- 6
if (show_fig | show_fig6){
  set.seed(11 + seedplus)
  support <- prep_data(data = Beijing, prep = "support")

  pre <- prep_data(Beijing, prep = "pmf",
                   support = support,
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Beijing, prep = "pmf",
                    support = support,
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")

  synth_pre1 <- data.frame(MSRP = pre$MSRP,
                           count = rmultinom(1, sum(pre$count), pre$count))
  synth_pre2 <- data.frame(MSRP = pre$MSRP,
                           count = rmultinom(1, sum(pre$count), pre$count))

  bandwidth_seq = seq(0, 100000, 1000)

  real <- get_results(pre, post, bandwidth_seq = bandwidth_seq, conservative = F)
  placebo <- get_results(synth_pre1, synth_pre2, bandwidth_seq = bandwidth_seq, conservative = F)

  bandwidth_selection <- left_join(real, placebo, by = "bandwidth", suffix = c("_real", "_placebo")) %>%
    mutate(ratio = main_real / main_placebo) %>%
    mutate(order2 = ratio > 100)

  d1000 <- bandwidth_selection %>%
    filter(bandwidth == 10000) %>%
    .$main_real

  message(paste("Figure 6 analysis: the tranport cost at d = 10000 is ", d1000, sep = ""))

  d_table <- bandwidth_selection %>%
    filter(bandwidth %in% seq(10000, 90000, by = 10000))

  fig6_plot <- ggplot(data = bandwidth_selection, aes(x = bandwidth)) +
    geom_smooth(aes(y = 100*main_real, color = "0", linetype = "0"),
                method = loess,
                se = F,
                size = 0.5) +
    geom_line(aes(y = 100*main_real, color = "1", linetype = "1")) +
    geom_line(aes(y = 100*main_placebo, color = "2", linetype = "2")) +
    scale_color_manual(values = get_color_palette(3, grayscale),
                       labels = c("smoothed", "real", "placebo"),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("smoothed", "real", "placebo"),
                          name = "") +
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8),
              axissizefont = (fontsizeaxis - 5)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_x_continuous(breaks = seq(0, 100000, 10000))

  if (save_fig | save_fig6){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 7 ---------------------------
fignum <- 7
if (show_fig | show_fig7){
  pre <- prep_data(Tianjin, prep = "dist",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Tianjin, prep = "dist",
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")
  fig7_plot <- ggplot() +
    bmp_twohist(data1 = pre,
                data2 = post,
                x = MSRP,
                scale = 1000,
                binwidth = 20) +
    bmp_plot(data = Tianjin,
             color = postBeijing,
             fill = postBeijing,
             legendlabels = c("pre-lottery", "post-lottery"),
             xtype = "continuous",
             xbreaks = seq(0, 1400, by = 100),
             ytype = "continuous",
             ybreaks = seq(0, 0.008, by = 0.002),
             xlab = "MSRP (RMB 1000)",
             ylab = "Density",
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5))

  if (save_fig | save_fig7){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 8 ---------------------------
fignum <- 8
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2012_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2012, "n"] %>% as.numeric
  n_2011_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2011, "n"] %>% as.numeric
  n_2012_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2012, "n"] %>% as.numeric
  n_2011_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2011, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2012 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2012-01-01", upperdate = "2013-01-01")
  T2012 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2012-01-01", upperdate = "2013-01-01")


  bandwidth_seq = seq(0, 15000, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  for (i in seq_len(numsim)){
    B2012_n2012 <- data.frame(MSRP = B2012$MSRP,
                              count = rmultinom(1, n_2012_Beijing, B2012$count))
    B2012_n2011 <- data.frame(MSRP = B2012$MSRP,
                              count = rmultinom(1, n_2011_Beijing, B2012$count))
    T2012_n2012 <- data.frame(MSRP = T2012$MSRP,
                              count = rmultinom(1, n_2012_Tianjin, T2012$count))
    T2012_n2011 <- data.frame(MSRP = T2012$MSRP,
                              count = rmultinom(1, n_2011_Tianjin, T2012$count))
    cat("\n")
    print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

    placeboB <- get_results(B2012_n2011, B2012_n2012, bandwidth = bandwidth_seq,
                            conservative = T,
                            quietly = T)
    placeboT <- get_results(T2012_n2011, T2012_n2012, bandwidth = bandwidth_seq,
                            quietly = T)

    placeboB_prop[i, ] <- placeboB$maincons_prop
    placeboT_prop[i, ] <- placeboT$main
  }

  diffprop <- placeboB_prop - placeboT_prop

  bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                    diffprop = apply(diffprop, 2, mean)) %>%
    mutate(diffprop_lag = lag(diffprop, 1)) %>%
    mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
    mutate(closetozero = diffprop_diff < 1e-3) %>%
    mutate(closetozero_lag1 = lag(closetozero)) %>%
    mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
    mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)


  # d_a = first time that the difference in the mean cost at a particular bandwidth and the previous bandwidth is less than 1e-3 three times for consecutive bandwidth choices
  d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                             "bandwidth"]
  meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                      "diffprop"]

  message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  fig8_plot <- ggplot() +
    bmp_vline(xint = d_a / 2) +
    bmp_vline(xint = d_a) +
    geom_smooth(data = bandwidth_selection,
                mapping = aes(x = bandwidth, y = diffprop),
                method = loess,
                se = F,
                color = "black",
                size = 0.5) +
    bmp_plot(data = bandwidth_selection,
             xlab = "d",
             ylab = "Mean Cost",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)

  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 9 ---------------------------
fignum <- 9
if (show_fig | show_fig9){
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")


  post_Bpmf <- prep_data(Beijing, prep = "pmf",
                         support = supportB,
                         lowerdate = "2015-01-01", upperdate = "2016-01-01")
  pre_Bpmf <- prep_data(Beijing, prep = "pmf",
                        support = supportB,
                        lowerdate = "2014-01-01", upperdate = "2015-01-01")
  post_Tpmf <- prep_data(Tianjin, prep = "pmf",
                         support = supportT,
                         lowerdate = "2015-01-01", upperdate = "2016-01-01")
  pre_Tpmf <- prep_data(Tianjin, prep = "pmf",
                        support = supportT,
                        lowerdate = "2014-01-01", upperdate = "2015-01-01")

  bandwidth_seq = seq(0, 40000, 1000)

  temp <- get_results(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                      bandwidth_seq = bandwidth_seq,
                      conservative = F,
                      quietly = T)
  temp2 <- temp %>%
    pivot_longer(cols = c(main, control, diff),
                 names_to = "type",
                 values_to = "diffprop") %>%
    mutate(type = case_when(type == "main" ~ "Beijing",
                            type == "control" ~ "Tianjin",
                            type == "diff" ~ "a"))

  bandwidth_selection <- left_join(placeboB, placeboT, by = "bandwidth", suffix = c("_B", "_T")) %>%
    mutate(diffprop = main_B - main_T) %>%
    select(-maincons_prop) %>%
    pivot_longer(cols = c(main_B,
                                 main_T,
                                 diffprop),
                        names_to = "type",
                        values_to = "diffprop")

  fig9_plot <- ggplot(data = temp2,
                       aes(x = bandwidth)) +
    geom_line(aes(y = diffprop*100, color = type, linetype = type)) +
    bmp_plot(data = bandwidth_selection,
             color = type,
             legendlabels = c("Difference", "Beijing placebo", "Tianjin placebo"),
             xlab = "d",
             ylab = "Transport Cost (%)",
             ytype = "continuous",
             ybreaks = seq(-50, 100, 10),
             xtype = "continuous",
             xbreaks = seq(0, 40000, 5000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5)) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("Difference", "Beijing placebo", "Tianjin placebo"),
                          name = "")

  if (save_fig | save_fig9){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 10 ---------------------------
fignum <- 10
if (show_fig | show_fig10){
  # BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  supportB <- prep_data(Beijing,
                        prep = "support")
  supportT <- prep_data(Tianjin,
                        prep = "support")
  post_Bpmf <- prep_data(Beijing, prep = "pmf",
                         support = supportB,
                         lowerdate = "2011-01-01", upperdate = "2012-01-01")
  pre_Bpmf <- prep_data(Beijing, prep = "pmf",
                        support = supportB,
                        lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post_Tpmf <- prep_data(Tianjin, prep = "pmf",
                         support = supportT,
                         lowerdate = "2011-01-01", upperdate = "2012-01-01")
  pre_Tpmf <- prep_data(Tianjin, prep = "pmf",
                        support = supportT,
                        lowerdate = "2010-01-01", upperdate = "2011-01-01")

  bandwidth_seq = seq(0, 40000, 1000)

  cons_dit <- get_results(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                          bandwidth_seq = bandwidth_seq,
                          conservative = T)
  dit <- get_results(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                     bandwidth_seq = bandwidth_seq,
                     conservative = F)

  bandwidth_selection <- cons_dit %>%
    select(-main2d)  %>%
    pivot_longer(cols = c(main,
                          control,
                          diff,
                          diff2d),
                 names_to = "type",
                 values_to = "diffprop") %>%
    mutate(type = case_when(type == "main" ~ "Beijing",
                            type == "control" ~ "Tianjin",
                            type == "diff" ~ "a",
                            type == "diff2d" ~ "b"))

  diffprop <- cons_dit$diff
  diffprop2 <- cons_dit$diff2d

  whichmax <- which.max(diffprop)
  mostinform <- bandwidth_seq[whichmax]

  whichmax2 <- which.max(diffprop2)
  mostinform2 <- bandwidth_seq[whichmax2]

  fig10_plot <- ggplot(data = bandwidth_selection %>%
                         filter(type != "b"), # control d-d or 2d-d
                       aes(x = bandwidth,
                           linetype = type)) +
    bmp_vline(xint = mostinform) +
    bmp_vline(xint = mostinform2) +
    geom_line(aes(y = diffprop*100, color = type)) +
    bmp_plot(data = bandwidth_selection %>%
               filter(type != "b"),
             color = type,
             legendlabels = c("Difference", "Beijing", "Tianjin"),
             xlab = "d",
             ylab = "Transport Cost (%)",
             ytype = "continuous",
             ybreaks = seq(-50, 100, 10),
             xtype = "continuous",
             xbreaks = seq(0, 40000, 5000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5)) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("Difference", "Beijing", "Tianjin"),
                          name = "")
  if (save_fig | save_fig10){
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}


### Miscellaneous ---------------------------

# Original Figure 3 (Comparing prices of car models between 2010 and 2011)
fig3_prep <- Beijing %>%
  filter(ym < as.Date("2012-01-01")) %>%
  pivot_wider(id_cols = c(month, color, noticenum),
                     names_from = year,
                     values_from = MSRP) %>%
  filter(!(is.na(`2010`) | is.na(`2011`))) %>%
  mutate(diff = `2011` - `2010`)
nrow(fig3_prep)
summary(fig3_prep$diff)

# Diff-in-diff
if (show_diff_in_diff){
  did_BT <- do.call("rbind", list(Beijing, Tianjin)) %>%
    filter(ym < "2012-01-01") %>%
    filter(city == "Beijing" | city == "Tianjin") %>%
    mutate(Beijing = ifelse(city == "Beijing", 1, 0)) %>%
    group_by(Beijing, postBeijing, MSRP) %>%
    summarise(sum = sum(sales)) %>%
    uncount(sum)
  did_BT_reg <- lm(log(MSRP) ~ postBeijing + Beijing + postBeijing*Beijing, data = did_BT)

  did_BS <- do.call("rbind", list(Beijing, Shijiazhuang)) %>%
    filter(ym < "2012-01-01") %>%
    filter(city == "Beijing" | city == "Shijiazhuang") %>%
    mutate(Beijing = ifelse(city == "Beijing", 1, 0)) %>%
    group_by(Beijing, postBeijing, MSRP) %>%
    summarise(sum = sum(sales)) %>%
    uncount(sum)
  did_BS_reg <- lm(log(MSRP) ~ postBeijing + Beijing + postBeijing*Beijing, data = did_BS)

  did_BST <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang)) %>%
    filter(ym < "2012-01-01") %>%
    filter(city == "Beijing" | city == "Shijiazhuang" | city == "Tianjin") %>%
    mutate(Beijing = ifelse(city == "Beijing", 1, 0)) %>%
    group_by(Beijing, postBeijing, MSRP) %>%
    summarise(sum = sum(sales)) %>%
    uncount(sum)
  did_BST_reg <- lm(log(MSRP) ~ postBeijing + Beijing + postBeijing*Beijing, data = did_BST)

  stargazer(did_BT_reg, did_BS_reg, did_BST_reg,
            label = "tab:eight",
            type = "latex",
            # covariate.labels = c("post", "Beijing", "Beijing $\\times$ post", "Constant"),
            order = c(2, 1, 3, 4),
            covariate.labels = c("Beijing", "post", "Beijing $\\times$ post", "Constant"),
            keep.stat = c("rsq","n"),
            align = T,
            p.auto = FALSE,
            se = list(summary(did_BT_reg)$coefficients[,"t value"],
                      summary(did_BS_reg)$coefficients[,"t value"],
                      summary(did_BST_reg)$coefficients[,"t value"]),
            column.labels = c("Tianjin", "Shijiazhuang", "Both"),
            model.numbers = FALSE,
            dep.var.caption = "",
            dep.var.labels.include = FALSE,
            # single.row = TRUE,
            notes = c("t-statistics are in parentheses."))

  # add caption
  # add \hline after header row
  # change label as necessary
  # change float as necessary

}

