### Header ---------------------------
###
### Title: reproduce_results.R
###
### Description: R script to reproduce results of Daljord, Pouliot, Hu, and Xiao (2020).
###
### Author: Omkar A. Katta
###
### Notes: User must change the information in the Preliminaries Section
###       as needed. In particular, the `version` variable, the file paths,
###       and the plotting parameters can be changed to fit the user's preferences.
###
###       User needs to change file paths to source functions used to prepare and
###       plot the data.
###
###       Due to privacy restrictions, the original data set cannot be distributed.
###       Hence, an anonymized version of the data set will be provided.
###       The results of this script will therefore differ slightly from the paper's results.
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

# source functions: CHANGE FILE PATH
source(here::here("data-raw", "prepare_data.R"))
source(here::here("data-raw", "plotting_functions.R"))


# What data do you want to use?
# Option 1: "original" i.e. use entire data set
# Option 2: "v1" i.e. exclude December 2010
# Option 3: "v2" i.e. exclude December 2010, January 2011, and February 2012
version <- "original"

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
linetype3 <- "twodash"
linetype4 <- "longdash"

# Where will you store plots?
# img_path <- ... # uncomment and replace ... with destination file path
# suffix <- "_"  # naming convention

# How large do you want your plots to be? (inches)
# default_width <- 7
# default_height <- 3

### Prepare Data ---------------------------

if (version == "original") {
  Beijing <- Beijing_sample
  Tianjin <- Tianjin_sample
  Shijiazhuang <- Shijiazhuang_sample
  seedplus <- 0
  message(paste("Version: ", version, sep = ""))
} else if (version == "v1") {
  # filter out Dec 2010
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12))
  Tianjin <- Tianjin_sample %>%
    filter(!(year == 2010 & month == 12))
  Shijiazhuang <- Shijiazhuang_sample %>%
    filter(!(year == 2010 & month == 12))
  seedplus <- 1
  message(paste("Version: ", version, sep = ""))
} else if (version == "v2") {
  # filter out Dec 2010, Jan 2011, and Feb 2011
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Tianjin <- Tianjin_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  Shijiazhuang <- Shijiazhuang_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  seedplus <- 2
  message(paste("Version: ", version, sep = ""))
} else {
  stop("Specify a valid value for `version`.")
}

### Figure 1 ---------------------------
fignum <- 1
if (show_fig | show_fig1) {
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
              color = factor(city,
                             levels = c("Beijing", "Tianjin", "Shijiazhuang")),
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
                                    color = factor(city,
                                                   levels = c("Beijing", "Tianjin", "Shijiazhuang")),
                                    linetype = factor(city,
                                                      levels = c("Beijing", "Tianjin", "Shijiazhuang"))),
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
if (show_fig | show_fig2) {
  pre <- prep_data(Beijing, prep = "pmf",
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  # find prices of new cars in 2011
  post <- Beijing %>%
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
  if (save_fig | save_fig2) {
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 3 ---------------------------
fignum <- 3
if (show_fig | show_fig3) {
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
  if (save_fig | save_fig3) {
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 4 ---------------------------
fignum <- 4
if (show_fig | show_fig4) {
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
  # fig4_plot_show <- grid.arrange(fig4a_OK, fig4b_OK, ncol = 2)
  fig4_plot <- arrangeGrob(fig4a_OK, fig4b_OK, ncol = 2)
  if (save_fig | save_fig4) {
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), fig4_plot, path = img_path,
           width = default_width, height = default_height+1, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 5 ---------------------------
fignum <- 5
if (show_fig | show_fig5) {
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
  # fig5_plot_show <- grid.arrange(fig5a_OK, fig5b_OK,
  #                                fig5c_OK, fig5d_OK,
  #                                nrow = 2)
  fig5_plot <- arrangeGrob(fig5a_OK, fig5b_OK,
                           fig5c_OK, fig5d_OK,
                           nrow = 2)

  if (save_fig | save_fig5) {
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), fig5_plot, path = img_path,
           width = default_width, height = default_height+5, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))
}

### Figure 6 ---------------------------
fignum <- "6_B1011"
if (show_fig | show_fig6) {
  set.seed(11 + seedplus)
  support <- prep_data(data = Beijing, prep = "support")

  pre <- prep_data(Beijing, prep = "pmf",
                   support = support,
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
  post <- prep_data(Beijing, prep = "pmf",
                    support = support,
                    lowerdate = "2011-01-01", upperdate = "2012-01-01")
  bandwidth_seq <- seq(0, 100000, 1000)

  real <- diftrans(pre, post, bandwidth_seq = bandwidth_seq, conservative = F)

  numsims <- 500
  placebo_results <- matrix(NA, nrow = numsims, ncol = length(bandwidth_seq))
  for (i in seq_len(numsims)) {
    print(paste("Simulation Number", i, "out of", numsims, sep = " "))
    # draw n_B,2010 samples from Beijing, 2010
    synth_pre1 <- data.frame(MSRP = pre$MSRP,
                             count = rmultinom(1, sum(pre$count), pre$count))
    # draw n_B,2011 samples from Beijing, 2010
    synth_pre2 <- data.frame(MSRP = pre$MSRP,
                             count = rmultinom(1, sum(post$count), pre$count))
    placebo <- diftrans(synth_pre1, synth_pre2,
                        bandwidth_seq = bandwidth_seq, conservative = F)
    placebo_results[i, ] <- placebo$main
  }

  placebo_mean <- apply(placebo_results, 2, mean) * 100
  placebo_sd <- apply(placebo_results, 2, sd) * 100
  probs <- c(0.90, 0.95, 0.99)
  placebo_quantiles <- apply(placebo_results, 2, quantile, prob = probs) * 100

  plot_mat <- do.call(rbind, list(placebo_mean, placebo_sd, placebo_quantiles))
  rownames(plot_mat) <- c("mean", "sd", paste("quantile", probs, sep = ""))
  plot_table <- t(plot_mat) %>%
    as.data.frame() %>%
    mutate(bandwidth = bandwidth_seq,
           real = real$main * 100) %>%
    select(bandwidth, real, everything())

  # close to mean placebo = 0.05%
  lower <- 0.04
  upper <- 0.06
  valid <- plot_table$bandwidth[plot_table$mean < upper & plot_table$mean > lower]

  # table 4
  d_table <- plot_table %>%
    filter(bandwidth %in% c(valid,
                            4000, 5000, 6000, 7000, 8000, 9000, 10000,
                            15000, 20000, 25000, 30000, 35000, 40000, 45000,
                            50000, 70000, 90000))
  knitr::kable(d_table, format = "latex", booktabs = T, linesep = "")

  # plot real cost and mean placebo cost - fig6
  fig6_plot <- ggplot(data = plot_table, aes(x = bandwidth)) +
    geom_line(aes(y = real, color = "0", linetype = "0")) +
    #~ geom_line(aes(y = quantile0.99, color = "1", linetype = "1")) +
    #~ geom_line(aes(y = quantile0.95, color = "2", linetype = "2")) +
    #~ geom_line(aes(y = quantile0.9, color = "3", linetype = "3")) +
    geom_line(aes(y = mean, color = "5", linetype = "5")) +
    scale_color_manual(values = get_color_palette(2, grayscale),
                       labels = c("real", "placebo"),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("real", "placebo"),
                          name = "") +
    bmp_vline(xint = 25000) +
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8),
              axissizefont = (fontsizeaxis - 5)) +
    scale_x_continuous(breaks = seq(0, 100000, 10000))

  # simulation results only, no real cost (additional figure)
  fig6_plot2 <- ggplot(data = plot_table, aes(x = bandwidth)) +
    geom_line(aes(y = quantile0.99, color = "1", linetype = "1")) +
    geom_line(aes(y = quantile0.95, color = "2", linetype = "2")) +
    geom_line(aes(y = quantile0.9, color = "3", linetype = "3")) +
    geom_line(aes(y = mean, color = "5", linetype = "5")) +
    geom_line(aes(y = sd, color = "6", linetype = "6")) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "sd"),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "sd"),
                          name = "") +
    bmp_vline(xint = 25000) +
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8),
              axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.1)) +
    scale_x_continuous(breaks = seq(0, 100000, 10000))

  if (save_fig | save_fig6) {
    ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""),
           path = img_path, fig6_plot,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig",
                  fignum, suffix, "OK.jpg", sep = ""))

    ggsave(paste("fig", fignum, suffix,"zoomed", suffix, "OK.jpg", sep = ""),
           path = img_path, fig6_plot2,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, "_zoomed", " is saved in ", img_path, " as fig",
                  fignum, suffix, "OK.jpg", sep = ""))
  }

  message(paste("Figure ", fignum, " is complete.", sep = ""))


}

### \hat{P}_{B,2010} vs \tilde{P}_{B,2010} (Oct 16) ----------------
fignum <- "B10"
if (show_fig | show_fig6) {
  set.seed(19 + seedplus)
  support <- prep_data(data = Beijing, prep = "support")

  pre <- prep_data(Beijing, prep = "pmf",
                   support = support,
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")

  bandwidth_seq = seq(0, 100000, 1000)
  bandwidth_table = c(4000, 5000, 6000, 7000, 8000, 9000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 70000, 90000)
  numsims <- 200
  results <- matrix(NA_real_, nrow = numsims, ncol = length(bandwidth_seq))
  colnames(results) <- bandwidth_seq

  # real <- diftrans(pre, post, bandwidth_seq = bandwidth_seq, conservative = F)
  
  for (i in seq_len(numsims)) {
    print(paste("Simulation Number ", i, " out of ", numsims, sep = ""))
    synth <- data.frame(MSRP = pre$MSRP, 
                        count = rmultinom(1, sum(pre$count), pre$count))
    placebo <- diftrans(pre, synth, bandwidth_seq = bandwidth_seq, conservative = F)
    results[i, ] <- placebo$main
  }

  results <- results*100
  # real_estimate <- real$main*100
  # names(real_estimate) <- real$bandwidth

  mean_placebo <- apply(results, 2, mean)
  sd_placebo_centered <- sapply(seq_along(bandwidth_seq), function(x) sqrt(sum((results[,x] - mean(results[,x]))^2) / (numsims)))
  # sd_placebo_real_centered <- sapply(seq_along(bandwidth_seq), function(x) sqrt(sum((results[,x] - real_estimate[x])^2) / (numsims)))
  # quantile_placebo <- apply(results, 2, quantile, probs = c(0.01, 0.99, 0.05, 0.95, 0.1, 0.9))
  quantile_placebo <- apply(results, 2, quantile, probs = c(0.99, 0.95, 0.9))

  # real_estimate_round <- round(real_estimate, 3)
  mean_placebo_round <- round(mean_placebo, 5)
  sd_placebo_centered_round <- round(sd_placebo_centered, 5)
  # sd_placebo_real_centered_round <- round(sd_placebo_real_centered, 5)
  quantile_placebo_round <- round(quantile_placebo, 5)

  plot_table <- data.frame(t(rbind(mean_placebo, sd_placebo_centered, quantile_placebo)))

  d_table <- plot_table[as.character(bandwidth_table), ]
  knitr::kable(d_table, format = "latex", booktabs = T, linesep = "")

  # q1 <- quantile_placebo["99%",] - quantile_placebo["1%",]
  # q2 <- quantile_placebo["95%",] - quantile_placebo["5%",]
  # q3 <- quantile_placebo["90%",] - quantile_placebo["10%",]
  # q <- t(rbind(q1, q2, q3))[as.character(bandwidth_table), ]


  fig_B10_1 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_line(aes(y = X99., color = "0", linetype = "0")) + 
    geom_line(aes(y = X95., color = "1", linetype = "1")) + 
    geom_line(aes(y = X90., color = "2", linetype = "2")) + 
    geom_line(aes(y = mean_placebo_round, color = "3", linetype = "3")) +
    geom_line(aes(y = sd_placebo_centered_round, color = "4", linetype = "4")) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    # ylim(0, 0.15) +
    scale_y_continuous(breaks = seq(0, 1, .10)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100000, 10000))

  fig_B10_2 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_line(aes(y = X99., color = "0", linetype = "0")) + 
    geom_line(aes(y = X95., color = "1", linetype = "1")) + 
    geom_line(aes(y = X90., color = "2", linetype = "2")) + 
    geom_line(aes(y = mean_placebo_round, color = "3", linetype = "3")) +
    geom_line(aes(y = sd_placebo_centered_round, color = "4", linetype = "4")) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    ylim(0, 0.15) +
#     scale_y_continuous(breaks = seq(0, 0.2, .05)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000)) +
    ggtitle("OT(P_hat_Beijing_2010, P_tilde_Beijing_2010) - zoomed in")

  fig_B10_3 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_smooth(aes(y = X99., color = "0", linetype = "0"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = X95., color = "1", linetype = "1"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = X90., color = "2", linetype = "2"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = mean_placebo_round, color = "3", linetype = "3"), method = loess, se = F, size = 0.5) +
    geom_smooth(aes(y = sd_placebo_centered_round, color = "4", linetype = "4"), method = loess, se = F, size = 0.5) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    ylim(0, 0.15) +
#     scale_y_continuous(breaks = seq(0, 0.2, .05)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000)) +
    ggtitle("OT(P_hat_Beijing_2010, P_tilde_Beijing_2010) - zoomed in with loess filter")

  if (save_fig | save_fig6) {
    ggsave(paste("fig", fignum, suffix, "1", suffix, "OK.jpg", sep = ""), plot = fig_B10_1, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    ggsave(paste("fig", fignum, suffix, "2", suffix, "OK.jpg", sep = ""), plot = fig_B10_2, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    ggsave(paste("fig", fignum, suffix, "3", suffix, "OK.jpg", sep = ""), plot = fig_B10_3, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

}


### \hat{P}_{B,2011} vs \tilde{P}_{B,2011} (Oct 16) ----------------
fignum <- "B11"
if (show_fig | show_fig6) {
  set.seed(19 + seedplus)
  support <- prep_data(data = Beijing, prep = "support")

  pre <- prep_data(Beijing, prep = "pmf",
                   support = support,
                   lowerdate = "2011-01-01", upperdate = "2012-01-01")

  bandwidth_seq = seq(0, 100000, 1000)
  bandwidth_table = c(4000, 5000, 6000, 7000, 8000, 9000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 70000, 90000)
  numsims <- 200
  results <- matrix(NA_real_, nrow = numsims, ncol = length(bandwidth_seq))
  colnames(results) <- bandwidth_seq

  # real <- diftrans(pre, post, bandwidth_seq = bandwidth_seq, conservative = F)
  
  for (i in seq_len(numsims)) {
    print(paste("Simulation Number ", i, " out of ", numsims, sep = ""))
    synth <- data.frame(MSRP = pre$MSRP, 
                        count = rmultinom(1, sum(pre$count), pre$count))
    placebo <- diftrans(pre, synth, bandwidth_seq = bandwidth_seq, conservative = F)
    results[i, ] <- placebo$main
  }

  results <- results*100
  # real_estimate <- real$main*100
  names(real_estimate) <- real$bandwidth

  mean_placebo <- apply(results, 2, mean)
  sd_placebo_centered <- sapply(seq_along(bandwidth_seq), function(x) sqrt(sum((results[,x] - mean(results[,x]))^2) / (numsims)))
  # sd_placebo_real_centered <- sapply(seq_along(bandwidth_seq), function(x) sqrt(sum((results[,x] - real_estimate[x])^2) / (numsims)))
  # quantile_placebo <- apply(results, 2, quantile, probs = c(0.01, 0.99, 0.05, 0.95, 0.1, 0.9))
  quantile_placebo <- apply(results, 2, quantile, probs = c(0.99, 0.95, 0.9))

  # real_estimate_round <- round(real_estimate, 3)
  mean_placebo_round <- round(mean_placebo, 5)
  sd_placebo_centered_round <- round(sd_placebo_centered, 5)
  # sd_placebo_real_centered_round <- round(sd_placebo_real_centered, 5)
  quantile_placebo_round <- round(quantile_placebo, 5)

  plot_table <- data.frame(t(rbind(mean_placebo, sd_placebo_centered, quantile_placebo)))

  d_table <- plot_table[as.character(bandwidth_table), ]
  knitr::kable(d_table, format = "latex", booktabs = T, linesep = "")

  # q1 <- quantile_placebo["99%",] - quantile_placebo["1%",]
  # q2 <- quantile_placebo["95%",] - quantile_placebo["5%",]
  # q3 <- quantile_placebo["90%",] - quantile_placebo["10%",]
  # q <- t(rbind(q1, q2, q3))[as.character(bandwidth_table), ]


  fig_B11_1 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_line(aes(y = X99., color = "0", linetype = "0")) + 
    geom_line(aes(y = X95., color = "1", linetype = "1")) + 
    geom_line(aes(y = X90., color = "2", linetype = "2")) + 
    geom_line(aes(y = mean_placebo_round, color = "3", linetype = "3")) +
    geom_line(aes(y = sd_placebo_centered_round, color = "4", linetype = "4")) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    # ylim(0, 0.15) +
    scale_y_continuous(breaks = seq(0, 1, .10)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100000, 10000))

  fig_B11_2 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_line(aes(y = X99., color = "0", linetype = "0")) + 
    geom_line(aes(y = X95., color = "1", linetype = "1")) + 
    geom_line(aes(y = X90., color = "2", linetype = "2")) + 
    geom_line(aes(y = mean_placebo_round, color = "3", linetype = "3")) +
    geom_line(aes(y = sd_placebo_centered_round, color = "4", linetype = "4")) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    ylim(0, 0.15) +
#     scale_y_continuous(breaks = seq(0, 0.2, .05)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000)) +
    ggtitle("OT(P_hat_Beijing_2011, P_tilde_Beijing_2011) - zoomed in")

  fig_B11_3 <- ggplot(data = plot_table, aes(x = as.numeric(rownames(plot_table)))) +
    geom_smooth(aes(y = X99., color = "0", linetype = "0"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = X95., color = "1", linetype = "1"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = X90., color = "2", linetype = "2"), method = loess, se = F, size = 0.5) + 
    geom_smooth(aes(y = mean_placebo_round, color = "3", linetype = "3"), method = loess, se = F, size = 0.5) +
    geom_smooth(aes(y = sd_placebo_centered_round, color = "4", linetype = "4"), method = loess, se = F, size = 0.5) + 
    xlab("d") +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    ylim(0, 0.15) +
#     scale_y_continuous(breaks = seq(0, 0.2, .05)) +
    scale_color_manual(values = get_color_palette(5, grayscale),
                       labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                          labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                          name = "") +
    scale_x_continuous(breaks = seq(0, 100000, 10000)) +
    ggtitle("OT(P_hat_Beijing_2011, P_tilde_Beijing_2011) - zoomed in with loess filter")

  if (save_fig | save_fig6) {
    ggsave(paste("fig", fignum, suffix, "1", suffix, "OK.jpg", sep = ""), plot = fig_B11_1, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    ggsave(paste("fig", fignum, suffix, "2", suffix, "OK.jpg", sep = ""), plot = fig_B11_2, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    ggsave(paste("fig", fignum, suffix, "3", suffix, "OK.jpg", sep = ""), plot = fig_B11_3, path = img_path,
           width = default_width, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

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


### Figure ?? - Oct 19 ---------------------------
fignum <- "?"
set.seed(9 + seedplus)
BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                      prep = "support")
supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                      prep = "support")
B_pre <- prep_data(Beijing, prep = "pmf",
                   support = supportB,
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
B_post <- prep_data(Beijing, prep = "pmf",
                   support = supportB,
                   lowerdate = "2011-01-01", upperdate = "2012-01-01")
T_pre <- prep_data(Tianjin, prep = "pmf",
                   support = supportT,
                   lowerdate = "2010-01-01", upperdate = "2011-01-01")
T_post <- prep_data(Tianjin, prep = "pmf",
                   support = supportT,
                   lowerdate = "2011-01-01", upperdate = "2012-01-01")


max_bw <- 20000
bandwidth_seq <- seq(0, max_bw, 1000)
numsim <- 500
LHS_all <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))
RHS_all <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))
diff_all <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))

for (i in seq_len(numsim)){
  print(paste("Simulation Number: ", i, " out of ", numsim, sep = ""))
  B_pre_tilde <- data.frame(MSRP = B_pre$MSRP, 
                            count = rmultinom(1, sum(B_pre$count), B_pre$count))
  B_post_tilde <- data.frame(MSRP = B_post$MSRP, 
                            count = rmultinom(1, sum(B_post$count), B_post$count))
  T_pre_tilde <- data.frame(MSRP = T_pre$MSRP, 
                            count = rmultinom(1, sum(T_pre$count), T_pre$count))
  T_post_tilde <- data.frame(MSRP = T_post$MSRP, 
                            count = rmultinom(1, sum(T_post$count), T_post$count))

  LHS1 <- diftrans(B_pre_tilde, B_post_tilde, bandwidth = bandwidth_seq, conservative = T)
  LHS2 <- diftrans(B_pre, B_post, bandwidth = bandwidth_seq, conservative = T)
  RHS1 <- diftrans(T_pre_tilde, T_post_tilde, bandwidth = bandwidth_seq, conservative = F)
  RHS2 <- diftrans(T_pre, T_post, bandwidth = bandwidth_seq, conservative = F)
  LHS <- 100*LHS1$main2d - 100*LHS2$main2d
  RHS <- 100*RHS1$main - 100*RHS2$main
  diff <- RHS - LHS

  LHS_all[i, ] <- LHS
  RHS_all[i, ] <- RHS
  diff_all[i, ] <- diff
}

LHS_mean <- apply(LHS_all, 2, mean)
RHS_mean <- apply(RHS_all, 2, mean)
diff_mean <- apply(diff_all, 2, mean)


sum(LHS_mean < RHS_mean)
plot_table <- data.frame(d = bandwidth_seq, LHS = LHS_mean, RHS = RHS_mean, diff = diff_mean) %>%
  pivot_longer(cols = c(LHS, RHS, diff))
bandwidth_seq[diff_mean >= 0]

ggplot(plot_table, aes(x = d)) +
  geom_line(aes(y = value, color = name, linetype = name)) +
  bmp_plot(data = plot_table,
         color = name,
         legendlabels = c("Difference", "LHS", "RHS"),
         xlab = "d",
         ylab = "Difference in Transport Cost (%)",
         ytype = "continuous",
         # ybreaks = seq(-50, 100, 10),
         xtype = "continuous",
         xbreaks = seq(0, max_bw, 5000),
         sizefont = (fontsize - 8),
         axissizefont = (fontsizeaxis - 5)) +
  scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                        labels = c("Difference", "LHS", "RHS"),
                        name = "")

if (save_fig | save_fig8){
  ggsave(paste("fig", fignum, suffix, "OK.jpg", sep = ""), path = img_path,
         width = default_width+2, height = default_height, units = "in")
  message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
}

ggplot(plot_table, aes(x = d)) +
  geom_smooth(aes(y = value, color = name, linetype = name),
              method = loess, se = F, size = 0.5) +
  bmp_plot(data = plot_table,
         color = name,
         legendlabels = c("Difference", "LHS", "RHS"),
         xlab = "d",
         ylab = "Difference in Transport Cost (%)",
         ytype = "continuous",
         # ybreaks = seq(-50, 100, 10),
         xtype = "continuous",
         xbreaks = seq(0, max_bw, 5000),
         sizefont = (fontsize - 8),
         axissizefont = (fontsizeaxis - 5)) +
  scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                        labels = c("Difference", "LHS", "RHS"),
                        name = "")

if (save_fig | save_fig8){
  ggsave(paste("fig", fignum, suffix, "loess", suffix, "OK.jpg", sep = ""), path = img_path,
         width = default_width+2, height = default_height, units = "in")
  message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
}



### Figure 8 ---------------------------
fignum <- 8
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2011_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2010, "n"] %>% as.numeric
  n_2011_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2010, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2010 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")
  T2010 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")

  B2011 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")
  T2011 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")

  max_bw <- 30000
  bandwidth_seq = seq(0, max_bw, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  for (i in seq_len(numsim)){
    B2011_n2011 <- data.frame(MSRP = B2011$MSRP,
                              count = rmultinom(1, n_2011_Beijing, B2011$count))
    B2011_n2010 <- data.frame(MSRP = B2011$MSRP,
                              count = rmultinom(1, n_2010_Beijing, B2010$count))
    T2011_n2011 <- data.frame(MSRP = T2011$MSRP,
                              count = rmultinom(1, n_2011_Tianjin, T2011$count))
    T2011_n2010 <- data.frame(MSRP = T2011$MSRP,
                              count = rmultinom(1, n_2010_Tianjin, T2010$count))
    cat("\n")
    print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

    placeboB <- diftrans(B2011_n2010, B2011_n2011, bandwidth = bandwidth_seq,
                         # conservative = T,
                         quietly = T)
    placeboT <- diftrans(T2011_n2010, T2011_n2011, bandwidth = bandwidth_seq,
                         quietly = T)

    placeboB_prop[i, ] <- placeboB$main
    placeboT_prop[i, ] <- placeboT$main
  }

  diffprop <- placeboB_prop - placeboT_prop

  meandiff <- matrix(rep(apply(diffprop, 2, mean), numsim), nrow = numsim, byrow = T) # TRY THIS

  diffprop <- abs(diffprop - meandiff)

  bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                    diffprop = apply(diffprop, 2, mean)) %>%
    mutate(diffprop_lag = lag(diffprop, 1)) %>%
    mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
    mutate(closetozero = diffprop_diff < 1e-3) %>%
    mutate(closetozero_lag1 = lag(closetozero)) %>%
    mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
    mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)


  # d_a = first time that the difference in the mean cost at a particular bandwidth and the two previous bandwidths is less than 1e-2
  d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                             "bandwidth"]
  meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                      "diffprop"]

  message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  fig8_plot <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = bandwidth_selection,
                mapping = aes(x = bandwidth, y = diffprop*100),
                method = loess,
                se = F,
                color = "black",
                size = 0.5) +
    bmp_plot(data = bandwidth_selection,
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, max_bw, 1000),
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


### Figure 8 Exploration - 14 and 15---------------------------
fignum <- "8exp1415"
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2015_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2015, "n"] %>% as.numeric
  n_2014_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2014, "n"] %>% as.numeric
  n_2015_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2015, "n"] %>% as.numeric
  n_2014_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2014, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2014 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2014-01-01", upperdate = "2015-01-01")
  T2014 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2014-01-01", upperdate = "2015-01-01")

  B2015 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2015-01-01", upperdate = "2016-01-01")
  T2015 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2015-01-01", upperdate = "2016-01-01")

  max_bw <- 30000
  bandwidth_seq = seq(0, max_bw, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  for (i in seq_len(numsim)){
    B2015_n2015 <- data.frame(MSRP = B2015$MSRP,
                              count = rmultinom(1, n_2015_Beijing, B2015$count))
    B2015_n2014 <- data.frame(MSRP = B2015$MSRP,
                              count = rmultinom(1, n_2014_Beijing, B2014$count))
    T2015_n2015 <- data.frame(MSRP = T2015$MSRP,
                              count = rmultinom(1, n_2015_Tianjin, T2015$count))
    T2015_n2014 <- data.frame(MSRP = T2015$MSRP,
                              count = rmultinom(1, n_2014_Tianjin, T2014$count))
    cat("\n")
    print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

    placeboB <- diftrans(B2015_n2014, B2015_n2015, bandwidth = bandwidth_seq,
                         # conservative = T,
                         quietly = T)
    placeboT <- diftrans(T2015_n2014, T2015_n2015, bandwidth = bandwidth_seq,
                         quietly = T)

    placeboB_prop[i, ] <- placeboB$main
    placeboT_prop[i, ] <- placeboT$main
  }

  diffprop <- placeboB_prop - placeboT_prop

  meandiff <- matrix(rep(apply(diffprop, 2, mean), numsim), nrow = numsim, byrow = T) # TRY THIS

  diffprop <- abs(diffprop - meandiff)

  bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                    diffprop = apply(diffprop, 2, mean)) %>%
    mutate(diffprop_lag = lag(diffprop, 1)) %>%
    mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
    mutate(closetozero = diffprop_diff < 1e-3) %>%
    mutate(closetozero_lag1 = lag(closetozero)) %>%
    mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
    mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)


  # d_a = first time that the difference in the mean cost at a particular bandwidth and the two previous bandwidths is less than 1e-2
  d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                             "bandwidth"]
  meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                      "diffprop"]

  message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  fig8_plot <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = bandwidth_selection,
                mapping = aes(x = bandwidth, y = diffprop*100),
                method = loess,
                se = F,
                color = "black",
                size = 0.5) +
    bmp_plot(data = bandwidth_selection,
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, max_bw, 1000),
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
### Figure 8 lambda - 14 and 15 ---------------------------
fignum <- "8lambda1415"
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2015_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2015, "n"] %>% as.numeric
  n_2014_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2014, "n"] %>% as.numeric
  n_2015_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2015, "n"] %>% as.numeric
  n_2014_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2014, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2014 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2014-01-01", upperdate = "2015-01-01")
  T2014 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2014-01-01", upperdate = "2015-01-01")

  B2015 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2015-01-01", upperdate = "2016-01-01")
  T2015 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2015-01-01", upperdate = "2016-01-01")


  bandwidth_seq = seq(0, 15000, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  lambda_seq = c(1, 0.75, 0.5, 0.25, 0)
  lambda_data_final <- data.frame(bandwidth = bandwidth_seq)
  lambda_data_final2 <- data.frame(bandwidth = bandwidth_seq)

  for (lambda in lambda_seq){
    print(lambda)
    for (i in seq_len(numsim)){
      B2015_n2015 <- data.frame(MSRP = B2015$MSRP,
                                count = rmultinom(1, n_2015_Beijing, lambda*B2014$count+(1-lambda)*B2015$count))
      B2014_n2014 <- data.frame(MSRP = B2015$MSRP,
                                count = rmultinom(1, n_2014_Beijing, B2014$count))
      T2015_n2015 <- data.frame(MSRP = T2015$MSRP,
                                count = rmultinom(1, n_2015_Tianjin, lambda*T2014$count+(1-lambda)*T2015$count))
      T2014_n2014 <- data.frame(MSRP = T2015$MSRP,
                                count = rmultinom(1, n_2014_Tianjin, T2014$count))
      cat("\n")
      print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

      placeboB <- diftrans(B2014_n2014, B2015_n2015, bandwidth = bandwidth_seq,
                           # conservative = T,
                           quietly = T)
      placeboT <- diftrans(T2014_n2014, T2015_n2015, bandwidth = bandwidth_seq,
                           quietly = T)

      placeboB_prop[i, ] <- placeboB$main
      placeboT_prop[i, ] <- placeboT$main
    }

    diffprop <- placeboB_prop
    diffprop2 <- placeboT_prop

    meandiff <- matrix(rep(apply(diffprop, 2, mean), numsim), nrow = numsim, byrow = T) # TRY THIS
    meandiff2 <- matrix(rep(apply(diffprop2, 2, mean), numsim), nrow = numsim, byrow = T)

    diffprop <- abs(diffprop - meandiff)
    diffprop2 <- abs(diffprop2 - meandiff2)

    bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                      diffprop = apply(diffprop, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection", lambda, sep = "_")
    assign(name, bandwidth_selection, envir = .GlobalEnv)

    lambda_data <- bandwidth_selection %>%
      select(bandwidth, diffprop)
    names(lambda_data) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final <- left_join(lambda_data, lambda_data_final, by = "bandwidth")

    bandwidth_selection2 <- data.frame(bandwidth = bandwidth_seq,
                                       diffprop = apply(diffprop2, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection2", lambda, sep = "_")
    assign(name, bandwidth_selection2, envir = .GlobalEnv)

    lambda_data2 <- bandwidth_selection2 %>%
      select(bandwidth, diffprop)
    names(lambda_data2) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final2 <- left_join(lambda_data2, lambda_data_final2, by = "bandwidth")

    # d_a = first time that the difference in the mean cost at a particular bandwidth and the two previous bandwidths is less than 1e-2
    d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                               "bandwidth"]
    meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                        "diffprop"]

    message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  }

  plot_data <- lambda_data_final %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data,
             color = lambda,
             ggtitle = "Beijing",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "horizontal",
             legend.position = c(0.8, 0.93)) # +
  # geom_line(data = plot_data,
  #           mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
  #           # color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Beijing", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Beijing", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  plot_data2 <- lambda_data_final2 %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot2 <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data2,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data2,
             color = lambda,
             ggtitle = "Tianjin",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "vertical",
             legend.position = c(0.7, 0.7)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Tianjin", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Tianjin", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  message(paste("Figure ", fignum, "_lambda is complete.", sep = ""))
}




### Figure 8 lambda - 10 and 11 ---------------------------
fignum <- "8lambda1011"
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2011_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2010, "n"] %>% as.numeric
  n_2011_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2010, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2010 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")
  T2010 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")

  B2011 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")
  T2011 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")


  bandwidth_seq = seq(0, 15000, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  lambda_seq = c(1, 0.75, 0.5, 0.25, 0)
  lambda_data_final <- data.frame(bandwidth = bandwidth_seq)
  lambda_data_final2 <- data.frame(bandwidth = bandwidth_seq)

  for (lambda in lambda_seq){
    print(lambda)
    for (i in seq_len(numsim)){
      B2011_n2011 <- data.frame(MSRP = B2011$MSRP,
                                count = rmultinom(1, n_2011_Beijing, lambda*B2010$count+(1-lambda)*B2011$count))
      B2010_n2010 <- data.frame(MSRP = B2011$MSRP,
                                count = rmultinom(1, n_2010_Beijing, B2010$count))
      T2011_n2011 <- data.frame(MSRP = T2011$MSRP,
                                count = rmultinom(1, n_2011_Tianjin, lambda*T2010$count+(1-lambda)*T2011$count))
      T2010_n2010 <- data.frame(MSRP = T2011$MSRP,
                                count = rmultinom(1, n_2010_Tianjin, T2010$count))
      cat("\n")
      print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

      placeboB <- diftrans(B2010_n2010, B2011_n2011, bandwidth = bandwidth_seq,
                           # conservative = T,
                           quietly = T)
      placeboT <- diftrans(T2010_n2010, T2011_n2011, bandwidth = bandwidth_seq,
                           quietly = T)

      placeboB_prop[i, ] <- placeboB$main
      placeboT_prop[i, ] <- placeboT$main
    }

    diffprop <- placeboB_prop
    diffprop2 <- placeboT_prop

    meandiff <- matrix(rep(apply(diffprop, 2, mean), numsim), nrow = numsim, byrow = T) # TRY THIS
    meandiff2 <- matrix(rep(apply(diffprop2, 2, mean), numsim), nrow = numsim, byrow = T)

    diffprop <- abs(diffprop - meandiff)
    diffprop2 <- abs(diffprop2 - meandiff2)

    bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                      diffprop = apply(diffprop, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection", lambda, sep = "_")
    assign(name, bandwidth_selection, envir = .GlobalEnv)

    lambda_data <- bandwidth_selection %>%
      select(bandwidth, diffprop)
    names(lambda_data) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final <- left_join(lambda_data, lambda_data_final, by = "bandwidth")

    bandwidth_selection2 <- data.frame(bandwidth = bandwidth_seq,
                                       diffprop = apply(diffprop2, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection2", lambda, sep = "_")
    assign(name, bandwidth_selection2, envir = .GlobalEnv)

    lambda_data2 <- bandwidth_selection2 %>%
      select(bandwidth, diffprop)
    names(lambda_data2) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final2 <- left_join(lambda_data2, lambda_data_final2, by = "bandwidth")

    # d_a = first time that the difference in the mean cost at a particular bandwidth and the two previous bandwidths is less than 1e-2
    d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                               "bandwidth"]
    meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                        "diffprop"]

    message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  }

  plot_data <- lambda_data_final %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data,
             color = lambda,
             ggtitle = "Beijing",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "horizontal",
             legend.position = c(0.8, 0.93)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Beijing", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Beijing", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  plot_data2 <- lambda_data_final2 %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot2 <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data2,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data2,
             color = lambda,
             ggtitle = "Tianjin",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "vertical",
             legend.position = c(0.7, 0.7)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Tianjin", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Tianjin", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  message(paste("Figure ", fignum, "_lambda is complete.", sep = ""))
}


### Figure 8 lambda - 10 and 11 diff samplesizes (wip) ---------------------------
fignum <- "8lambda1011_samplesizes0.7"
if (show_fig | show_fig8){
  set.seed(16 + seedplus)
  BTS <- do.call("rbind", list(Beijing, Tianjin, Shijiazhuang))
  year_count <- BTS %>%
    group_by(city, year) %>%
    count()
  n_2011_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Beijing <- year_count[year_count$city == "Beijing" & year_count$year == 2010, "n"] %>% as.numeric
  n_2011_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2011, "n"] %>% as.numeric
  n_2010_Tianjin <- year_count[year_count$city == "Tianjin" & year_count$year == 2010, "n"] %>% as.numeric

  supportB <- prep_data(BTS %>% filter(city == "Beijing"),
                        prep = "support")
  supportT <- prep_data(BTS %>% filter(city == "Tianjin"),
                        prep = "support")

  B2010 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")
  T2010 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2010-01-01", upperdate = "2011-01-01")

  B2011 <- prep_data(Beijing, prep = "pmf",
                     support = supportB,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")
  T2011 <- prep_data(Tianjin, prep = "pmf",
                     support = supportT,
                     lowerdate = "2011-01-01", upperdate = "2012-01-01")


  bandwidth_seq = seq(0, 15000, 500)
  numsim <- 100
  placeboB_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))
  placeboT_prop <- matrix(NA_real_, numsim, length(bandwidth_seq))

  lambda_seq = c(1, 0.75, 0.5, 0.25, 0)
  lambda_data_final <- data.frame(bandwidth = bandwidth_seq)
  lambda_data_final2 <- data.frame(bandwidth = bandwidth_seq)

  for (lambda in lambda_seq){
    print(lambda)
    for (i in seq_len(numsim)){
      B2011_n2011 <- data.frame(MSRP = B2011$MSRP,
                                count = rmultinom(1, 0.7*n_2011_Beijing, lambda*B2010$count+(1-lambda)*B2011$count))
      B2010_n2010 <- data.frame(MSRP = B2011$MSRP,
                                count = rmultinom(1, 0.7*n_2010_Beijing, B2010$count))
      # T2011_n2011 <- data.frame(MSRP = T2011$MSRP,
      #                           count = rmultinom(1, n_2011_Tianjin, T2011$count))
      # T2010_n2010 <- data.frame(MSRP = T2011$MSRP,
      #                           count = rmultinom(1, n_2010_Tianjin, T2010$count))
      T2011_n2011 <- data.frame(MSRP = T2011$MSRP,
                                count = rmultinom(1, 0.7*n_2011_Beijing, T2011$count)) # use Beijing sample sizes
      T2010_n2010 <- data.frame(MSRP = T2011$MSRP,
                                count = rmultinom(1, 0.7*n_2010_Beijing, T2010$count)) # use Beijing sample sizes
      cat("\n")
      print(paste("Simulation Number ", i, " out of ", numsim, sep = ""))

      placeboB <- diftrans(B2010_n2010, B2011_n2011, bandwidth = bandwidth_seq,
                           # conservative = T,
                           quietly = T)
      placeboT <- diftrans(T2010_n2010, T2011_n2011, bandwidth = bandwidth_seq,
                           quietly = T)

      placeboB_prop[i, ] <- placeboB$main
      placeboT_prop[i, ] <- placeboT$main
    }

    diffprop <- placeboB_prop
    diffprop2 <- placeboT_prop

    meandiff <- matrix(rep(apply(diffprop, 2, mean), numsim), nrow = numsim, byrow = T) # TRY THIS
    meandiff2 <- matrix(rep(apply(diffprop2, 2, mean), numsim), nrow = numsim, byrow = T)

    diffprop <- abs(diffprop - meandiff)
    diffprop2 <- abs(diffprop2 - meandiff2)

    bandwidth_selection <- data.frame(bandwidth = bandwidth_seq,
                                      diffprop = apply(diffprop, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection", lambda, sep = "_")
    assign(name, bandwidth_selection, envir = .GlobalEnv)

    lambda_data <- bandwidth_selection %>%
      select(bandwidth, diffprop)
    names(lambda_data) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final <- left_join(lambda_data, lambda_data_final, by = "bandwidth")

    bandwidth_selection2 <- data.frame(bandwidth = bandwidth_seq,
                                      diffprop = apply(diffprop2, 2, mean)) %>%
      mutate(diffprop_lag = lag(diffprop, 1)) %>%
      mutate(diffprop_diff = abs(diffprop - diffprop_lag)) %>%
      mutate(closetozero = diffprop_diff < 1e-3) %>%
      mutate(closetozero_lag1 = lag(closetozero)) %>%
      mutate(closetozero_lag2 = lag(closetozero, 2)) %>%
      mutate(valid = closetozero*closetozero_lag1*closetozero_lag2)

    name <- paste("bandwidth_selection2", lambda, sep = "_")
    assign(name, bandwidth_selection2, envir = .GlobalEnv)

    lambda_data2 <- bandwidth_selection2 %>%
      select(bandwidth, diffprop)
    names(lambda_data2) <- c("bandwidth", paste("diffprop", lambda, sep = "_"))

    lambda_data_final2 <- left_join(lambda_data2, lambda_data_final2, by = "bandwidth")

    # d_a = first time that the difference in the mean cost at a particular bandwidth and the two previous bandwidths is less than 1e-2
    d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                               "bandwidth"]
    meancost_d_a <- bandwidth_selection[match(1, bandwidth_selection$valid),
                                        "diffprop"]

    message(paste("Figure 8 analysis: d_a = ", d_a, " with cost = ", meancost_d_a, sep = ""))

  }

  plot_data <- lambda_data_final %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data,
             color = lambda,
             ggtitle = "Beijing",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "horizontal",
             legend.position = c(0.8, 0.93)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Beijing", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Beijing", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  plot_data2 <- lambda_data_final2 %>%
    pivot_longer(cols = contains("diffprop"), names_to = "lambda", values_to = "diffprop") %>%
    mutate(lambda = stringr::str_extract(lambda, "\\-*\\d+\\.*\\d*"))

  fig8lambda_plot2 <- ggplot() +
    # bmp_vline(xint = d_a / 2) +
    # bmp_vline(xint = d_a) +
    geom_smooth(data = plot_data2,
                mapping = aes(x = bandwidth, y = diffprop*100, color = lambda),
                method = loess,
                se = F,
                # color = "black",
                size = 0.5) +
    # theme_bmp() +
    # xlab("d") +
    # ylab("Transport Cost (%)") +
    # scale_x_continuous(breaks = seq(0, 15000, 1000))
    bmp_plot(data = plot_data2,
             color = lambda,
             ggtitle = "Tianjin",
             xlab = "d",
             ylab = "Transport Cost (%)",
             xtype = "continuous",
             xbreaks = seq(0, 15000, 1000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5),
             legend.direction = "vertical",
             legend.position = c(0.7, 0.7)) # +
  # geom_line(data = bandwidth_selection,
  #           mapping = aes(x = bandwidth, y = diffprop),
  #           color = get_color_palette(2, grayscale)[[2]],
  #           linetype = linetype1)



  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "Tianjin", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, suffix, "Tianjin", suffix, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
    # print("lambda is over.")
  }

  message(paste("Figure ", fignum, "_lambda is complete.", sep = ""))
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

  temp <- diftrans(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                      bandwidth_seq = bandwidth_seq,
                      conservative = F,
                      quietly = T)
  bandwidth_selection <- temp %>%
    pivot_longer(cols = c(main, control, diff),
                 names_to = "type",
                 values_to = "diffprop") %>%
    mutate(type = case_when(type == "main" ~ "Beijing",
                            type == "control" ~ "Tianjin",
                            type == "diff" ~ "a"))
  # bandwidth_selection %>% filter(type == "a") %>% mutate(percent_diffprop = 100*diffprop) %>% View()

  fig9_plot <- ggplot(data = bandwidth_selection,
                       aes(x = bandwidth)) +
    geom_line(aes(y = diffprop*100, color = type, linetype = type)) +
    bmp_vline(xint = 2000) +
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

  cons_dit <- diftrans(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                          bandwidth_seq = bandwidth_seq,
                          conservative = T,
                          save_dit = T)
  dit <- diftrans(pre_Bpmf, post_Bpmf, pre_Tpmf, post_Tpmf,
                     bandwidth_seq = bandwidth_seq,
                     conservative = F,
                     save_dit = T)

  bandwidth_selection <- cons_dit$out %>%
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


  mostinform <- dit$optimal_bandwidth
  mostinform2 <- cons_dit$optimal_bandwidth

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

