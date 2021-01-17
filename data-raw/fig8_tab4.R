set.seed(11 + seedplus)
support <- prep_data(data = Beijing, prep = "support")

preB <- prep_data(Beijing, prep = "pmf",
               support = support,
               lowerdate = "2010-01-01", upperdate = "2011-01-01")
postB <- prep_data(Beijing, prep = "pmf",
                support = support,
                lowerdate = "2011-01-01", upperdate = "2012-01-01")
preT <- prep_data(Tianjin, prep = "pmf",
               support = support,
               lowerdate = "2010-01-01", upperdate = "2011-01-01")
postT <- prep_data(Tianjin, prep = "pmf",
                support = support,
                lowerdate = "2011-01-01", upperdate = "2012-01-01")
bandwidth_seq <- seq(0, 100000, 1000)

realB <- diftrans(preB, postB, bandwidth_seq = bandwidth_seq, conservative = F)
realT <- diftrans(preT, postT, bandwidth_seq = bandwidth_seq, conservative = F)
realB_2d <- diftrans(preB, postB, bandwidth_seq = bandwidth_seq, conservative = T)

numsims <- 500
placebo_resultsB <- matrix(NA, nrow = numsims, ncol = length(bandwidth_seq))
placebo_resultsT <- matrix(NA, nrow = numsims, ncol = length(bandwidth_seq))
for (i in seq_len(numsims)) {
  print(paste("Simulation Number", i, "out of", numsims, sep = " "))
  # draw n_B,2010 samples from Beijing, 2010
  synth_pre1B <- data.frame(MSRP = preB$MSRP,
                             count = rmultinom(1, sum(preB$count), preB$count))
  # draw n_B,2011 samples from Beijing, 2010
  synth_pre2B <- data.frame(MSRP = preB$MSRP,
                             count = rmultinom(1, sum(postB$count), preB$count))
  placeboB <- diftrans(synth_pre1B, synth_pre2B,
                       bandwidth_seq = bandwidth_seq, conservative = F)
  placebo_resultsB[i, ] <- placeboB$main

  # draw n_T,2010 samples from Tianjin, 2010
  synth_pre1T <- data.frame(MSRP = preT$MSRP,
                             count = rmultinom(1, sum(preT$count), preT$count))
  # draw n_T,2011 samples from Tianjin, 2010
  synth_pre2T <- data.frame(MSRP = preT$MSRP,
                             count = rmultinom(1, sum(postT$count), preT$count))
  placeboT <- diftrans(synth_pre1T, synth_pre2T,
                       bandwidth_seq = bandwidth_seq, conservative = F)
  placebo_resultsT[i, ] <- placeboT$main
}

placebo_meanB <- apply(placebo_resultsB, 2, mean) * 100
placebo_sdB <- apply(placebo_resultsB, 2, sd) * 100
probs <- c(0.90, 0.95, 0.99)
placebo_quantilesB <- apply(placebo_resultsB, 2, quantile, prob = probs) * 100

placebo_meanT <- apply(placebo_resultsT, 2, mean) * 100
placebo_sdT <- apply(placebo_resultsT, 2, sd) * 100
probs <- c(0.90, 0.95, 0.99)
placebo_quantilesT <- apply(placebo_resultsT, 2, quantile, prob = probs) * 100

plot_matB <- do.call(rbind, list(placebo_meanB, placebo_sdB, placebo_quantilesB))
rownames(plot_matB) <- c("mean", "sd", paste("quantile", probs, sep = ""))
plot_tableB <- t(plot_matB) %>%
  as.data.frame() %>%
  mutate(bandwidth = bandwidth_seq,
         real = realB$main * 100) %>%
  select(bandwidth, real, everything())

plot_matT <- do.call(rbind, list(placebo_meanT, placebo_sdT, placebo_quantilesT))
rownames(plot_matT) <- c("mean", "sd", paste("quantile", probs, sep = ""))
plot_tableT <- t(plot_matT) %>%
  as.data.frame() %>%
  mutate(bandwidth = bandwidth_seq,
         real = realT$main * 100) %>%
  select(bandwidth, real, everything())

# close to mean placebo = 0.05%
lower <- 0.04
upper <- 0.06
validB <- plot_tableB$bandwidth[plot_tableB$mean < upper & plot_tableB$mean > lower]
validT <- plot_tableT$bandwidth[plot_tableT$mean < upper & plot_tableT$mean > lower]

# replicate table 4 - Beijing
d_tableB <- plot_tableB %>%
filter(bandwidth %in% c(validB,
                        0,
                        4000, 5000, 6000, 7000, 8000, 9000, 10000,
                        15000, 20000, 25000, 30000, 35000, 40000, 45000,
                        50000, 70000, 90000))
knitr::kable(d_tableB, format = "latex", booktabs = T, linesep = "")


# replicate table 4 - Tianjin
d_tableT <- plot_tableT %>%
filter(bandwidth %in% c(validT,
                        0,
                        4000, 5000, 6000, 7000, 8000, 9000, 10000,
                        15000, 20000, 25000, 30000, 35000, 40000, 45000,
                        50000, 70000, 90000))
knitr::kable(d_tableT, format = "latex", booktabs = T, linesep = "")

# average of absolute difference, with 2d

max_bw <- max(bandwidth_seq)
half_max_bw <- which(bandwidth_seq == max_bw / 2)
diff_2d <- matrix(NA_real_, nrow = half_max_bw, ncol = 6)
for (i in seq_len(half_max_bw)) {
  bw <- bandwidth_seq[i]
  B_2d <- placebo_resultsB[, which(bandwidth_seq == 2 * bw)]
  T_d <- placebo_resultsT[, which(bandwidth_seq == bw)]
  diff <- abs(B_2d - T_d)
  diff_2d[i, ] <- c(bw,
                    mean(diff) * 100,
                    sd(diff) * 100,
                    quantile(diff, prob = probs) * 100)
}

knitr::kable(diff_2d[which(bandwidth_seq %in%
                           c(validB,
                             0,
                             4000, 5000, 6000, 7000, 8000, 9000, 10000,
                             15000, 20000, 25000, 30000, 35000, 40000, 45000,
                             50000)), ],
             format = "latex", booktabs = T, linesep = "")

# average of absolute difference, with d

diff_d <- matrix(NA_real_, nrow = length(bandwidth_seq), ncol = 6)
for (i in seq_along(bandwidth_seq)) {
  bw <- bandwidth_seq[i]
  B_d <- placebo_resultsB[, which(bandwidth_seq == bw)]
  T_d <- placebo_resultsT[, which(bandwidth_seq == bw)]
  diff <- abs(B_d - T_d)
  diff_d[i, ] <- c(bw,
                   mean(diff) * 100,
                   sd(diff) * 100,
                   quantile(diff, prob = probs) * 100)
}
knitr::kable(diff_d[which(bandwidth_seq %in%
                          c(validB,
                            0,
                            4000, 5000, 6000, 7000, 8000, 9000, 10000,
                            15000, 20000, 25000, 30000, 35000, 40000, 45000,
                            50000)), ], format = "latex", booktabs = T, linesep = "")

################## Jan 7

valid_d <- c(0,
             4000, 5000, 6000, 7000, 8000, 9000, 10000,
             15000, 20000,
             23000, 24000,
             25000, 30000, 35000, 40000, 45000,
             50000)
filtered_B <- realB_2d[which(bandwidth_seq %in% valid_d),
                      "main2d"] * 100
filtered_T <- realT[which(bandwidth_seq %in% valid_d),
                      "main"] * 100
real <- data.frame(bandwidth = valid_d,
                   real_Beijing_2d = filtered_B,
                   real_Tianjin_d = filtered_T) %>%
  mutate(diff = real_Beijing_2d - real_Tianjin_d) %>%
  mutate(abs_diff = abs(diff))

knitr::kable(real,
             format = "latex", booktabs = T, linesep = "")

#~ temp <- diftrans(pre_main = preB, post_main = postB,
#~          pre_control = preT, post_control = postT, bandwidth_seq = valid_d,
#~          conservative = T)

################# Dec 25

### Figure 6 -- post instead of pre ---------------------------


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
    # draw n_B,2010 samples from Beijing, 2011
    synth_post1 <- data.frame(MSRP = post$MSRP,
                              count = rmultinom(1, sum(pre$count), post$count))
    # draw n_B,2011 samples from Beijing, 2011
    synth_post2 <- data.frame(MSRP = post$MSRP,
                              count = rmultinom(1, sum(post$count), post$count))
    placebo <- diftrans(synth_post1, synth_post2,
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
    geom_line(aes(y = mean, color = "5", linetype = "5")) +
    scale_color_manual(values = get_color_palette(2, grayscale),
                       labels = c("real", "placebo"),
                       name = "") +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("real", "placebo"),
                          name = "") +
    bmp_vline(xint = 25000) +
    xlab(TeX("\\textit{d}")) +
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
    xlab(TeX("\\textit{d}")) +
    ylab("Transport Cost (%)") +
    theme_bmp(sizefont = (fontsize - 8),
              axissizefont = (fontsizeaxis - 5),
              legend.position = c(0.8, 0.7)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.1)) +
    scale_x_continuous(breaks = seq(0, 100000, 10000))

ggsave("fig6_with_post.jpg", fig6_plot, path = img_path)
ggsave("fig6_with_post_zoomed.jpg", fig6_plot2, path = img_path)


### Using what was previously inequality (16)


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
  B_emp <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))
  B_sim <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))
  T_emp <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))
  T_sim <- matrix(NA_real_, nrow = numsim, ncol = length(bandwidth_seq))

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

    B_sim[i, ] <- diftrans(B_pre_tilde, B_post_tilde, bandwidth = bandwidth_seq, conservative = T)$main2d
    B_emp[i, ] <- diftrans(B_pre, B_post, bandwidth = bandwidth_seq, conservative = T)$main2d
    T_sim[i, ] <- diftrans(T_pre_tilde, T_post_tilde, bandwidth = bandwidth_seq, conservative = F)$main
    T_emp[i, ] <- diftrans(T_pre, T_post, bandwidth = bandwidth_seq, conservative = F)$main
  }

  LHS <- B_sim - B_emp
  RHS <- T_sim - T_emp
  diff <- RHS - LHS

  LHS_mean <- apply(LHS, 2, mean)*100
  RHS_mean <- apply(RHS, 2, mean)*100
  diff_mean <- apply(diff, 2, mean)*100


  sum(LHS_mean < RHS_mean)
  plot_table <- data.frame(d = bandwidth_seq, LHS = LHS_mean, RHS = RHS_mean, diff = diff_mean) %>%
    pivot_longer(cols = c(LHS, RHS, diff))
  bandwidth_seq[diff_mean >= 0]

  #~ no smoothing, LHS vs RHS vs RHS-LHS
  ggplot(plot_table, aes(x = d)) +
    geom_line(aes(y = value, color = name, linetype = name)) +
    bmp_plot(data = plot_table,
           color = name,
           legendlabels = c("Overall difference", "Beijing difference", "Tianjin difference"),
           xlab = TeX("\\textit{d}"),
           ylab = "Difference in Transport Cost (%)",
           ytype = "continuous",
           # ybreaks = seq(-50, 100, 10),
           xtype = "continuous",
           xbreaks = seq(0, max_bw, 5000),
           sizefont = (fontsize - 8),
           axissizefont = (fontsizeaxis - 5)) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("Overall difference", "Beijing difference", "Tianjin difference"),
                          name = "")

  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "by1000_500sims", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  #~ with smoothing, LHS vs RHS vs RHS-LHS
  ggplot(plot_table, aes(x = d)) +
    geom_smooth(aes(y = value, color = name, linetype = name),
                method = loess, se = F, size = 0.5) +
    bmp_plot(data = plot_table,
           color = name,
           legendlabels = c("Overall difference", "Beijing difference", "Tianjin difference"),
           xlab = TeX("\\textit{d}"),
           ylab = "Difference in Transport Cost (%)",
           ytype = "continuous",
           # ybreaks = seq(-50, 100, 10),
           xtype = "continuous",
           xbreaks = seq(0, max_bw, 5000),
           sizefont = (fontsize - 8),
           axissizefont = (fontsizeaxis - 5)) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype2),
                          labels = c("Overall difference", "Beijing difference", "Tianjin difference"),
                          name = "")

  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "loess_by1000_500sims", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }

  #~ for exploring: all four terms of (16)
  B_sim_mean <- apply(B_sim, 2, mean)*100
  B_emp_mean <- apply(B_emp, 2, mean)*100
  T_sim_mean <- apply(T_sim, 2, mean)*100
  T_emp_mean <- apply(T_emp, 2, mean)*100
  prep_table <- data.frame(d = bandwidth_seq,
                           B_sim = B_sim_mean,
                           B_emp = B_emp_mean,
                           T_sim = T_sim_mean,
                           T_emp = T_emp_mean)
  kable_table <- prep_table %>%
    mutate(diff_B = B_sim - B_emp) %>%
    mutate(diff_T = T_sim - T_emp) %>%
    mutate(diff_emp = B_emp - T_emp) %>%
    mutate(diff_sim = B_sim - T_sim) %>%
    mutate(overall = diff_T - diff_B)
  knitr::kable(kable_table, format = "latex", booktabs = T, linesep = "", digits = 4)

  plot_table <- prep_table %>%
    pivot_longer(cols = c(B_emp, B_sim, T_emp, T_sim))

  ggplot(plot_table, aes(x = d)) +
    geom_line(aes(y = value, color = name, linetype = name)) +
    bmp_plot(data = plot_table,
             color = name,
             legendlabels = c("Empirical Beijing", "Simulated Beijing",
                              "Empirical Tianjin", "Simulated Tianjin"),
             xlab = TeX("\\textit{d}"),
             ylab = "Difference in Transport Cost (%)",
             ytype = "continuous",
             # ybreaks = seq(-50, 100, 10),
             xtype = "continuous",
             xbreaks = seq(0, max_bw, 5000),
             sizefont = (fontsize - 8),
             axissizefont = (fontsizeaxis - 5)) +
    scale_linetype_manual(values = c(linetype0, linetype1, linetype3, linetype2),
                          labels = c("Empirical Beijing", "Simulated Beijing",
                                     "Empirical Tianjin", "Simulated Tianjin"),
                          name = "")

  if (save_fig | save_fig8){
    ggsave(paste("fig", fignum, suffix, "fourterms_by1000_500sims", suffix, "OK.jpg", sep = ""), path = img_path,
           width = default_width+2, height = default_height, units = "in")
    message(paste("fig", fignum, " is saved in ", img_path, " as fig", fignum, suffix, "OK.jpg", sep = ""))
  }
