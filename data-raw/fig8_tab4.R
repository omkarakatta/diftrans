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
