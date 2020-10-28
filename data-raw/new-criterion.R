### Header ---------------------------
###
### Title: new-criterion.R
###
### Description: Implement new criterion for choosing bandwidth for before-and-after estimator
###
### Author: Omkar A. Katta
###
### Notes: See email exchange with Guillaume (Oct 21, 23)
###

### Preliminaries ---------------------------
devtools::load_all()
library(ggplot2)

source(here::here("data-raw", "prepare_data.R"))
source(here::here("data-raw", "plotting_functions.R"))
load(here::here(".hidden/Beijing_cleaned.RData"))
Beijing <- Beijing_cleaned
version <- "original"

set.seed(23)

grayscale <- FALSE # toggle to TRUE for black-and-white plots
temp <- ifelse(grayscale, "grayscale", "color")
fontsize <- 20 # change font size of plot, axis, and legend titles
fontsizeaxis <- 15 # change font size of axis labels
linetype0 <- "solid" # main line type
linetype1 <- "dashed" # secondary line type
linetype2 <- "dotted" # tertiary line type
linetype3 <- "twodash"
linetype4 <- "longdash"
img_path <- paste("/Users/omkar_katta/BFI/3_BMP_GP/img/img_misc/Oct28", paste(version, temp, sep = "-"), sep = "/")
suffix <- "_"
default_width <- 7
default_height <- 3

epsilon <- 0.1
bandwidth_seq <- seq(0, 40000, 1000)
numsims <- 500

common_support <- prep_data(Beijing, prep = "support", lowerdate = "2010-01-01", upperdate = "2013-01-01")
pre <- prep_data(Beijing, prep = "pmf",
                 support = common_support,
                 lowerdate = "2010-01-01", upperdate = "2011-01-01")
post <- prep_data(Beijing, prep = "pmf",
                  support = common_support,
                  lowerdate = "2012-01-01", upperdate = "2013-01-01")
post_total <- sum(post$count)

Lpcost <- function(min_pre_support, min_post_support, p) {
  costm <- matrix(NA_real_, nrow = length(min_pre_support), ncol = length(min_post_support))
  for (i in seq_along(min_pre_support)) {
      costm[i, ] <- abs(min_pre_support[i] - min_post_support)^p
  }
  costm
}
common_cost <- Lpcost(common_support, common_support, p = epsilon)


results_raw <- matrix(NA_real_, nrow = numsims, ncol = length(bandwidth_seq))
gamma_raw <- vector(mode = "list", length = numsims)
support_raw <- vector(mode = "list", length = numsims)
pb <- txtProgressBar(0, numsims)
for (sim in seq_len(numsims)) {
  setTxtProgressBar(pb, sim)
  # print(paste("Simulation ", sim, " out of ", numsims, sep = ""))

  pre_tilde <- data.frame(MSRP = pre$MSRP, count = rmultinom(1, sum(pre$count), pre$count))
  post_tilde <- data.frame(MSRP = post$MSRP, count = rmultinom(1, sum(post$count), post$count))

  hat_OT <- transport(
    as.numeric(sum(post$count) / sum(pre$count) * pre$count),
    as.numeric(post$count),
    common_cost
  )
  tilde_OT <- transport(
    as.numeric(sum(post_tilde$count) / sum(pre_tilde$count) * pre_tilde$count),
    as.numeric(post_tilde$count),
    common_cost
  )

  pre_support <- unique(pre$MSRP[pre$count != 0 & !is.na(pre$MSRP)])
  post_support <- unique(post$MSRP[post$count != 0 & !is.na(post$MSRP)])
  pre_tilde_support <- unique(pre_tilde$MSRP[pre_tilde$count != 0 & !is.na(pre_tilde$MSRP)])
  post_tilde_support <- unique(post_tilde$MSRP[post_tilde$count != 0 & !is.na(post_tilde$MSRP)])

  hat_OT_revised <- hat_OT %>%
    dplyr::rename(hat_mass = mass) %>%
    dplyr::mutate(from = pre_support[from]) %>%
    dplyr::mutate(to = post_support[to])
  tilde_OT_revised <- tilde_OT %>%
    dplyr::rename(tilde_mass = mass) %>%
    dplyr::mutate(from = pre_tilde_support[from]) %>%
    dplyr::mutate(to = post_tilde_support[to])
  OT_combine <- dplyr::full_join(hat_OT_revised, tilde_OT_revised, by = c("from", "to"))
  support <- unique(sort(c(OT_combine$from, OT_combine$to)))
  OT_final <- OT_combine %>%
    dplyr::rowwise() %>%
    dplyr::mutate(from = which(support == from)) %>%
    dplyr::mutate(to = which(support == to)) %>%
    dplyr::ungroup()

  hat_gamma <- matrix(0, nrow = length(support), ncol = length(support))
  tilde_gamma <- matrix(0, nrow = length(support), ncol = length(support))
  for (i in seq_len(nrow(OT_final))) {
    temprow <- OT_final[i, ]
    hat_gamma[as.numeric(temprow["from"]), as.numeric(temprow["to"])] <- as.numeric(temprow["hat_mass"])
    tilde_gamma[as.numeric(temprow["from"]), as.numeric(temprow["to"])] <- as.numeric(temprow["tilde_mass"])
  }
  hat_gamma[is.na(hat_gamma)] <- 0
  tilde_gamma[is.na(tilde_gamma)] <- 0

  gamma <- abs(tilde_gamma - hat_gamma)
  gamma_raw[[sim]] <- gamma
  support_raw[[sim]] <- support

  for (bw in seq_along(bandwidth_seq)) {
    bandwidth <- bandwidth_seq[bw]
    cost <- build_costmatrix(support, bandwidth = bandwidth)
    results_raw[sim, bw] <- sum(cost * gamma)
  }

}

results <- results_raw / post_total
probs <- c(0.9, 0.95, 0.99)
mean_results <- apply(results, 2, mean)
sd_results <- apply(results, 2, sd)
quantile_results <- apply(results, 2, quantile, probs)
plot_table <- data.frame(bandwidth = bandwidth_seq, mean = mean_results, sd = sd_results, t(quantile_results)) %>%
  tidyr::pivot_longer(cols = c(mean, sd, tidyselect::contains("X"))) %>%
  dplyr::mutate(name = dplyr::case_when(name == "X90." ~ "90%ile",
                                        name == "X95." ~ "95%ile",
                                        name == "X99." ~ "99%ile",
                                        TRUE ~ name))

ggplot(plot_table) +
  geom_line(aes(x = bandwidth, y = value * 100, 
                color = factor(name, levels = c("99%ile", "95%ile", "90%ile", "mean", "sd")))) +
  scale_color_manual(values = get_color_palette(5, grayscale),
                     labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                     name = "") +
  scale_linetype_manual(values = c(linetype0, linetype1, linetype2, linetype3, linetype4),
                        labels = c("99%ile", "95%ile", "90%ile", "mean", "st. dev."),
                        name = "") +
  # scale_y_continuous(breaks = seq(0, max(plot_table$value), 2000),
                     # labels = seq(0, max(plot_table$value), 2000)) +
  scale_y_continuous(breaks = seq(0, 8, 0.5), labels = seq(0, 8, 0.5)) +
  scale_x_continuous(breaks = seq(0, 40000, 4000), labels = seq(0, 40000, 4000)) +
  ylab("Percentage") +
  xlab("d") +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
            legend.direction = "vertical",
            legend.position = c(0.85, 0.8))

ggsave(paste("fig_newcriterion", suffix,  "perc", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height+1, units = "in")

if (length(unique(support_raw)) == 1) {
  #~ get support of gamma matrix
  final_support <- support_raw[[1]]
  #~ get mean of each entry in gamma matrix across all 500 simulations
  #~ save the result as a matrix and a vector
  gamma_mean_mat <- apply(simplify2array(gamma_raw), c(1, 2), mean)
  gamma_mean_vec <- c(gamma_mean_mat)
  gamma_dt <- data.frame(gamma_vec = gamma_mean_vec)
}

ggplot(gamma_dt) +
  geom_histogram(aes(x = gamma_vec, y = stat(count)/sum(stat(count))), binwidth = 10) +
  bmp_plot(data = gamma_dt,
           xlab = "Absolute Difference in Transport Maps",
           ylab = "Density",
           xtype = "continuous", xbreaks = seq(0, 600, 30), xlabels = seq(0, 600, 30),
           sizefont = (fontsize - 8),
           axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "gamma", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height, units = "in")

#~ collect all pairs of the support and respective indices
expand_support <- expand.grid(final_support, final_support) %>%
  dplyr::rename(from = Var1, to = Var2)
expand_index <- expand.grid(seq(1, length(final_support)),
                            seq(1, length(final_support))) %>%
  dplyr::rename(from_index = Var1, to_index = Var2)
#~ compute the absolute difference and the mean transfer for each support pair
plot_table <- cbind(expand_support, expand_index) %>%
  dplyr::mutate(abs_diff = abs(from - to)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gamma = gamma_mean_mat[from_index, to_index]) %>%
  dplyr::ungroup()
#~ compute mean transfer and total transfer at each absolute difference
plot_table2 <- plot_table %>%
  dplyr::group_by(abs_diff) %>%
  dplyr::summarise(mean = mean(gamma),
                   sum = sum(gamma))


#~ scatter
ggplot() +
  geom_point(data = plot_table, aes(x = abs_diff, y = gamma), alpha = 0.5) +
  ylab("average mass transferred across 500 simulations") +
  xlab("absolute difference between values of support") +
  scale_x_continuous(breaks = seq(0, max(plot_table$abs_diff), by = 100000),
                     labels = seq(0, max(plot_table$abs_diff), by = 100000)) +
  scale_y_continuous(breaks = seq(0, max(plot_table$gamma), by = 50),
                     labels = seq(0, max(plot_table$gamma), by = 50)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "scatter", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

#~ scatter with total
ymax <- max(plot_table2$sum[plot_table2$abs_diff > 0])
ggplot() +
  geom_point(data = plot_table, aes(x = abs_diff, y = gamma), alpha = 0.5) +
  geom_point(data = plot_table2, aes(x = abs_diff, y = sum), color = "red", size = 0.25) +
  ylab("average mass transferred across 500 simulations") +
  xlab("absolute difference between values of support") +
  scale_x_continuous(breaks = seq(0, max(plot_table$abs_diff), by = 100000),
                     labels = seq(0, max(plot_table$abs_diff), by = 100000)) +
  scale_y_continuous(breaks = seq(0, ymax, by = 50),
                     labels = seq(0, ymax, by = 50),
                     limits = c(0, ymax)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "total_scatter", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

#~ scatter with mean
ggplot() +
  geom_point(data = plot_table, aes(x = abs_diff, y = gamma), alpha = 0.5) +
  geom_point(data = plot_table2, aes(x = abs_diff, y = mean), color = "red", size = 0.25) +
  ylab("average mass transferred across 500 simulations") +
  xlab("absolute difference between values of support") +
  scale_x_continuous(breaks = seq(0, max(plot_table$abs_diff), by = 100000),
                     labels = seq(0, max(plot_table$abs_diff), by = 100000)) +
  scale_y_continuous(breaks = seq(0, max(plot_table$gamma), by = 50),
                     labels = seq(0, max(plot_table$gamma), by = 50)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "mean_scatter", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

#~ remove points that do not contribute to optimal cost
plot_table_cleaned <- plot_table %>%
  dplyr::filter(abs_diff != 0) %>%
  dplyr::filter(gamma != 0)
plot_table2_cleaned <- plot_table_cleaned %>%
  dplyr::group_by(abs_diff) %>%
  dplyr::summarise(mean = mean(gamma),
                   sum = sum(gamma))


#~ scatter
ggplot() +
  geom_point(data = plot_table_cleaned, aes(x = abs_diff, y = gamma), alpha = 0.5) +
  ylab("average mass transferred across 500 simulations") +
  xlab("absolute difference between values of support") +
  scale_x_continuous(breaks = seq(0, max(plot_table$abs_diff), by = 100000),
                     labels = seq(0, max(plot_table$abs_diff), by = 100000)) +
  scale_y_continuous(breaks = seq(0, max(plot_table$gamma), by = 50),
                     labels = seq(0, max(plot_table$gamma), by = 50)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "scatter", suffix, "cleaned", "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

#~ scatter with total
ggplot() +
  geom_point(data = plot_table_cleaned, aes(x = abs_diff, y = gamma), alpha = 0.5) +
  geom_point(data = plot_table2_cleaned, aes(x = abs_diff, y = sum), color = "red", size = 0.25) +
  ylab("average mass transferred across 500 simulations") +
  xlab("absolute difference between values of support") +
  scale_x_continuous(breaks = seq(0, max(plot_table_cleaned$abs_diff), by = 100000),
                     labels = seq(0, max(plot_table_cleaned$abs_diff), by = 100000)) +
  scale_y_continuous(breaks = seq(0, max(plot_table2_cleaned$sum)+50, by = 50),
                     labels = seq(0, max(plot_table2_cleaned$sum)+50, by = 50),
                     limits = c(0, max(plot_table2_cleaned$sum))) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5))

ggsave(paste("fig_newcriterion", suffix,  "total_scatter_clean", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

# Binning ------
binwidth <- 10000
bins <- seq(0, max(final_support) + binwidth, by = binwidth)
labels <- paste(">", bins)
labels <- labels[1:(length(labels)-1)]
binvalue_raw <- cut(plot_table$abs_diff, breaks = bins, labels = labels,
                include.lowest = FALSE, right = TRUE)
binvalue <- addNA(binvalue_raw) #~ ensure <NA> is a factor as NA
levels(binvalue) <- c(levels(binvalue_raw), "=0") #~ replace level NA with "= 0"
binvalue <- factor(binvalue, #~ reorganize levels so that "= 0" comes first
                   levels(binvalue)[c(length(levels(binvalue)),
                                      1:(length(levels(binvalue))-1))])
plot_table_bins <- cbind(plot_table, binvalue) %>%
  dplyr::group_by(binvalue) %>%
  dplyr::summarise(sum = sum(gamma))

ggplot() +
  geom_bar(data = plot_table_bins, stat = "identity",
           aes(x = binvalue, y = sum)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
            xangle = 90) +
  xlab("binned absolute difference") +
  ylab("total average mass transferred")

ggsave(paste("fig_newcriterion", suffix,  "total_bin", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")

#~ cleaner plot
plot_table_bins_cleaned <- plot_table_bins %>%
  dplyr::filter(sum != 0)

ggplot() +
  geom_bar(data = plot_table_bins_cleaned, stat = "identity",
           aes(x = binvalue, y = sum)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
            xangle = 90) +
  xlab("binned absolute difference") +
  ylab("total average mass transferred")

ggsave(paste("fig_newcriterion", suffix,  "total_bin_clean", suffix, "OK.jpg", sep = ""), path = img_path,
       width = default_width, height = default_height + 2, units = "in")
