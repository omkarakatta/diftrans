### Meta ---------------------------
###
### Title: penalty.r
###
### Description: Carry out the task laid out by 3_BMP_GP/notes/penalty.pdf
###
### Author: Omkar A. Katta
###
### ---------------------------
###
### Notes:
###
###

# Preliminaries ---------------------------
devtools::load_all()
library(ggplot2)
source(here::here("data-raw", "prepare_data.R"))
source(here::here("data-raw", "plotting_functions.R"))

version <- "original"
grayscale <- FALSE # toggle to TRUE for black-and-white plots
temp <- ifelse(grayscale, "grayscale", "color")

fontsize <- 20 # change font size of plot, axis, and legend titles
fontsizeaxis <- 15 # change font size of axis labels
linetype0 <- "solid" # main line type
linetype1 <- "dashed" # secondary line type
linetype2 <- "dotted" # tertiary line type
linetype3 <- "twodash"
linetype4 <- "longdash"

img_path <- paste("/Users/omkar_katta/BFI/3_BMP_GP/img/img_misc/Nov03", paste(version, temp, sep = "-"), sep = "/")
suffix <- "_"
default_width <- 7
default_height <- 3

load(here::here(".hidden/Beijing_cleaned.RData"))
Beijing_sample <- Beijing_cleaned

if (version == "original"){
  Beijing <- Beijing_sample
  message(paste("Version: ", version, sep = ""))
} else if (version == "v1"){
  # filter out Dec 2010
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12))
  message(paste("Version: ", version, sep = ""))
} else if (version == "v2"){
  # filter out Dec 2010, Jan 2011, and Feb 2011
  Beijing <- Beijing_sample %>%
    filter(!(year == 2010 & month == 12)) %>%
    filter(!(year == 2011 & month %in% c(1, 2)))
  message(paste("Version: ", version, sep = ""))
} else {
  stop("Specify a valid value for `version`.")
}

# create support
support <- prep_data(data = Beijing, prep = "support")
pre <- prep_data(Beijing, prep = "pmf",
                 support = support,
                 lowerdate = "2010-01-01", upperdate = "2011-01-01")
post <- prep_data(Beijing, prep = "pmf",
                  support = support,
                  lowerdate = "2011-01-01", upperdate = "2012-01-01")

# create L1 penalty cost
Lpcost <- function(min_pre_support, min_post_support, p) {
  costm <- matrix(NA_real_, nrow = length(min_pre_support), ncol = length(min_post_support))
  for (i in seq_along(min_pre_support)) {
      costm[i, ] <- abs(min_pre_support[i] - min_post_support)^p
  }
  costm
}

l1_cost <- Lpcost(support, support, 1)

# initialize values
bandwidth_seq <- seq(0, 100000, 1000)
lambda_seq <- c(0, 0.01)
results <- matrix(NA_real_,
                  nrow = length(bandwidth_seq), ncol = length(lambda_seq))

# compute data
pb <- txtProgressBar(0, length(bandwidth_seq))
for (i in seq_along(bandwidth_seq)) {
  setTxtProgressBar(pb, i)
  bw <- bandwidth_seq[i]
  l0_cost <- build_costmatrix(support = support, bandwidth = bw)
  for (j in seq_along(lambda_seq)) {
    lambda <- lambda_seq[j]
    cost_mat <- l0_cost + lambda * l1_cost
    OT <- get_OTcost(pre, post, bandwidth = bw, costmat = cost_mat)$prop_bribe * 100
    results[i, j] <- OT
  }
}
colnames(results) <- paste("lambda", lambda_seq, sep = "")

#~ # sanity check
#~ real <- diftrans(pre, post, bandwidth_seq = bandwidth_seq, conservative = F)$main * 100
#~ test <- results[, "lambda0"]
#~ stopifnot(identical(real, test))

plot_table <- data.frame(results) %>%
  dplyr::mutate(bandwidth = bandwidth_seq) %>%
  tidyr::pivot_longer(cols = contains("lambda"))

ggplot(plot_table, aes(x = bandwidth, y = value,
                       color = name, linetype = name)) +
  geom_line() +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5)) +
  scale_linetype_manual(values = c(linetype0, linetype1),
                        labels = c("real", "lambda = 0.01"), name = "") +
  scale_color_manual(values = get_color_palette(2, grayscale),
                     labels = c("real", "lambda = 0.01"),
                     name = "") +
  scale_x_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100000, 10000)) +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  xlab("d") +
  ylab("Transport Cost (%)")

ggsave(paste("fig", suffix, "penalty", suffix, "OK.jpg", sep = ""),
       path = img_path,
       width = default_width, height = default_height, units = "in")

# HISTOGRAM OF UNIQUE SOLN
d <- 25000
lambda <- 0.01
l1_cost <- Lpcost(support, support, 1)
l0_cost <- build_costmatrix(support = support, bandwidth = d)
cost_mat <- l0_cost + lambda + l1_cost
OT <- transport(
    as.numeric(sum(post$count) / sum(pre$count) * pre$count),
    as.numeric(post$count),
    cost_mat
)

pre_support <- unique(pre$MSRP[pre$count != 0 & !is.na(pre$MSRP)])
post_support <- unique(post$MSRP[post$count != 0 & !is.na(post$MSRP)])
OT_revised <- OT %>%
  dplyr::mutate(from = pre_support[from]) %>%
  dplyr::mutate(to = post_support[to])
common <- unique(sort(c(OT_revised$from, OT_revised$to)))
OT_final <- OT_revised %>%
  #~ dplyr::rowwise() %>%
  #~ dplyr::mutate(from_index = which(common == from)) %>%
  #~ dplyr::mutate(to_index = which(common == to)) %>%
  #~ dplyr::ungroup() %>%
  dplyr::mutate(abs_diff = abs(from - to)) %>%
  dplyr::group_by(abs_diff) %>%
  dplyr::summarise(total = sum(mass))

#~ bar graph
ggplot(OT_final) +
  geom_bar(aes(x = abs_diff, y = total), stat = "identity")

#~ default histogram
OT_uncount <- OT_final %>%
  tidyr::uncount(total)
ggplot(OT_uncount) +
  geom_histogram(aes(x = abs_diff))

#~ binning
OT_final_bin <- OT_revised %>%
  dplyr::mutate(abs_diff = abs(from - to))
binwidth <- 10000
bins <- seq(0, max(common) + binwidth, by = binwidth)
labels <- paste(">", bins)
labels <- labels[1:(length(labels)-1)]
binvalue_raw <- cut(OT_final_bin$abs_diff, breaks = bins, labels = labels,
                    include.lowest = FALSE, right = TRUE)
binvalue <- addNA(binvalue_raw) #~ ensure <NA> is a factor as NA
levels(binvalue) <- c(levels(binvalue_raw), "= 0") #~ replace level NA with "= 0"
binvalue <- factor(binvalue, #~ reorganize levels so that "= 0" comes first
                   levels(binvalue)[c(length(levels(binvalue)),
                                      1:(length(levels(binvalue))-1))])
OT_plot_bin <- cbind(OT_final_bin, binvalue) %>%
  dplyr::group_by(binvalue) %>%
  dplyr::summarise(total = sum(mass))

ggplot() +
  geom_bar(data = OT_plot_bin, stat = "identity",
           aes(x = binvalue, y = total)) +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5),
            xangle = 90) +
  scale_y_continuous(breaks = seq(0, 100000, 5000),
                     labels = seq(0, 100000, 5000)) +
  xlab("binned absolute difference") +
  ylab("total mass transferred")

ggsave(paste("fig", "penalty", "bin", "25000.jpg", sep = "_"),
       path = img_path, width = default_width, height = default_height, units = "in")


#~ running sum of penalized optimal transport at d* = 25000
total_mass <- sum(OT_final$total)
OT_running_sum <- OT_final %>%
  dplyr::arrange(desc(abs_diff)) %>%
  dplyr::mutate(cumulative = cumsum(total)) %>%
  dplyr::mutate(percentage = cumulative / total_mass * 100) %>%
  dplyr::arrange(abs_diff)

ggplot(OT_running_sum, aes(x = abs_diff, y = percentage)) +
  geom_line() +
  theme_bmp(sizefont = (fontsize - 8), axissizefont = (fontsizeaxis - 5)) +
  xlab("d") +
  ylab("Mass above d (% of total mass)") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_continuous(breaks = seq(0, max(OT_running_sum$abs_diff), 100000))

ggsave(paste("fig", "penalty", "run", "25000.jpg", sep = "_"),
       path = img_path, width = default_width, height = default_height, units = "in")

OT_running_sum %>%
  dplyr::filter(abs_diff %% 10000 < 1000) %>%
  dplyr::select(abs_diff, cumulative, percentage) %>%
  knitr::kable(., format = "latex", booktabs = T, linesep = "", longtable = T, align = 'c')
