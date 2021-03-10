### Meta -------------------------
###
### Title: price_msrp_discrepancy2.R
###
### Description: Compare MSRP subsamples with matched MSRP values
###
### Author: Omkar A. Katta
###

### Preliminaries -------------------------

devtools::load_all()
library(dplyr)
library(tidyr)
library(ggplot2)

### Import -------------------------

matched_raw <- data.table::fread("~/BFI/3_BMP_GP/data-raw/Beijing_cleaned_merged.csv")

matched <- matched_raw %>%
  mutate(is_matched = !is.na(avg_price))
num_msrp <- nrow(matched)
num_matches <- sum(matched$is_matched)
msrp_support <- unique(matched_raw$msrp)
bandwidth_vec <- seq(0, 40000, 1000)

prep_msrp_pmf <- function(df, years, only_keep_matches, support) {
  if (only_keep_matches) {
    match_values <- c(TRUE)
  } else {
    match_values <- c(FALSE, TRUE)
  }
  df %>%
    filter(year %in% years) %>%
    filter(is_matched %in% match_values) %>%
    select(msrp, sales) %>%
    group_by(msrp) %>%
    summarize(total = sum(sales)) %>%
    full_join(data.frame(msrp = support), by = "msrp") %>%
    replace_na(list(total = 0)) %>%
    arrange(msrp) %>%
    ungroup()
}

msrp_full_2010 <- prep_msrp_pmf(matched, 2010, FALSE, msrp_support)
msrp_full_2011 <- prep_msrp_pmf(matched, 2011, FALSE, msrp_support)
msrp_match_2010 <- prep_msrp_pmf(matched, 2010, TRUE, msrp_support)
msrp_match_2011 <- prep_msrp_pmf(matched, 2011, TRUE, msrp_support)

### Weighted -------------------------

full <- diftrans(pre_main = msrp_full_2010,
                 post_main = msrp_full_2011,
                 bandwidth_vec = bandwidth_vec,
                 var = msrp,
                 count = total)
full_results <- full$empirical_table$result

match <- diftrans(pre_main = msrp_match_2010,
                  post_main = msrp_match_2011,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total)
match_results <- match$empirical_table$result

### Unweighted -------------------------

msrp_support_values <- unique(matched$msrp)

prep_msrp_values_pmf <- function(df, years, only_keep_matches, support) {
  if (only_keep_matches) {
    match_values <- c(TRUE)
  } else {
    match_values <- c(FALSE, TRUE)
  }
  df %>%
    filter(year %in% years) %>%
    filter(is_matched %in% match_values) %>%
    select(msrp) %>%
    group_by(msrp) %>%
    count() %>%
    rename(total = n) %>%
    full_join(data.frame(msrp = support), by = "msrp") %>%
    replace_na(list(total = 0)) %>%
    arrange(msrp) %>%
    ungroup()
}

msrp_full_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  FALSE,
  msrp_support_values
)
msrp_full_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  FALSE,
  msrp_support_values
)
msrp_match_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  TRUE,
  msrp_support_values
)
msrp_match_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  TRUE,
  msrp_support_values
)

bandwidth_vec <- seq(0, 40000, 1000)
full <- diftrans(pre_main = msrp_full_2010_values,
                 post_main = msrp_full_2011_values,
                 bandwidth_vec = bandwidth_vec,
                 var = msrp,
                 count = total)
full_results <- full$empirical_table$result

match <- diftrans(pre_main = msrp_match_2010_values,
                  post_main = msrp_match_2011_values,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total)
match_results <- match$empirical_table$result

ggplot() +
  geom_line(aes(x = bandwidth_vec, y = full_results), color = "steelblue") +
  geom_line(aes(x = bandwidth_vec, y = match_results), color = "orange") +
  theme_bw()

matched_2010 <- matched %>% filter(year == 2010)
matched_2011 <- matched %>% filter(year == 2011)
sims <- 100
results <- matrix(NA_real_, nrow = length(bandwidth_vec), ncol = sims)
store_indices_2010 <- matrix(NA_real_, nrow = nrow(matched_2010), ncol = sims)
store_indices_2011 <- matrix(NA_real_, nrow = nrow(matched_2011), ncol = sims)

for (sim in seq_len(sims)) {
  print(paste("Simulation", sim, "out of", sims))

  sub_index_2010 <- sample(seq_len(nrow(matched_2010)), sum(matched_2010$is_matched), replace = FALSE)
  sub_index_2011 <- sample(seq_len(nrow(matched_2011)), sum(matched_2011$is_matched), replace = FALSE)
  sub_df_2010 <- matched_2010[sub_index_2010, ]
  sub_df_2011 <- matched_2011[sub_index_2011, ]
  zeros_2010 <- rep(0, nrow(matched_2010))
  zeros_2010[sub_index_2010] <- 1
  zeros_2011 <- rep(0, nrow(matched_2011))
  zeros_2011[sub_index_2011] <- 1
  store_indices_2010[, sim] <- zeros_2010
  store_indices_2011[, sim] <- zeros_2011

  sub_2010 <- prep_msrp_values_pmf(
    sub_df_2010,
    2010,
    FALSE,
    msrp_support_values
  )
  sub_2011 <- prep_msrp_values_pmf(
    sub_df_2011,
    2011,
    FALSE,
    msrp_support_values
  )

  sub <- diftrans(pre_main = sub_2010,
                  post_main = sub_2011,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total,
                  seed = NULL)
  sub_results <- sub$empirical_table$result
  results[, sim] <- sub_results
}

results_df <- as.data.frame(results)
colnames(results_df) <- paste0("sim", seq_len(sims))

save(results_df, file = here::here("scrapnotes/results_unweighted2_byyear.RData"))
save(store_indices, file = here::here("scrapnotes/subsample_unweighted_indices_byyear.RData"))

check_if_matched <- apply(store_indices, 2, function(x) {mean(x[matched$is_matched == 1])})
summary(check_if_matched)
check_if_matched <- apply(store_indices, 2, function(x) {all(x == matched$is_matched)})
summary(as.numeric(check_if_matched))

plot_df <- cbind(bandwidth = bandwidth_vec,
                 results_df,
                 match_sim = match_results,
                 full_sim = full_results) %>%
  tidyr::pivot_longer(cols = contains("sim")) %>%
  mutate(value = 100 * value)

ggplot() +
  geom_line(data = plot_df %>%
            filter(!(name %in% c("full_sim", "match_sim"))),
            aes(x = bandwidth, y = value, color = name), alpha = 0.75) +
  geom_line(data = plot_df %>%
            filter(name %in% c("full_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  geom_line(data = plot_df %>%
            filter(name %in% c("match_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  theme_bw() +
  guides(color = FALSE)

ggsave(filename = "before-and-after_unweighted_MSRP2_byyear.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

### Unweighted w/o outlier -------------------------


tmp <- matched_raw %>%
  filter(year == 2011) %>%
  filter(!is.na(avg_price)) %>%
  select(msrp, sales) %>%
  group_by(msrp) %>%
  summarize(sales = sum(sales)) %>%
  arrange(desc(sales)) #  %>%
  # mutate(prop = sales / (sum(sales) * 10000))
outlier <- as.numeric(tmp[1, "msrp"])

matched <- matched_raw %>%
  mutate(is_matched = !is.na(avg_price)) %>%
  filter(msrp != outlier)

msrp_support_values <- unique(matched$msrp)

prep_msrp_values_pmf <- function(df, years, only_keep_matches, support) {
  if (only_keep_matches) {
    match_values <- c(TRUE)
  } else {
    match_values <- c(FALSE, TRUE)
  }
  df %>%
    filter(year %in% years) %>%
    filter(is_matched %in% match_values) %>%
    select(msrp) %>%
    group_by(msrp) %>%
    count() %>%
    rename(total = n) %>%
    full_join(data.frame(msrp = support), by = "msrp") %>%
    replace_na(list(total = 0)) %>%
    arrange(msrp) %>%
    ungroup()
}

msrp_full_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  FALSE,
  msrp_support_values
)
msrp_full_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  FALSE,
  msrp_support_values
)
msrp_match_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  TRUE,
  msrp_support_values
)
msrp_match_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  TRUE,
  msrp_support_values
)

bandwidth_vec <- seq(0, 40000, 1000)
full <- diftrans(pre_main = msrp_full_2010_values,
                 post_main = msrp_full_2011_values,
                 bandwidth_vec = bandwidth_vec,
                 var = msrp,
                 count = total)
full_results <- full$empirical_table$result

match <- diftrans(pre_main = msrp_match_2010_values,
                  post_main = msrp_match_2011_values,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total)
match_results <- match$empirical_table$result

ggplot() +
  geom_line(aes(x = bandwidth_vec, y = full_results), color = "steelblue") +
  geom_line(aes(x = bandwidth_vec, y = match_results), color = "orange") +
  theme_bw()

sims <- 500
results <- matrix(NA_real_, nrow = length(bandwidth_vec), ncol = sims)
store_indices <- matrix(NA_real_, nrow = nrow(matched), ncol = sims)
for (sim in seq_len(sims)) {
  print(paste("Simulation", sim, "out of", sims))
  sub_index <- sample(seq_len(nrow(matched)), num_matches, replace = FALSE)
  sub_df <- matched[sub_index, ]
  zeros <- rep(0, nrow(matched))
  zeros[sub_index] <- 1
  store_indices[, sim] <- zeros
  if (all.equal(sub_df, matched[matched$is_matched == 1, ]) == TRUE) {
    print("SUB = MATCH!!!")
  }
  sub_2010 <- prep_msrp_values_pmf(
    sub_df,
    2010,
    FALSE,
    msrp_support_values
  )
  sub_2011 <- prep_msrp_values_pmf(
    sub_df,
    2011,
    FALSE,
    msrp_support_values
  )
  sub <- diftrans(pre_main = sub_2010,
                  post_main = sub_2011,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total,
                  seed = NULL)
  sub_results <- sub$empirical_table$result
  results[, sim] <- sub_results
}

results_df <- as.data.frame(results)
colnames(results_df) <- paste0("sim", seq_len(sims))

save(results_df, file = here::here("scrapnotes/results_unweighted2_no-outlier.RData"))
save(store_indices, file = here::here("scrapnotes/subsample_unweighted_indices_no-outlier.RData"))

check_if_matched <- apply(store_indices, 2, function(x) {mean(x[matched$is_matched == 1])})
summary(check_if_matched)
check_if_matched <- apply(store_indices, 2, function(x) {all(x == matched$is_matched)})
summary(as.numeric(check_if_matched))

plot_df <- cbind(bandwidth = bandwidth_vec,
                 results_df,
                 match_sim = match_results,
                 full_sim = full_results) %>%
  tidyr::pivot_longer(cols = contains("sim")) %>%
  mutate(value = 100 * value)

ggplot() +
  geom_line(data = plot_df %>%
            filter(!(name %in% c("full_sim", "match_sim"))),
            aes(x = bandwidth, y = value, color = name), alpha = 0.75) +
  geom_line(data = plot_df %>%
            filter(name %in% c("full_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  geom_line(data = plot_df %>%
            filter(name %in% c("match_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  theme_bw() +
  guides(color = FALSE)

ggsave(filename = "before-and-after_unweighted_MSRP2_no-outlier.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

### Compare match vs. subsample -------------------------

msrp_support_values <- unique(matched$msrp)

prep_msrp_values_pmf <- function(df, years, only_keep_matches, support) {
  if (only_keep_matches) {
    match_values <- c(TRUE)
  } else {
    match_values <- c(FALSE, TRUE)
  }
  df %>%
    filter(year %in% years) %>%
    filter(is_matched %in% match_values) %>%
    select(msrp) %>%
    group_by(msrp) %>%
    count() %>%
    rename(total = n) %>%
    full_join(data.frame(msrp = support), by = "msrp") %>%
    replace_na(list(total = 0)) %>%
    arrange(msrp) %>%
    ungroup()
}

msrp_full_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  FALSE,
  msrp_support_values
)
msrp_full_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  FALSE,
  msrp_support_values
)
msrp_match_2010_values <- prep_msrp_values_pmf(
  matched,
  2010,
  TRUE,
  msrp_support_values
)
msrp_match_2011_values <- prep_msrp_values_pmf(
  matched,
  2011,
  TRUE,
  msrp_support_values
)

bandwidth_vec <- seq(0, 40000, 1000)
compare_2010 <- diftrans(pre_main = msrp_full_2010_values,
                 post_main = msrp_match_2010_values,
                 bandwidth_vec = bandwidth_vec,
                 var = msrp,
                 count = total)
compare_2010_results <- compare_2010$empirical_table$result

compare_2011 <- diftrans(pre_main = msrp_full_2011_values,
                  post_main = msrp_match_2011_values,
                  bandwidth_vec = bandwidth_vec,
                  var = msrp,
                  count = total)
compare_2011_results <- compare_2011$empirical_table$result

ggplot() +
  geom_line(aes(x = bandwidth_vec, y = compare_2010_results), color = "steelblue") +
  geom_line(aes(x = bandwidth_vec, y = compare_2011_results), color = "orange") +
  theme_bw()
