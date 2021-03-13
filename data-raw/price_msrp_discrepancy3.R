### Meta -------------------------
###
### Title: price_msrp_discrepancy3.R
###
### Description: Study discrepancy in MSRP and transaction price
###
### Author: Omkar A. Katta
###

### Preliminaries -------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
devtools::load_all()

matching_raw_path <- "~/BFI/3_BMP_GP/data-raw/Beijing_cleaned_merged.csv"
matching_raw <- data.table::fread(matching_raw_path)

matching <- matching_raw %>%
  mutate(is_matched = as.numeric(!is.na(avg_price)))
unique(matching$is_matched)
matched <- matching %>%
  filter(is_matched == 1)
unmatched <- matching %>%
  filter(is_matched == 0)

msrp_support <- unique(matching$msrp)

prep_unweighted <- function(df,
                            years,
                            support,
                            matching = c(1, 0),
                            cols = everything()) {
  df %>%
    filter(year %in% years) %>%
    filter(is_matched %in% matching) %>%
    select({{cols}})
}

full_2010 <- prep_unweighted(
  matching,
  2010,
  msrp_support,
  cols = c(msrp, sales, avg_price, is_matched)
)
full_2011 <- prep_unweighted(
  matching,
  2011,
  msrp_support,
  cols = c(msrp, sales, avg_price, is_matched)
)
match_2010 <- prep_unweighted(
  matching,
  2010,
  msrp_support,
  matching = c(1),
  cols = c(msrp, sales, avg_price, is_matched)
)
match_2011 <- prep_unweighted(
  matching,
  2011,
  msrp_support,
  matching = c(1),
  cols = c(msrp, sales, avg_price, is_matched)
)

bandwidth_vec <- seq(0, 40000, 1000)

### Unweighted Subsample vs. Full -------------------------

# Prepare PMFs of unweighted MSRPs
prep_pmf_unweighted <- function(df, support, var = msrp) {
  df %>%
    group_by({{var}}) %>%
    count() %>%
    rename(total = n) %>%
    rename(msrp = {{var}}) %>%
    full_join(data.frame(msrp = support), by = "msrp") %>%
    replace_na(list(total = 0)) %>%
    arrange(msrp) %>%
    ungroup()
}

full_2010_pmf <- prep_pmf_unweighted(full_2010, msrp_support)
full_2011_pmf <- prep_pmf_unweighted(full_2011, msrp_support)
match_2010_pmf <- prep_pmf_unweighted(match_2010, msrp_support)
match_2011_pmf <- prep_pmf_unweighted(match_2011, msrp_support)

# Get indices to create subsample
get_sub_indices <- function(from, size, sims = 500) {
  sub_indices <- sapply(
    seq_len(sims),
    function(sim) {
      selected_rows <- sample(from, size, replace = FALSE)
      zeros <- rep(0, from)
      zeros[selected_rows] <- 1
      zeros
    }
  )
  stopifnot(nrow(sub_indices) == from)
  stopifnot(ncol(sub_indices) == sims)
  return(sub_indices)
}

sub_indices_2010 <- get_sub_indices(nrow(full_2010), nrow(match_2010))
sub_indices_2011 <- get_sub_indices(nrow(full_2011), nrow(match_2011))

# Summarize indices: Of the true matches, how many were in sample?

summary_indices <- function(full, sub_indices) {
  summary(
    apply(
      sub_indices,
      2,
      function(x) {
        mean(x[full$is_matched == 1])
      }
    )
  )
}
summary_indices(full_2010, sub_indices_2010)
summary_indices(full_2011, sub_indices_2011)
nrow(match_2010) / nrow(full_2010)
nrow(match_2011) / nrow(full_2011)

# Compute OT between subsample and full data
# Compare with OT of match and full data

sub_vs_full <- function(full, sub_indices, sim) {
  sub_pmf <- prep_pmf_unweighted(
    full[sub_indices[, sim] == 1, ],
    msrp_support
  )
  full_pmf <- prep_pmf_unweighted(
    full,
    msrp_support
  )
  raw <- diftrans(
    sub_pmf, full_pmf,
    var = msrp, count = total,
    bandwidth_vec = bandwidth_vec
  )
  raw$empirical_table$result
}

sims <- 500
sub_vs_full_2010 <- matrix(NA_real_, nrow = length(bandwidth_vec), ncol = sims)
sub_vs_full_2011 <- matrix(NA_real_, nrow = length(bandwidth_vec), ncol = sims)
for (sim in seq_len(sims)) {
  print(paste("Simulation", sim, "out of", sims))
  sub_vs_full_2010[, sim] <- sub_vs_full(full_2010, sub_indices_2010, sim)
  sub_vs_full_2011[, sim] <- sub_vs_full(full_2011, sub_indices_2011, sim)
}

# Clean simulation results
sub_vs_full_2010 <- as.data.frame(sub_vs_full_2010)
colnames(sub_vs_full_2010) <- paste0("sim", seq_len(sims))
sub_vs_full_2011 <- as.data.frame(sub_vs_full_2011)
colnames(sub_vs_full_2011) <- paste0("sim", seq_len(sims))

# match vs full
match_vs_full_2010 <- diftrans(
  match_2010_pmf, full_2010_pmf,
  var = msrp, count = total,
  bandwidth_vec = bandwidth_vec
)$empirical_table$result
match_vs_full_2011 <- diftrans(
  match_2011_pmf, full_2011_pmf,
  var = msrp, count = total,
  bandwidth_vec = bandwidth_vec
)$empirical_table$result

sub_vs_full_and_match_vs_full_2010 <- cbind(
  bandwidth = bandwidth_vec,
  sub_vs_full_2010,
  sim_2010 = match_vs_full_2010
) %>%
  as.data.frame() %>%
  pivot_longer(cols = contains("sim")) %>%
  mutate(value = 100 * value)
sub_vs_full_and_match_vs_full_2011 <- cbind(
  bandwidth = bandwidth_vec,
  sub_vs_full_2011,
  sim_2011 = match_vs_full_2011
) %>%
  as.data.frame() %>%
  pivot_longer(cols = contains("sim")) %>%
  mutate(value = 100 * value)

ggplot() +
  geom_line(data = sub_vs_full_and_match_vs_full_2010 %>%
    filter(!(name %in% c("sim_2010"))),
    aes(x = bandwidth, y = value, color = name), alpha = 0.75
  ) +
  geom_line(data = sub_vs_full_and_match_vs_full_2010 %>%
    filter(name %in% c("sim_2010")),
    aes(x = bandwidth, y = value), color = "black", alpha = 1
  ) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  ggtitle("sub vs full & match vs full -- 2010") +
  theme_bw() +
  guides(color = FALSE)

ggplot() +
  geom_line(data = sub_vs_full_and_match_vs_full_2011 %>%
    filter(!(name %in% c("sim_2011"))),
    aes(x = bandwidth, y = value, color = name), alpha = 0.75
  ) +
  geom_line(data = sub_vs_full_and_match_vs_full_2011 %>%
    filter(name %in% c("sim_2011")),
    aes(x = bandwidth, y = value), color = "black", alpha = 1
  ) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  ggtitle("sub vs full & match vs full -- 2011") +
  theme_bw() +
  guides(color = FALSE)

### Mapping -------------------------
