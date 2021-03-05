### Meta ---------------------------
###
### Title: price_msrp_discrepancy.R
###
### Description: Explore the discrepancy in transaction price and MSRP
###
### Author: Omkar A. Katta
###

### Preliminaries ---------------------------
devtools::load_all()
library(dplyr)

Beijing_raw <- data.table::fread("~/BFI/3_BMP_GP/data-raw/Beijing_cleaned_merged.csv")
head(Beijing_raw)

Beijing <- Beijing_raw %>%
  filter(!is.na(avg_price)) %>%
  group_by(avg_price, year) %>%
  summarize(sales = sum(sales))

pre_Beijing <- Beijing %>%
  filter(year == 2010) %>%
  select(avg_price, sales)
post_Beijing <- Beijing %>%
  filter(year == 2011) %>%
  select(avg_price, sales)

merged <- full_join(pre_Beijing, post_Beijing, by = "avg_price") %>%
  tidyr::replace_na(list(sales.x = 0, sales.y = 0))

pre_Beijing <- merged %>%
  select(avg_price, sales.x) %>%
  rename(sales = sales.x) %>%
  arrange(avg_price)
post_Beijing <- merged %>%
  select(avg_price, sales.y) %>%
  rename(sales = sales.y) %>%
  arrange(avg_price)

ba_transaction_price <- diftrans(pre_main = pre_Beijing,
                                 post_main = post_Beijing,
                                 var = avg_price,
                                 count = sales,
                                 sims_bandwidth_selection = 500,
                                 seed = 5,
                                 show_progress = TRUE)

save(ba_transaction_price, file = here::here("scrapnotes/ba_transaction_price.RData"))

######

load("scrapnotes/ba_transaction_price.RData")
print(ba_transaction_price)
summary(ba_transaction_price)
plot_main(ba_transaction_price, binwidth = 20000)
plot_empirical(ba_transaction_price)
ba_transaction_price$empirical_table

# get_OTcost(pre_Beijing,
#            post_Beijing,
#            bandwidth = 4000, var = avg_price, count = sales)
# pre <- pre_Beijing$sales
# post <- post_Beijing$sales
# support <- check_support(pre_Beijing, post_Beijing, var = avg_price)$support
# bandwidth <- 5000
# support_pre <- pre_Beijing %>%
#   filter(sales != 0) %>%
#   select(avg_price) %>%
#   distinct(avg_price) %>%
#   arrange(avg_price) %>%
#   filter(!is.na(avg_price)) %>%
#   unlist()
# support_post <- post_Beijing %>%
#   filter(sales != 0) %>%
#   select(avg_price) %>%
#   distinct(avg_price) %>%
#   arrange(avg_price) %>%
#   filter(!is.na(avg_price)) %>%
#   unlist()
# costm_ref <- build_costmatrix2(support_pre, support_post, bandwidth)
#
# tmp <- costm_ref
# tmp <- build_costmatrix(support, bandwidth = 23000)
# # tmp <- build_costmatrix(seq_len(100), bandwidth = 2)
# tmp_melt <- reshape2::melt(tmp)
#
# library(ggplot2)
# ggplot(data = tmp_melt, aes(x=Var1, y=Var2, fill=as.character(value))) +
#   geom_tile()
#

load("scrapnotes/ba.RData")
summary(ba)

result1 <- ba_transaction_price$empirical_table %>%
  select(bandwidth, result)
result2 <- ba$empirical_table %>%
  select(bandwidth, result)
result <- dplyr::inner_join(result1, result2, by = "bandwidth") %>%
  rename(transaction_price = result.x) %>%
  rename(msrp = result.y)
result_long <- result %>%
  tidyr::pivot_longer(cols = c(transaction_price, msrp))

ggplot(result_long) +
  geom_vline(xintercept = ba_transaction_price$optimal_bandwidth) +
  geom_vline(xintercept = ba$optimal_bandwidth) +
  theme_bw() +
  geom_line(aes(x = bandwidth, y = value * 100, color = name)) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 5000)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth")

ggsave(filename = "before-and-after.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

### Smaller-sized MSRP -------------------------

devtools::load_all()
library(dplyr)
library(ggplot2)

# prepare MSRP data

load(here::here(".hidden", "Beijing_cleaned.RData"))

pre_Beijing_raw <- Beijing_cleaned %>%
  filter(year == 2010) %>%
  group_by(MSRP) %>%
  summarize(sales = sum(sales))
post_Beijing_raw <- Beijing_cleaned %>%
  filter(year == 2011) %>%
  group_by(MSRP) %>%
  summarize(sales = sum(sales))
merged <- full_join(pre_Beijing_raw, post_Beijing_raw, by = "MSRP") %>%
  rename(pre_sales = sales.x,
         post_sales = sales.y) %>%
  tidyr::replace_na(list(pre_sales = 0, post_sales = 0))
pre_Beijing <- merged %>%
  select(MSRP, pre_sales) %>%
  rename(sales = pre_sales) %>%
  arrange(MSRP)
post_Beijing <- merged %>%
  select(MSRP, post_sales) %>%
  rename(sales = post_sales) %>%
  arrange(MSRP)

# prepare transaction price data to get totals

Beijing_raw <- data.table::fread("~/BFI/3_BMP_GP/data-raw/Beijing_cleaned_merged.csv")
head(Beijing_raw)

Beijing <- Beijing_raw %>%
  filter(!is.na(avg_price)) %>%
  group_by(year) %>%
  summarize(sales = sum(sales))
pre_total <- as.numeric(Beijing[Beijing$year == 2010, "sales"])
post_total <- as.numeric(Beijing[Beijing$year == 2011, "sales"])

# create samples from MSRP data that is of same size as transaction price data

sims <- 100
set.seed(50)
pre_Beijing_dist <- unlist(tidyr::uncount(pre_Beijing, sales))
post_Beijing_dist <- unlist(tidyr::uncount(post_Beijing, sales))
support <- pre_Beijing$MSRP
bandwidth_vec <- seq(0, 40000, 1000)
results <- matrix(NA_real_, nrow = length(bandwidth_vec), ncol = sims)
pre_sample_storage <- matrix(NA_real_, nrow = length(support), ncol = sims)
post_sample_storage <- matrix(NA_real_, nrow = length(support), ncol = sims)

for (i in seq_len(sims)) {
  print(paste("Simulation", i, "out of", sims))
  pre_sample <- organize_subsamples(pre_Beijing_dist, pre_total, support)
  post_sample <- organize_subsamples(post_Beijing_dist, post_total, support)
  pre_sample_storage[, i] <- pre_sample
  post_sample_storage[, i] <- post_sample
  stopifnot(sum(pre_sample) == pre_total)
  stopifnot(sum(post_sample) == post_total)
  pre_sample_df <- data.frame(MSRP = support,
                              sales = pre_sample)
  post_sample_df <- data.frame(MSRP = support,
                               sales = post_sample)
  ba_sample <- diftrans(pre_sample_df,
                        post_sample_df,
                        var = MSRP,
                        count = sales,
                        bandwidth_vec = bandwidth_vec,
                        seed = NULL)
  results[, i] <- ba_sample$empirical_table$result
}

# combine MSRP sampled results and transaction price results and actual result

results_df <- as.data.frame(results)
colnames(results_df) <- paste0("sim", seq_len(sims))

load(here::here("scrapnotes/ba.RData"))
msrp_results <- ba$empirical_table$result

load(here::here("scrapnotes/ba_transaction_price.RData"))
tp_results <- ba_transaction_price$empirical_table$result

plot_df <- cbind(bandwidth = bandwidth_vec,
                 results_df,
                 msrp_sim = msrp_results,
                 tp_sim = tp_results) %>%
  tidyr::pivot_longer(cols = contains("sim")) %>%
  mutate(value = 100 * value)

ggplot() +
  geom_line(data = plot_df %>%
            filter(!(name %in% c("msrp_sim", "tp_sim"))),
            aes(x = bandwidth, y = value, color = name), alpha = 0.75) +
  geom_line(data = plot_df %>%
            filter(name %in% c("msrp_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  geom_line(data = plot_df %>%
            filter(name %in% c("tp_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  theme_bw() +
  guides(color = FALSE)

ggsave(filename = "before-and-after-correct-size-without-replacement.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

ggplot() +
  geom_line(data = plot_df %>%
            filter(!(name %in% c("msrp_sim", "tp_sim"))),
            aes(x = bandwidth, y = value, color = name), alpha = 0.75, size = 0.5) +
  geom_line(data = plot_df %>%
            filter(name %in% c("msrp_sim")),
            aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  # geom_line(data = plot_df %>%
  #           filter(name %in% c("tp_sim")),
  #           aes(x = bandwidth, y = value), color = "black", alpha = 1) +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 2500)) +
  xlab("Transport Cost (%)") +
  ylab("Bandwidth") +
  theme_bw() +
  guides(color = FALSE)

ggsave(filename = "before-and-after-correct-size-without-replacement-onlyMSRP.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)
