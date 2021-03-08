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

sims <- 500
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

# exploration

pre_total
post_total
sum(pre_Beijing$sales)
sum(post_Beijing$sales)

tp_30000 <- plot_main(ba_transaction_price, binwidth = 30000) +
  ggtitle("Trans. Price, Binwidth = 30000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
tp_50000 <- plot_main(ba_transaction_price, binwidth = 50000) +
  ggtitle("Trans. Price, Binwidth = 50000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
tp_70000 <- plot_main(ba_transaction_price, binwidth = 70000) +
  ggtitle("Trans. Price, Binwidth = 70000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
tp_90000 <- plot_main(ba_transaction_price, binwidth = 90000) +
  ggtitle("Trans. Price, Binwidth = 90000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)

msrp_30000 <- plot_main(ba, binwidth = 30000) +
  ggtitle("MSRP, Binwidth = 30000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
msrp_50000 <- plot_main(ba, binwidth = 50000) +
  ggtitle("MSRP, Binwidth = 50000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
msrp_70000 <- plot_main(ba, binwidth = 70000) +
  ggtitle("MSRP, Binwidth = 70000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)
msrp_90000 <- plot_main(ba, binwidth = 90000) +
  ggtitle("MSRP, Binwidth = 90000") +
  scale_y_continuous(limits = c(0, 7.1e-6), breaks = seq(0, 7.1e-6, 5e-7)) +
  guides(color = FALSE, fill = FALSE)

library(gridExtra)

tp_hist <- arrangeGrob(tp_30000, tp_50000, tp_70000, tp_90000,
                       ncol = 2)

ggsave(filename = "tp_hist.jpg",
       plot = tp_hist,
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 9,
       height = 9)

msrp_hist <- arrangeGrob(msrp_30000, msrp_50000, msrp_70000, msrp_90000,
                         ncol = 2)

ggsave(filename = "msrp_hist.jpg",
       plot = msrp_hist,
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 9,
       height = 9)

matched_raw <- data.table::fread("~/BFI/3_BMP_GP/data-raw/Beijing_cleaned_merged.csv")

matched <- matched_raw %>%
  filter(!is.na(avg_price)) %>%
  select(year, msrp, sales)
matched_msrp <- matched %>%
  group_by(year, msrp) %>%
  summarize(sales = sum(sales)) %>%
  ungroup()
matched_pre_raw <- matched_msrp %>% filter(year == 2010) %>% select(-year)
matched_post_raw <- matched_msrp %>% filter(year == 2011) %>% select(-year)
merged <- full_join(matched_pre_raw, matched_post_raw, by = "msrp") %>%
  tidyr::replace_na(list(sales.x = 0, sales.y = 0))

matched_msrp_pre <- merged %>%
  select(msrp, sales.x) %>%
  rename(sales = sales.x) %>%
  arrange(msrp)
matched_msrp_post <- merged %>%
  select(msrp, sales.y) %>%
  rename(sales = sales.y) %>%
  arrange(msrp)

ba_msrp <- diftrans(pre_main = matched_msrp_pre,
                    post_main = matched_msrp_post,
                    var = msrp,
                    count = sales,
                    sims_bandwidth_selection = 500,
                    seed = 80,
                    show_progress = TRUE)

save(ba_msrp, file = here::here("scrapnotes/ba_msrp.RData"))

load(here::here("scrapnotes/ba_msrp.RData"))
ba_msrp_df <- ba_msrp$empirical_table %>% select(bandwidth, result)

load(here::here("scrapnotes/ba_transaction_price.RData"))
ba_tp_df <- ba_transaction_price$empirical_table %>% select(bandwidth, result)

plot_df <- merge(ba_msrp_df, ba_tp_df, by = "bandwidth") %>%
  rename(msrp = result.x, tp = result.y) %>%
  mutate(diff = abs(msrp - tp)) %>%
  tidyr::pivot_longer(cols = c(msrp, tp, diff))

ggplot(plot_df) +
  geom_line(aes(x = bandwidth, y = 100 * value, color = name)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 90, 5)) +
  scale_x_continuous(breaks = seq(0, 40000, 5000)) +
  scale_color_discrete(name = "",
                     labels = c("absolute difference", "msrp", "transaction price")) +
  xlab("Bandwidth") +
  ylab("Transport Cost (%)")

ggsave(filename = "msrp_vs_transactionprice.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

plot_df <- merge(ba_msrp_df, ba_tp_df, by = "bandwidth") %>%
  rename(msrp = result.x, tp = result.y) %>%
  mutate(ratio = msrp / tp) %>%
  tidyr::pivot_longer(cols = c(msrp, tp, ratio))

ggplot(plot_df) +
  geom_line(aes(x = bandwidth, y = value, color = name)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1.1, 0.2)) +
  scale_x_continuous(breaks = seq(0, 40000, 5000)) +
  scale_color_discrete(name = "",
                     labels = c("MSRP", "Ratio", "Transaction Price")) +
  xlab("Bandwidth") +
  ylab("Transport Cost (%)")

ggsave(filename = "msrp_vs_transactionprice_ratio.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)

# Compare matched and sampled distributions

head(matched_msrp_pre)
head(matched_msrp_post)
head(pre_sample)
head(post_sample)

pre_sample_df <- data.frame(msrp = support, sales = pre_sample)
post_sample_df <- data.frame(msrp = support, sales = post_sample)

# matched_msrp <- full_join(matched_msrp_pre, matched_msrp_post, by = "msrp") %>%
#   rename(matched_pre = sales.x,
#          matched_post = sales.y)
# sample_msrp <- data.frame(msrp = support,
#                           samples_pre = pre_sample,
#                           samples_post = post_sample)
# plot_df <- full_join(matched_msrp, sample_msrp, by = "msrp") %>%
#   tidyr::pivot_longer(cols = c(contains("pre"), contains("post")))

matched_msrp_pre_dist <- tidyr::uncount(matched_msrp_pre, sales)
matched_msrp_post_dist <- tidyr::uncount(matched_msrp_post, sales)
pre_sample_dist <- tidyr::uncount(pre_sample_df, sales)
post_sample_dist <- tidyr::uncount(post_sample_df, sales)

ggplot() +
  geom_histogram(aes(x = matched_msrp_pre_dist$msrp,
                     y = ..density..),
                 alpha = 0.5,
                 fill = "orange", color = "white", binwidth = 20000) +
  geom_histogram(aes(x = pre_sample_dist$msrp,
                     y = ..density..),
                 alpha = 0.5,
                 fill = "steelblue", color = "white", binwidth = 20000) +
  ggtitle("Matched data (orange) vs Sampled MSRP data (blue) -- 2010") +
  xlab("MSRP") +
  theme_bw()

ggsave("matched_vs_sample_2010.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)


ggplot() +
  geom_histogram(aes(x = matched_msrp_post_dist$msrp,
                     y = ..density..),
                 alpha = 0.5,
                 fill = "orange", color = "white", binwidth = 20000) +
  geom_histogram(aes(x = post_sample_dist$msrp,
                     y = ..density..),
                 alpha = 0.5,
                 fill = "steelblue", color = "white", binwidth = 20000) +
  ggtitle("Matched data (orange) vs Sampled MSRP data (blue) -- 2011") +
  xlab("MSRP") +
  theme_bw()

ggsave("matched_vs_sample_2011.jpg",
       path = "~/BFI/3_BMP_GP/img/img_misc/price_msrp_discrepancy",
       width = 7,
       height = 4)


head(matched)
matched %>%
  group_by(year) %>%
  summarize(mean = mean(msrp),
            sd = sd(msrp),
            q95 = quantile(msrp, 0.95))

# compare_2010_2011 <- matched_raw %>%
#   filter(!is.na(avg_price)) %>%
#   select(msrp, swtprice, color, noticenum, avg_price)

# Compare the data I sent with the data they sent me

# head(matched_raw)
# head(Beijing_cleaned)
Beijing_cleaned_2 <- Beijing_cleaned %>%
  mutate(ym = as.character(ym)) %>%
  distinct()
matched_2 <- matched_raw %>%
  arrange(v1) %>%
  select(-v1, -avg_price) %>%
  as_tibble() %>%
  rename(MSRP = msrp, postBeijing = postbeijing, postTianjin = posttianjin, Beijing = beijing, Tianjin = tianjin, Shijiazhuang = shijiazhuang)
dim(Beijing_cleaned_2)
dim(matched_2)
head(Beijing_cleaned_2)
head(matched_2)
all.equal(Beijing_cleaned_2, matched_2)
identical(Beijing_cleaned_2$id, matched_2$id)
identical(Beijing_cleaned_2$year, matched_2$year)
identical(Beijing_cleaned_2$city, matched_2$city)
identical(Beijing_cleaned_2$MSRP, matched_2$MSRP)
identical(Beijing_cleaned_2$sales, matched_2$sales)
all.equal(Beijing_cleaned_2$swtprice, matched_2$swtprice)
Beijing_cleaned_2[Beijing_cleaned_2$swtprice == matched_2$swtprice, ]
identical(Beijing_cleaned_2$color, matched_2$color)
identical(Beijing_cleaned_2$noticenum, matched_2$noticenum)
