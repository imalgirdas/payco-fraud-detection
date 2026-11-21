# -----------------------------------------------------------------------------
# Script:   04_explore.R
#
# Purpose:  Exploration aimed at modelling: target rates and imbalance, yearly
#           drift, how single features separate each target (univariate AUC,
#           rate by bucket), population stability between train and test, and
#           the payment-failure story (methods, reasons, outage).
#
# Input:    Output/03_features.rds, 00_theme.R
# Output:   Output/04_explore_results.rds, Output/04_charts.rds,
#           Output/charts/04_*.png
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "lubridate", "ggplot2", "scales", "forcats", "ragg")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(forcats)
})

theme_file <- if (file.exists("R/00_theme.R")) "R/00_theme.R" else "00_theme.R"
source(theme_file)

output_dir <- "Output"
f <- readRDS(file.path(output_dir, "03_features.rds"))
fraud <- f$fraud; failure <- f$failure; churn <- f$churn

charts <- list()
keep_chart <- function(p, name, width = 10, height = 6) {
  charts[[name]] <<- p
  save_chart(p, paste0("04_", name), width = width, height = height)
}

# Fast AUC (Mann-Whitney) for a single numeric feature --------------------------
auc_fast <- function(score, y) {
  ok <- !is.na(score)
  score <- score[ok]; y <- y[ok]
  r <- rank(score)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * n0)
}

# Population stability index: train (2021-2024) vs test (2025) ----------------
psi <- function(train, test, bins = 10) {
  br <- unique(quantile(train, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
  if (length(br) < 3) return(NA_real_)
  br[1] <- -Inf; br[length(br)] <- Inf
  p <- as.numeric(table(cut(train, br))) / length(train)
  q <- as.numeric(table(cut(test, br))) / length(test)
  p <- pmax(p, 1e-4); q <- pmax(q, 1e-4)
  sum((q - p) * log(q / p))
}

# 1. Targets: size and imbalance ----------------------------------------------
target_summary <- bind_rows(
  fraud   %>% group_by(year) %>% summarise(rows = n(), positives = sum(is_fraud)) %>% mutate(target = "Fraud (paid transactions)"),
  failure %>% group_by(year) %>% summarise(rows = n(), positives = sum(is_failed)) %>% mutate(target = "Payment failure (attempts)"),
  churn   %>% mutate(year = year(snapshot)) %>% group_by(year) %>%
    summarise(rows = n(), positives = sum(is_churned)) %>% mutate(target = "Buyer churn (snapshots)")
) %>%
  mutate(rate = positives / rows, negatives_per_positive = (rows - positives) / positives) %>%
  relocate(target)

p <- target_summary %>%
  filter(year >= 2021) %>%
  ggplot(aes(factor(year), rate)) +
  geom_col(fill = pay$teal, width = 0.65) +
  geom_text(aes(label = percent(rate, accuracy = 0.1)), vjust = -0.5, size = 3.3) +
  facet_wrap(~ target, scales = "free_y") +
  scale_y_continuous(labels = fmt_pct1, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Three targets, three very different class balances",
       subtitle = "Positive rate by year. Fraud is a rare event, churn is close to balanced",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "target_rates", width = 12, height = 5)

# 2. Fraud drift over time ----------------------------------------------------
fraud_month <- fraud %>%
  group_by(month) %>%
  summarise(fraud_rate = mean(is_fraud), txns = n(), mature = all(label_mature), .groups = "drop")

p <- ggplot(fraud_month, aes(as.Date(month), fraud_rate)) +
  annotate("rect", xmin = as.Date("2022-07-01"), xmax = as.Date("2023-01-15"), ymin = -Inf, ymax = Inf,
           fill = pay$alert, alpha = 0.08) +
  annotate("rect", xmin = as.Date("2025-10-01"), xmax = as.Date("2025-12-31"), ymin = -Inf, ymax = Inf,
           fill = pay$grey, alpha = 0.15) +
  annotate("text", x = as.Date("2022-10-01"), y = max(fraud_month$fraud_rate) * 1.02,
           label = "Account takeover wave", size = 3.2, colour = pay$alert) +
  annotate("text", x = as.Date("2025-11-15"), y = max(fraud_month$fraud_rate) * 0.75,
           label = "Labels not\nmature yet", size = 3, colour = pay$slate) +
  geom_line(colour = pay$teal) +
  geom_point(size = 1.4, colour = pay$teal) +
  scale_y_continuous(labels = fmt_pct1, limits = c(0, NA)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  labs(title = "Fraud rate is not stable: the model must survive drift",
       subtitle = "Share of paid transactions later confirmed as fraud, by month",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "fraud_month", width = 12, height = 5.5)

fraud_type_year <- fraud %>%
  filter(is_fraud == 1) %>%
  count(year, payment_method) %>%
  group_by(year) %>% mutate(share = n / sum(n)) %>% ungroup()

# 3. Fraud: single-feature separation -----------------------------------------
fraud_num <- c("log_amount_eur", "account_age_days", "device_age_days", "device_other_accounts",
               "session_minutes", "items_viewed_session", "hour", "buyer_prior_txns", "buyer_txns_24h",
               "seller_age_days", "seller_prior_sales", "card_added_days_ago", "risk_score",
               "n_attempts", "n_failed_before_success", "ip_mismatch", "issuer_mismatch", "new_device",
               "is_emulator", "shipping_address_changed_7d", "card_new", "night", "phone_verified")
fraud_auc <- tibble(feature = fraud_num) %>%
  mutate(auc = map_dbl(feature, ~ auc_fast(fraud[[.x]], fraud$is_fraud)),
         strength = abs(auc - 0.5) * 2,
         direction = if_else(auc >= 0.5, "higher = more fraud", "lower = more fraud")) %>%
  arrange(desc(strength))

p <- fraud_auc %>%
  slice_head(n = 15) %>%
  mutate(feature = fct_reorder(feature, strength)) %>%
  ggplot(aes(strength, feature, fill = direction)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pay_pair) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Session behaviour and device age carry the most fraud signal on their own",
       subtitle = "Single-feature separation (Gini = 2 x |AUC - 0.5|), top 15 features",
       x = "Gini", y = NULL, caption = chart_caption)
keep_chart(p, "fraud_feature_strength", width = 10, height = 6.5)

fraud_buckets <- bind_rows(
  fraud %>% mutate(bucket = cut(account_age_days, c(-Inf, 7, 30, 180, 365, Inf),
                                labels = c("< 7 days", "7-30", "30-180", "180-365", "1 year +"))) %>%
    group_by(bucket) %>% summarise(rate = mean(is_fraud), n = n()) %>% mutate(feature = "Account age"),
  fraud %>% mutate(bucket = if_else(device_other_accounts > 0, "Shared with other accounts", "Own device")) %>%
    group_by(bucket) %>% summarise(rate = mean(is_fraud), n = n()) %>% mutate(feature = "Device"),
  fraud %>% mutate(bucket = if_else(ip_mismatch == 1, "IP abroad", "IP home country")) %>%
    group_by(bucket) %>% summarise(rate = mean(is_fraud), n = n()) %>% mutate(feature = "IP country"),
  fraud %>% mutate(bucket = if_else(new_device == 1, "First seen today", "Known device")) %>%
    group_by(bucket) %>% summarise(rate = mean(is_fraud), n = n()) %>% mutate(feature = "Device age")
) %>% mutate(bucket = as.character(bucket))

p <- fraud_buckets %>%
  mutate(bucket = fct_inorder(bucket)) %>%
  ggplot(aes(rate, bucket)) +
  geom_col(fill = pay$teal, width = 0.65) +
  geom_text(aes(label = percent(rate, accuracy = 0.1)), hjust = -0.15, size = 3.2) +
  facet_wrap(~ feature, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Fraud concentrates in new accounts, shared and new devices",
       subtitle = "Fraud rate by bucket, all paid transactions 2021-2025",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "fraud_buckets", width = 11, height = 6)

method_fraud <- fraud %>%
  group_by(year, payment_method) %>%
  summarise(rate = mean(is_fraud), n = n(), .groups = "drop")

p <- ggplot(method_fraud, aes(factor(year), payment_method, fill = rate)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = percent(rate, accuracy = 0.1),
                colour = rate > max(rate) * 0.6), size = 3.3) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = pay$navy), guide = "none") +
  scale_fill_payco_seq(labels = fmt_pct1) +
  labs(title = "Where fraud hides moves from cards to BNPL",
       subtitle = "Fraud rate by payment method and year",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(legend.position = "right", panel.grid = element_blank())
keep_chart(p, "fraud_method_heatmap", width = 10, height = 5)

# 4. Drift: train vs test population ------------------------------------------
psi_tbl <- tibble(feature = c("log_amount_eur", "account_age_days", "session_minutes", "items_viewed_session",
                              "buyer_prior_txns", "risk_score", "card_added_days_ago", "seller_age_days")) %>%
  mutate(psi = map_dbl(feature, ~ psi(fraud[[.x]][fraud$year <= 2024], fraud[[.x]][fraud$year == 2025])),
         verdict = case_when(psi < 0.1 ~ "stable", psi < 0.25 ~ "moderate shift", TRUE ~ "large shift")) %>%
  arrange(desc(psi))

# 5. Payment failure ----------------------------------------------------------
decline_method_year <- failure %>%
  group_by(year, payment_method) %>%
  summarise(fail_rate = mean(is_failed), attempts = n(), .groups = "drop") %>%
  filter(year <= 2025)

p <- ggplot(decline_method_year, aes(year, fail_rate, colour = payment_method)) +
  geom_line() + geom_point(size = 2) +
  scale_colour_payco() +
  scale_y_continuous(labels = fmt_pct, limits = c(0, NA)) +
  labs(title = "Cards and BNPL fail most often; balance payments almost never",
       subtitle = "Share of attempts not approved, by method and year",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "fail_method_year", width = 10, height = 5.5)

decline_reasons <- failure %>%
  filter(is_failed == 1, year <= 2025) %>%
  count(year, decline_code) %>%
  group_by(year) %>% mutate(share = n / sum(n)) %>% ungroup()
top_reasons <- decline_reasons %>% group_by(decline_code) %>% summarise(n = sum(n)) %>%
  slice_max(n, n = 6) %>% pull(decline_code)

reason_cols <- c(setNames(pay_categorical, top_reasons), other = pay$grey_light)
p <- decline_reasons %>%
  mutate(decline_code = if_else(decline_code %in% top_reasons, decline_code, "other"),
         decline_code = factor(decline_code, levels = c("other", rev(top_reasons)))) %>%
  group_by(year, decline_code) %>% summarise(share = sum(share), .groups = "drop") %>%
  ggplot(aes(factor(year), share, fill = decline_code)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = reason_cols, breaks = c(top_reasons, "other")) +
  scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Insufficient funds is the biggest decline reason every year",
       subtitle = "Mix of decline reasons among failed attempts",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(legend.position = "right")
keep_chart(p, "decline_reasons", width = 11, height = 5.5)

outage <- failure %>%
  filter(attempted_at >= as.POSIXct("2023-03-01", tz = "UTC"), attempted_at < as.POSIXct("2023-04-01", tz = "UTC")) %>%
  mutate(day = as.Date(attempted_at)) %>%
  group_by(day, psp) %>%
  summarise(error_rate = mean(status == "error"), attempts = n(), .groups = "drop") %>%
  filter(psp %in% c("psp_alpha", "psp_beta"))

p <- ggplot(outage, aes(day, error_rate, colour = psp)) +
  geom_line() + geom_point(size = 1.6) +
  scale_colour_manual(values = pay_pair) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(title = "psp_beta outage, March 2023",
       subtitle = "Daily technical error rate by PSP. Recent PSP error rate is a feature in the failure model",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "psp_outage", width = 10, height = 5)

dom <- failure %>%
  filter(payment_method == "card") %>%
  group_by(day_of_month) %>%
  summarise(fail_rate = mean(decline_code %in% "insufficient_funds"), .groups = "drop")

p <- ggplot(dom, aes(day_of_month, fail_rate)) +
  geom_col(aes(fill = day_of_month >= 22), width = 0.75) +
  scale_fill_manual(values = c(`TRUE` = pay$teal, `FALSE` = pay$grey_light), guide = "none") +
  scale_y_continuous(labels = fmt_pct1, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Insufficient-funds declines jump in the last 10 days of the month",
       subtitle = "Card attempts declined for insufficient funds, by day of month",
       x = "Day of month", y = NULL, caption = chart_caption)
keep_chart(p, "fail_day_of_month", width = 10, height = 5)

failure_num <- c("log_amount_eur", "attempt_number", "card_added_days_ago", "day_of_month", "hour",
                 "account_age_days", "psp_error_rate_1h", "issuer_mismatch", "end_of_month", "same_method_as_prev")
failure_auc <- tibble(feature = failure_num) %>%
  mutate(auc = map_dbl(feature, ~ auc_fast(failure[[.x]], failure$is_failed)),
         strength = abs(auc - 0.5) * 2) %>%
  arrange(desc(strength))

# 6. Churn --------------------------------------------------------------------
churn_snap <- churn %>% group_by(snapshot) %>%
  summarise(buyers = n(), churn_rate = mean(is_churned), .groups = "drop")

churn_buckets <- bind_rows(
  churn %>% mutate(bucket = cut(recency_days, c(-Inf, 30, 60, 90, 120, Inf),
                                labels = c("< 30 days", "30-60", "60-90", "90-120", "120-180"))) %>%
    group_by(bucket) %>% summarise(rate = mean(is_churned), n = n()) %>% mutate(feature = "Days since last purchase"),
  churn %>% mutate(bucket = cut(txns_180d, c(0, 1, 2, 4, 8, Inf), labels = c("1", "2", "3-4", "5-8", "9+"))) %>%
    group_by(bucket) %>% summarise(rate = mean(is_churned), n = n()) %>% mutate(feature = "Purchases in last 180 days"),
  # Compare like with like: frequent buyers have more chances to hit a problem,
  # so the raw comparison is misleading (Simpson's paradox). Look at 1-2 purchases only.
  churn %>% filter(txns_180d <= 2) %>%
    mutate(bucket = if_else(low_csat_180d > 0 | lost_parcels > 0 | nad_refunds > 0,
                            "Lost parcel, NAD claim or low CSAT", "No service issue")) %>%
    group_by(bucket) %>% summarise(rate = mean(is_churned), n = n()) %>%
    mutate(feature = "Service experience (buyers with 1-2 purchases)")
) %>% mutate(bucket = as.character(bucket))

p <- churn_buckets %>%
  mutate(bucket = fct_inorder(bucket)) %>%
  ggplot(aes(rate, bucket)) +
  geom_col(fill = pay$teal, width = 0.65) +
  geom_text(aes(label = percent(rate, accuracy = 1)), hjust = -0.15, size = 3.2) +
  facet_wrap(~ feature, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = fmt_pct, limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Recency and frequency drive churn; service problems add to it",
       subtitle = "Share of buyers with no purchase in the next 180 days, all snapshots",
       x = NULL, y = NULL, caption = chart_caption)
keep_chart(p, "churn_buckets", width = 11, height = 6)

churn_num <- c("recency_days", "tenure_days", "txns_60d", "txns_180d", "txns_365d", "spend_180d_eur",
               "avg_order_eur", "failed_checkouts_180d", "declines_180d", "cancelled_180d", "lost_parcels",
               "late_deliveries_180d", "nad_refunds", "tickets_180d", "low_csat_180d", "sales_180d",
               "trend_60_vs_180", "ato_victim", "phone_verified", "is_seller")
churn_auc <- tibble(feature = churn_num) %>%
  mutate(auc = map_dbl(feature, ~ auc_fast(churn[[.x]], churn$is_churned)),
         strength = abs(auc - 0.5) * 2) %>%
  arrange(desc(strength))

# Save ------------------------------------------------------------------------
results <- list(target_summary = target_summary, fraud_month = fraud_month, fraud_type_year = fraud_type_year,
                fraud_auc = fraud_auc, fraud_buckets = fraud_buckets, method_fraud = method_fraud,
                psi = psi_tbl, decline_method_year = decline_method_year, decline_reasons = decline_reasons,
                outage = outage, day_of_month = dom, failure_auc = failure_auc,
                churn_snap = churn_snap, churn_buckets = churn_buckets, churn_auc = churn_auc)
saveRDS(results, file.path(output_dir, "04_explore_results.rds"))
saveRDS(charts, file.path(output_dir, "04_charts.rds"))

print(target_summary, n = Inf)
print(fraud_auc, n = 10)
print(psi_tbl)
message("Exploration done: ", length(charts), " charts saved to Output/charts.")
