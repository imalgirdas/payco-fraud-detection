# -----------------------------------------------------------------------------
# Script:   03_features.R
#
# Purpose:  Build three modelling tables. Every feature uses only information
#           available at prediction time (no look-ahead):
#             1. fraud   - one row per paid transaction, label = confirmed fraud
#             2. failure - one row per payment attempt, label = not approved
#             3. churn   - one row per active buyer per snapshot date,
#                          label = no purchase in the next 180 days
#           Also builds the monthly / yearly KPI tables used in 04 and 07.
#
# Input:    Output/02_clean_data.rds
# Output:   Output/03_features.rds
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "lubridate", "stringr")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(stringr)
})

output_dir <- "Output"
cl <- readRDS(file.path(output_dir, "02_clean_data.rds"))

tx  <- cl$transactions
pa  <- cl$payment_attempts
us  <- cl$users
dv  <- cl$devices
lst <- cl$listings
fl  <- cl$fraud_labels
days_between <- function(a, b) as.numeric(difftime(a, b, units = "days"))

# Count of earlier events inside a time window, per group --------------------
# times must be sorted inside each group. Returns events in [t - window, t).
count_prior_window <- function(times, window_secs) {
  t <- as.numeric(times)
  seq_along(t) - 1 - findInterval(t - window_secs, t, left.open = FALSE)
}

# Base transaction table ------------------------------------------------------
base <- tx %>%
  left_join(select(lst, listing_id, category, brand_tier, item_condition), by = "listing_id") %>%
  left_join(select(us, buyer_id = user_id, buyer_signup = signup_at, email_domain_type,
                   phone_verified, signup_channel, age_band, buyer_is_seller = is_seller),
            by = "buyer_id") %>%
  left_join(select(us, seller_id = user_id, seller_signup = signup_at), by = "seller_id") %>%
  left_join(select(dv, device_id, device_first_seen = first_seen_at, is_emulator, os),
            by = "device_id")

# Buyer history (only earlier checkouts count) -------------------------------
base <- base %>%
  arrange(buyer_id, created_at) %>%
  group_by(buyer_id) %>%
  mutate(
    buyer_prior_txns      = row_number() - 1,
    buyer_txns_24h        = count_prior_window(created_at, 24 * 3600),
    buyer_prior_failed    = cumsum(lag(status == "payment_failed", default = FALSE)),
    hours_since_last_txn  = as.numeric(difftime(created_at, lag(created_at), units = "hours"))
  ) %>%
  ungroup() %>%
  arrange(seller_id, created_at) %>%
  group_by(seller_id) %>%
  mutate(seller_prior_sales = row_number() - 1) %>%
  ungroup()

# Accounts seen on the same device before this checkout -----------------------
# First time each buyer used each device; count other buyers that used it earlier
dev_pairs <- tx %>%
  group_by(device_id, buyer_id) %>%
  summarise(first_use = min(created_at), .groups = "drop")
dev_shared <- base %>%
  select(transaction_id, device_id, buyer_id, created_at) %>%
  inner_join(dev_pairs, by = "device_id", suffix = c("", "_other"), relationship = "many-to-many") %>%
  filter(buyer_id_other != buyer_id, first_use < created_at) %>%
  count(transaction_id, name = "device_other_accounts")
base <- base %>%
  left_join(dev_shared, by = "transaction_id") %>%
  mutate(device_other_accounts = coalesce(device_other_accounts, 0L))

# Attempt summary per transaction ---------------------------------------------
pa <- pa %>%
  left_join(select(tx, transaction_id, buyer_country), by = "transaction_id") %>%
  mutate(issuer_mismatch = as.integer(!is.na(card_issuer_country) & card_issuer_country != buyer_country))

att_sum <- pa %>%
  group_by(transaction_id) %>%
  summarise(n_attempts = n(),
            n_failed_before_success = sum(status != "approved"),
            n_methods_tried = n_distinct(payment_method),
            .groups = "drop")
ok_att <- pa %>%
  filter(status == "approved") %>%
  distinct(transaction_id, .keep_all = TRUE) %>%
  select(transaction_id, payment_method, wallet_provider, card_type, card_brand, issuer_mismatch,
         card_added_days_ago, psp, three_ds_flow, three_ds_result, risk_score)

# 1. Fraud table --------------------------------------------------------------
# Paid transactions only. Q4 2025 is kept but flagged: labels are not mature yet.
fraud <- base %>%
  filter(status != "payment_failed") %>%
  inner_join(ok_att, by = "transaction_id") %>%
  left_join(att_sum, by = "transaction_id") %>%
  mutate(
    is_fraud           = as.integer(transaction_id %in% fl$transaction_id),
    year               = year(created_at),
    month              = floor_date(created_at, "month"),
    label_mature       = created_at < as.POSIXct("2025-10-01", tz = "UTC"),
    log_amount_eur     = log1p(total_eur),
    account_age_days   = pmax(days_between(created_at, buyer_signup), 0),
    seller_age_days    = pmax(days_between(created_at, seller_signup), 0),
    device_age_days    = pmax(days_between(created_at, device_first_seen), 0),
    new_device         = as.integer(device_age_days < 1),
    ip_mismatch        = as.integer(ip_country != buyer_country & ip_country != "unknown"),
    ip_unknown         = as.integer(ip_country == "unknown"),
    session_missing    = as.integer(is.na(session_minutes)),
    session_minutes    = coalesce(session_minutes, median(session_minutes, na.rm = TRUE)),
    hour               = hour(created_at),
    night              = as.integer(hour <= 5),
    card_new           = as.integer(!is.na(card_added_days_ago) & card_added_days_ago <= 1),
    card_added_days_ago = coalesce(card_added_days_ago, -1),
    hours_since_last_txn = coalesce(hours_since_last_txn, -1),
    card_type          = coalesce(card_type, "none"),
    three_ds_flow      = coalesce(three_ds_flow, "not_card"),
    three_ds_result    = coalesce(three_ds_result, "none")
  ) %>%
  select(transaction_id, created_at, year, month, label_mature, is_fraud, total_eur,
         log_amount_eur, category, brand_tier, item_condition, shipping_service, is_cross_border,
         platform, os, is_emulator, new_device, device_age_days, device_other_accounts,
         ip_mismatch, ip_unknown, session_minutes, session_missing, items_viewed_session,
         shipping_address_changed_7d, hour, night, account_age_days, email_domain_type,
         phone_verified, signup_channel, age_band, buyer_is_seller, buyer_prior_txns,
         buyer_txns_24h, buyer_prior_failed, hours_since_last_txn, seller_age_days,
         seller_prior_sales, payment_method, card_type, issuer_mismatch, card_new,
         card_added_days_ago, psp, three_ds_flow, three_ds_result, risk_score,
         n_attempts, n_failed_before_success, n_methods_tried) %>%
  arrange(created_at)

# 2. Payment failure table ----------------------------------------------------
# One row per attempt. risk_decision, 3DS outcome and latency are left out:
# they are decided during the attempt and give the answer away.
psp_recent <- pa %>%
  arrange(psp, attempted_at) %>%
  group_by(psp) %>%
  mutate(
    .t = as.numeric(attempted_at),
    .err_cum = cumsum(status == "error"),
    .j = findInterval(.t - 3600, .t),
    psp_attempts_1h = row_number() - 1 - .j,
    psp_errors_1h   = lag(.err_cum, default = 0) - c(0, .err_cum)[.j + 1],
    psp_error_rate_1h = if_else(psp_attempts_1h > 0, psp_errors_1h / psp_attempts_1h, 0)
  ) %>%
  ungroup() %>%
  select(attempt_id, psp_attempts_1h, psp_error_rate_1h)

failure <- pa %>%
  left_join(psp_recent, by = "attempt_id") %>%
  left_join(select(tx, transaction_id, buyer_id, platform), by = "transaction_id") %>%
  left_join(select(us, buyer_id = user_id, buyer_signup = signup_at, age_band), by = "buyer_id") %>%
  arrange(transaction_id, attempt_number) %>%
  mutate(.same_txn = transaction_id == lag(transaction_id, default = -1),   # faster than group_by + lag
         prev_decline_code = if_else(.same_txn, coalesce(lag(decline_code), "unknown"), "first_attempt"),
         same_method_as_prev = as.integer(.same_txn & payment_method == lag(payment_method))) %>%
  select(-.same_txn) %>%
  mutate(
    is_failed        = 1L - is_approved,
    year             = pmin(year(attempted_at), 2025),   # a few retries spill into 1 Jan 2026
    log_amount_eur   = log1p(amount_eur),
    day_of_month     = mday(attempted_at),
    end_of_month     = as.integer(day_of_month >= 22),
    hour             = hour(attempted_at),
    account_age_days = pmax(days_between(attempted_at, buyer_signup), 0),
    card_added_days_ago = coalesce(card_added_days_ago, -1),
    card_type        = coalesce(card_type, "none"),
    card_brand       = coalesce(card_brand, "none"),
    wallet_provider  = coalesce(wallet_provider, "none")
  ) %>%
  select(attempt_id, transaction_id, attempted_at, year, is_failed, status, decline_code,
         payment_method, card_type, card_brand, wallet_provider, issuer_mismatch,
         card_added_days_ago, psp, attempt_number, prev_decline_code, same_method_as_prev,
         log_amount_eur, amount_eur, day_of_month, end_of_month, hour, platform, age_band,
         account_age_days, buyer_country, psp_attempts_1h, psp_error_rate_1h) %>%
  arrange(attempted_at)

# 3. Churn table --------------------------------------------------------------
# Snapshot every six months. Population: buyers with a checkout in the 180
# days before the snapshot. Label: no checkout in the 180 days after it.
snapshots <- as.POSIXct(c("2023-06-30", "2023-12-31", "2024-06-30", "2024-12-31", "2025-06-30"), tz = "UTC")

ship <- cl$shipments %>% select(transaction_id, status_ship = status, is_late)
refund_flags <- cl$refunds %>%
  group_by(transaction_id) %>%
  summarise(refund_nad = any(reason == "not_as_described"), .groups = "drop")
ato_victims <- fl %>% filter(fraud_type == "account_takeover") %>%
  inner_join(select(tx, transaction_id, buyer_id), by = "transaction_id") %>%
  select(buyer_id, labeled_at)
declines <- pa %>% filter(status != "approved") %>%
  left_join(select(tx, transaction_id, buyer_id), by = "transaction_id") %>%
  select(buyer_id, attempted_at)
tickets <- cl$support_tickets

tx_hist <- tx %>%
  left_join(ship, by = "transaction_id") %>%
  left_join(refund_flags, by = "transaction_id") %>%
  select(transaction_id, buyer_id, seller_id, created_at, status, total_eur, status_ship, is_late, refund_nad)

build_snapshot <- function(snap) {
  d180 <- snap - 180 * 86400
  h   <- tx_hist %>% filter(created_at <= snap)
  fut <- tx_hist %>% filter(created_at > snap, created_at <= snap + 180 * 86400)
  active <- h %>% filter(created_at > d180) %>% distinct(buyer_id)

  # Flags first, then plain sums per buyer (much faster than filtering inside summarise)
  hb <- h %>%
    semi_join(active, by = "buyer_id") %>%
    mutate(age_d  = as.numeric(difftime(snap, created_at, units = "days")),
           in_60  = age_d < 60, in_180 = age_d < 180, in_365 = age_d < 365,
           paid   = status != "payment_failed")
  feats <- hb %>%
    group_by(buyer_id) %>%
    summarise(
      recency_days     = min(age_d),
      tenure_days      = max(age_d),
      txns_60d         = sum(in_60),
      txns_180d        = sum(in_180),
      txns_365d        = sum(in_365),
      txns_total       = n(),
      spend_180d_eur   = sum(total_eur * (in_180 & paid)),
      avg_order_eur    = sum(total_eur * paid) / pmax(sum(paid), 1),
      failed_checkouts_180d = sum(!paid & in_180),
      cancelled_180d   = sum(status == "cancelled" & in_180),
      lost_parcels     = sum(status_ship == "lost", na.rm = TRUE),
      late_deliveries_180d = sum(is_late == 1 & in_180, na.rm = TRUE),
      nad_refunds      = sum(refund_nad, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(trend_60_vs_180 = txns_60d / pmax(txns_180d, 1))

  sales <- h %>% filter(created_at > d180) %>% count(seller_id, name = "sales_180d")
  dec   <- declines %>% filter(attempted_at <= snap, attempted_at > d180) %>%
    count(buyer_id, name = "declines_180d")
  tk    <- tickets %>% filter(created_at <= snap, created_at > d180) %>%
    group_by(user_id) %>%
    summarise(tickets_180d = n(),
              low_csat_180d = sum(csat_score <= 2, na.rm = TRUE),
              avg_csat = mean(csat_score, na.rm = TRUE), .groups = "drop")
  ato   <- ato_victims %>% filter(labeled_at <= snap) %>% distinct(buyer_id) %>% mutate(ato_victim = 1L)

  feats %>%
    left_join(sales, by = c("buyer_id" = "seller_id")) %>%
    left_join(dec, by = "buyer_id") %>%
    left_join(tk, by = c("buyer_id" = "user_id")) %>%
    left_join(ato, by = "buyer_id") %>%
    left_join(select(us, buyer_id = user_id, country_code, age_band, signup_channel, phone_verified,
                     is_seller, marketing_opt_in, email_domain_type), by = "buyer_id") %>%
    mutate(
      snapshot = as.Date(snap),
      across(c(sales_180d, declines_180d, tickets_180d, low_csat_180d, ato_victim), ~ coalesce(.x, 0L)),
      csat_missing = as.integer(is.na(avg_csat)),
      avg_csat = coalesce(avg_csat, 0),
      is_churned = as.integer(!buyer_id %in% fut$buyer_id)
    )
}
churn <- map_dfr(snapshots, build_snapshot) %>% relocate(snapshot, buyer_id, is_churned)

# KPI tables for EDA and dashboard --------------------------------------------
attempt_y <- pa %>%
  mutate(year = year(attempted_at)) %>%
  group_by(year) %>%
  summarise(attempts = n(), approval_rate = mean(status == "approved"),
            error_rate = mean(status == "error"), .groups = "drop")

kpi_year <- fraud %>%
  group_by(year) %>%
  summarise(paid_txns = n(), gmv_eur = sum(total_eur), aov_eur = mean(total_eur),
            fraud_rate = mean(is_fraud), fraud_eur = sum(total_eur[is_fraud == 1]),
            .groups = "drop") %>%
  left_join(tx %>% mutate(year = year(created_at)) %>% group_by(year) %>%
              summarise(checkouts = n(), checkout_success = mean(status != "payment_failed"),
                        buyers = n_distinct(buyer_id), .groups = "drop"), by = "year") %>%
  left_join(attempt_y, by = "year") %>%
  left_join(cl$chargebacks %>% left_join(select(tx, transaction_id, created_at), by = "transaction_id") %>%
              mutate(year = year(created_at)) %>% count(year, name = "chargebacks"), by = "year") %>%
  mutate(chargeback_rate = chargebacks / paid_txns,
         gmv_yoy = gmv_eur / lag(gmv_eur) - 1)

kpi_month <- fraud %>%
  group_by(month) %>%
  summarise(paid_txns = n(), gmv_eur = sum(total_eur), fraud_rate = mean(is_fraud), .groups = "drop") %>%
  left_join(pa %>% mutate(month = floor_date(attempted_at, "month")) %>% group_by(month) %>%
              summarise(approval_rate = mean(status == "approved"), error_rate = mean(status == "error"),
                        .groups = "drop"), by = "month")

method_year <- fraud %>% count(year, payment_method) %>%
  group_by(year) %>% mutate(share = n / sum(n)) %>% ungroup()

# Save ------------------------------------------------------------------------
features <- list(fraud = fraud, failure = failure, churn = churn,
                 kpi_year = kpi_year, kpi_month = kpi_month, method_year = method_year)
saveRDS(features, file.path(output_dir, "03_features.rds"))

message(sprintf("fraud: %s rows (%.2f%% fraud) | failure: %s rows (%.1f%% failed) | churn: %s rows (%.1f%% churned)",
                format(nrow(fraud), big.mark = ","), 100 * mean(fraud$is_fraud),
                format(nrow(failure), big.mark = ","), 100 * mean(failure$is_failed),
                format(nrow(churn), big.mark = ","), 100 * mean(churn$is_churned)))
