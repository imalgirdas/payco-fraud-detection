# -----------------------------------------------------------------------------
# Script:   02_clean.R
#
# Purpose:  Fix the known data quality issues, standardise text and dates,
#           build a daily FX table and add EUR amounts. Every rule is written
#           to a cleaning log with the number of rows it touched.
#
# Input:    Output/01_raw_data.rds
# Output:   Output/02_clean_data.rds, Output/02_cleaning_log.csv
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "lubridate", "stringr", "readr")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(lubridate)
  library(stringr)
  library(readr)
})

output_dir <- "Output"
raw <- readRDS(file.path(output_dir, "01_raw_data.rds"))
cl  <- raw

# Cleaning log ----------------------------------------------------------------
clean_log <- tibble(table = character(), rule = character(), rows_affected = integer())
log_step <- function(table, rule, n) {
  clean_log <<- add_row(clean_log, table = table, rule = rule, rows_affected = as.integer(n))
}

# General tidy-up -------------------------------------------------------------
# Trim text everywhere and make sure every timestamp is UTC
cl <- map(cl, function(df) {
  df %>%
    mutate(across(where(is.character), str_squish),
           across(where(is.POSIXct), ~ with_tz(.x, "UTC")))
})

# Exact duplicates (event replay in attempts, retried export in payouts)
for (t in c("payment_attempts", "payouts")) {
  n_dup <- sum(duplicated(cl[[t]]))
  cl[[t]] <- distinct(cl[[t]])
  log_step(t, "exact duplicate rows removed", n_dup)
}

# Users -----------------------------------------------------------------------
iso3 <- c(FRA = "FR", DEU = "DE", POL = "PL", ESP = "ES", ITA = "IT", LTU = "LT")
cc_raw <- cl$users$country_code
cc_new <- str_to_upper(str_trim(cc_raw))
cc_new <- if_else(cc_new %in% names(iso3), unname(iso3[cc_new]), cc_new)
log_step("users", "country_code trimmed, upper-cased, ISO-3 mapped to ISO-2", sum(cc_raw != cc_new))
cl$users$country_code <- cc_new
log_step("users", "country_code not in countries table", sum(!cc_new %in% cl$countries$country_code))

# signup_at comes in two formats: "YYYY-MM-DD HH:MM:SS" and "DD/MM/YYYY"
s <- cl$users$signup_at
dmy_rows <- str_detect(s, "^\\d{2}/\\d{2}/\\d{4}$")
signup <- ymd_hms(s, quiet = TRUE, tz = "UTC")
signup[dmy_rows] <- as.POSIXct(dmy(s[dmy_rows]), tz = "UTC")  # parse each format separately
cl$users$signup_at <- signup
log_step("users", "signup_at in DD/MM/YYYY parsed to date (time set to 00:00)", sum(dmy_rows))
log_step("users", "signup_at still unparsed", sum(is.na(cl$users$signup_at)))

log_step("users", "missing age_band set to 'unknown'", sum(is.na(cl$users$age_band)))
cl$users <- cl$users %>%
  mutate(age_band = factor(coalesce(age_band, "unknown"),
                           levels = c("18-24", "25-34", "35-44", "45-54", "55+", "unknown")),
         across(c(phone_verified, is_seller, marketing_opt_in), as.integer))

# Devices ---------------------------------------------------------------------
log_step("devices", "missing os set to 'unknown'", sum(is.na(cl$devices$os)))
cl$devices <- cl$devices %>% mutate(os = coalesce(os, "unknown"))

# FX: business days only -> full daily table, EUR added -----------------------
fx_daily <- cl$fx_rates %>%
  mutate(date = as.Date(date)) %>%
  complete(currency, date = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "day")) %>%
  arrange(currency, date) %>%
  group_by(currency) %>%
  fill(rate_per_eur, .direction = "downup") %>%
  ungroup()
log_step("fx_rates", "weekend / holiday rates filled forward from last business day",
         nrow(fx_daily) - nrow(cl$fx_rates))
fx_daily <- bind_rows(
  fx_daily,
  tibble(currency = "EUR", date = seq(as.Date("2021-01-01"), as.Date("2025-12-31"), by = "day"),
         rate_per_eur = 1)
)
cl$fx_daily <- fx_daily

to_eur <- function(df, amount_col, time_col, out_col = "amount_eur") {
  df %>%
    mutate(.date = pmin(as.Date(.data[[time_col]]), as.Date("2025-12-31"))) %>%  # a few retries land on 1 Jan 2026
    left_join(fx_daily, by = c("currency", ".date" = "date")) %>%
    mutate("{out_col}" := round(.data[[amount_col]] / rate_per_eur, 2)) %>%
    select(-.date, -rate_per_eur)
}

# Transactions ----------------------------------------------------------------
tx <- cl$transactions
comp_sum <- round(tx$item_price + tx$buyer_protection_fee + tx$shipping_fee, 2)
bad_total <- abs(tx$total_amount - comp_sum) > 0.05
log_step("transactions", "total_amount not equal to sum of components (0 or 100x) recomputed", sum(bad_total))
tx$total_amount <- if_else(bad_total, comp_sum, tx$total_amount)

log_step("transactions", "missing ip_country set to 'unknown'", sum(is.na(tx$ip_country)))
log_step("transactions", "missing session_minutes kept as NA (imputed in 03_features.R)", sum(is.na(tx$session_minutes)))
tx <- tx %>%
  mutate(ip_country = coalesce(ip_country, "unknown"),
         status = factor(status, levels = c("completed", "refunded", "cancelled", "payment_failed")),
         is_cross_border = as.integer(buyer_country != seller_country)) %>%
  to_eur("total_amount", "created_at", "total_eur") %>%
  to_eur("item_price", "created_at", "item_eur") %>%
  to_eur("buyer_protection_fee", "created_at", "fee_eur")
cl$transactions <- tx

# Payment attempts ------------------------------------------------------------
pa <- cl$payment_attempts
method_map <- c(card = "card", credit_card = "card", wallet = "wallet", `e-wallet` = "wallet",
                bank_transfer = "bank_transfer", `bank transfer` = "bank_transfer", bank = "bank_transfer",
                balance = "balance", bnpl = "bnpl")
m_raw <- pa$payment_method
m_new <- unname(method_map[str_to_lower(m_raw)])
log_step("payment_attempts", "payment_method labels standardised", sum(m_raw != m_new))
log_step("payment_attempts", "payment_method not recognised", sum(is.na(m_new)))
pa$payment_method <- m_new

# Replayed rows can differ only in the messy label, so check again by key
n_dup_id <- sum(duplicated(pa$attempt_id))
pa <- distinct(pa, attempt_id, .keep_all = TRUE)
log_step("payment_attempts", "duplicate attempt_id left after label fix removed", n_dup_id)

neg_lat  <- sum(pa$latency_ms < 0, na.rm = TRUE)
huge_lat <- sum(pa$latency_ms > 120000, na.rm = TRUE)
log_step("payment_attempts", "negative latency_ms set to NA", neg_lat)
log_step("payment_attempts", "latency_ms above 120,000 (impossible) set to NA", huge_lat)
log_step("payment_attempts", "latency_ms missing in source", sum(is.na(pa$latency_ms)))
pa <- pa %>%
  mutate(latency_ms = if_else(latency_ms < 0 | latency_ms > 120000, NA_real_, latency_ms),
         is_approved = as.integer(status == "approved")) %>%
  arrange(transaction_id, attempt_number, attempted_at) %>%
  to_eur("amount", "attempted_at", "amount_eur")
cl$payment_attempts <- pa

# Shipments -------------------------------------------------------------------
sk <- with(cl$shipments, !is.na(delivered_at) & delivered_at < shipped_at)
log_step("shipments", "delivered_at before shipped_at (clock skew) set to NA", sum(sk))
cl$shipments <- cl$shipments %>%
  mutate(delivered_at = if_else(delivered_at < shipped_at, as.POSIXct(NA, tz = "UTC"), delivered_at),
         is_late = as.integer(!is.na(delivered_at) & as.Date(delivered_at) > estimated_delivery_date),
         delivery_days = as.numeric(difftime(delivered_at, shipped_at, units = "days")))

# Refunds ---------------------------------------------------------------------
a_raw <- cl$refunds$amount
comma <- str_detect(a_raw, ",")
log_step("refunds", "amount with comma decimal mark converted to number", sum(comma))
cl$refunds <- cl$refunds %>%
  mutate(amount = as.numeric(str_replace(amount, ",", "."))) %>%
  to_eur("amount", "refunded_at")

# Chargebacks -----------------------------------------------------------------
cb <- cl$chargebacks %>%
  left_join(select(tx, transaction_id, created_at), by = "transaction_id")
early <- cb$opened_at < cb$created_at
log_step("chargebacks", "opened_at before the transaction set to NA (back-dated entry)", sum(early, na.rm = TRUE))
cl$chargebacks <- cb %>%
  mutate(opened_at = if_else(opened_at < created_at, as.POSIXct(NA, tz = "UTC"), opened_at)) %>%
  select(-created_at) %>%
  to_eur("amount", "resolved_at") %>%
  mutate(amount_eur = coalesce(amount_eur, amount))  # pending ones have no resolved date

# Payouts ---------------------------------------------------------------------
cl$payouts <- cl$payouts %>% to_eur("amount", "requested_at")

# Fraud labels / tickets ------------------------------------------------------
log_step("fraud_labels", "duplicate transaction_id", sum(duplicated(cl$fraud_labels$transaction_id)))
log_step("support_tickets", "csat_score missing (not answered)", sum(is.na(cl$support_tickets$csat_score)))

# Final checks ----------------------------------------------------------------
log_step("payment_attempts", "rows without EUR amount after FX join", sum(is.na(cl$payment_attempts$amount_eur)))
log_step("transactions", "rows without EUR amount after FX join", sum(is.na(cl$transactions$total_eur)))

print(clean_log, n = Inf)

# Save ------------------------------------------------------------------------
saveRDS(cl, file.path(output_dir, "02_clean_data.rds"))
write_csv(clean_log, file.path(output_dir, "02_cleaning_log.csv"))
message("Cleaning done: ", nrow(clean_log), " rules logged.")
