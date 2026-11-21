# -----------------------------------------------------------------------------
# Script:   01_load.R
#
# Purpose:  Read the 17 raw CSV tables, check that keys are unique and that
#           foreign keys resolve, and save everything as one list.
#
# Input:    Data/*.csv
# Output:   Output/01_raw_data.rds, Output/01_load_summary.csv
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("readr", "dplyr", "tibble", "purrr")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(purrr)
})

data_dir   <- "Data"
output_dir <- "Output"
dir.create(output_dir, showWarnings = FALSE)

# Tables ----------------------------------------------------------------------
tables <- c(
  "calendar", "countries", "fx_rates", "categories", "psps", "decline_codes",
  "users", "devices", "listings", "transactions", "payment_attempts",
  "shipments", "refunds", "chargebacks", "fraud_labels", "payouts",
  "support_tickets"
)

# Columns that are messy on purpose are read as text and fixed in 02_clean.R:
#   users$signup_at - two date formats
#   refunds$amount  - some values use a comma decimal mark ("12,50"),
#                     which readr would otherwise read as 1250
text_cols <- list(
  users   = cols(signup_at = col_character(), .default = col_guess()),
  refunds = cols(amount = col_character(), .default = col_guess())
)

read_table <- function(name) {
  read_csv(
    file.path(data_dir, paste0(name, ".csv")),
    col_types = if (name %in% names(text_cols)) text_cols[[name]] else NULL,
    guess_max = 100000,
    show_col_types = FALSE,
    na = c("", "NA")
  )
}

raw <- set_names(map(tables, read_table), tables)

# Key checks ------------------------------------------------------------------
# Duplicated keys are expected in payment_attempts and payouts (replayed rows)
keys <- c(users = "user_id", devices = "device_id", listings = "listing_id",
          transactions = "transaction_id", payment_attempts = "attempt_id",
          shipments = "shipment_id", refunds = "refund_id", chargebacks = "chargeback_id",
          payouts = "payout_id", support_tickets = "ticket_id", fraud_labels = "transaction_id")

orphan <- function(child, col, parent, pcol) sum(!raw[[child]][[col]] %in% raw[[parent]][[pcol]])

load_summary <- tibble(
  table = names(raw),
  rows  = map_int(raw, nrow),
  cols  = map_int(raw, ncol),
  duplicated_keys = map_int(names(raw), function(t) {
    if (t %in% names(keys)) sum(duplicated(raw[[t]][[keys[[t]]]])) else NA_integer_
  })
)

fk_checks <- tribble(
  ~check,                                 ~orphans,
  "transactions.buyer_id -> users",       orphan("transactions", "buyer_id", "users", "user_id"),
  "transactions.seller_id -> users",      orphan("transactions", "seller_id", "users", "user_id"),
  "transactions.listing_id -> listings",  orphan("transactions", "listing_id", "listings", "listing_id"),
  "transactions.device_id -> devices",    orphan("transactions", "device_id", "devices", "device_id"),
  "payment_attempts -> transactions",     orphan("payment_attempts", "transaction_id", "transactions", "transaction_id"),
  "fraud_labels -> transactions",         orphan("fraud_labels", "transaction_id", "transactions", "transaction_id"),
  "chargebacks -> transactions",          orphan("chargebacks", "transaction_id", "transactions", "transaction_id")
)

print(load_summary, n = Inf)
print(fk_checks)

# Save ------------------------------------------------------------------------
saveRDS(raw, file.path(output_dir, "01_raw_data.rds"))
write_csv(load_summary, file.path(output_dir, "01_load_summary.csv"))
message("Loaded ", length(raw), " tables, ", format(sum(load_summary$rows), big.mark = ","), " rows.")
