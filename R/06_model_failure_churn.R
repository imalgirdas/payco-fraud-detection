# -----------------------------------------------------------------------------
# Script:   06_model_failure_churn.R
# Purpose:  Two secondary models, same approach as the fraud model:
#           A. Payment failure - will this attempt be declined or error out?
#              Train 2021-2024 attempts, test 2025. Outcome-time columns
#              (risk decision, 3DS result, latency) are not used.
#           B. Buyer churn - will an active buyer make no purchase in the next
#              180 days? Train on snapshots 2023-06 to 2024-12, test 2025-06.
#           Each: logistic regression vs random forest, ROC-AUC / PR-AUC,
#           decile table and feature importance.
#
# Input:    Output/03_features.rds, 00_theme.R, 00_model_utils.R
# Output:   Output/06_models.rds, Output/06_charts.rds,
#           Output/06_model_metrics.csv, Output/charts/06_*.png
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "scales", "forcats", "ranger", "readr", "ragg")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(ranger)
  library(readr)
})

src <- function(f) source(if (file.exists(file.path("R", f))) file.path("R", f) else f)
src("00_theme.R")
src("00_model_utils.R")

output_dir <- "Output"
f <- readRDS(file.path(output_dir, "03_features.rds"))
set.seed(2025)
n_threads <- max(1, parallel::detectCores() - 1)

# Shared fitting routine ------------------------------------------------------
run_task <- function(train, test, target, num_vars, cat_vars, task, rf_rows = 80000) {
  prep <- function(df) df %>% mutate(across(all_of(cat_vars), ~ factor(as.character(.x))),
                                     .y = factor(.data[[target]], levels = c(0, 1)))
  train <- prep(train)
  test  <- align_factors(prep(test), train)

  m_glm <- glm(reformulate(c(num_vars, cat_vars), response = target), data = train, family = binomial())

  rf_train <- if (nrow(train) > rf_rows) train[sample(nrow(train), rf_rows), ] else train
  m_rf <- ranger(reformulate(c(num_vars, cat_vars), response = ".y"), data = rf_train,
                 num.trees = 300, min.node.size = 20, probability = TRUE,
                 importance = "permutation", num.threads = n_threads, seed = 42)

  y <- test[[target]]
  scores <- list("Logistic regression" = predict(m_glm, newdata = test, type = "response"),
                 "Random forest"       = predict(m_rf, data = test)$predictions[, "1"])

  metrics <- imap_dfr(scores, ~ eval_model(.x, y, .y, top_share = 0.10)) %>% mutate(task = task, .before = 1)
  best <- metrics$model[which.max(metrics$pr_auc)]
  list(
    metrics    = metrics,
    best_model = best,
    rocs       = imap_dfr(scores, ~ roc_curve(.x, y, .y)) %>% mutate(task = task),
    deciles    = lift_table(scores[[best]], y),
    importance = tibble(feature = names(m_rf$variable.importance), importance = m_rf$variable.importance) %>%
      arrange(desc(importance)),
    n_train = nrow(train), n_test = nrow(test), rate_train = mean(train[[target]]), rate_test = mean(y)
  )
}

# A. Payment failure ----------------------------------------------------------
failure <- f$failure %>%
  mutate(card_added_days_ago = log1p(pmax(card_added_days_ago, 0)),
         account_age_days = log1p(account_age_days),
         prev_decline_code = if_else(prev_decline_code %in% c("first_attempt", "insufficient_funds", "do_not_honor",
                                                              "three_ds_abandoned", "three_ds_failed", "processing_error",
                                                              "gateway_timeout", "expired_card", "incorrect_card_details",
                                                              "bnpl_rejected", "authorization_timeout"),
                                     prev_decline_code, "other"))

fail_num <- c("log_amount_eur", "attempt_number", "same_method_as_prev", "day_of_month", "end_of_month",
              "hour", "account_age_days", "issuer_mismatch", "card_added_days_ago", "psp_error_rate_1h")
fail_cat <- c("payment_method", "card_type", "card_brand", "wallet_provider", "psp", "prev_decline_code",
              "platform", "age_band")

fail_res <- run_task(train = filter(failure, year <= 2024), test = filter(failure, year == 2025),
                     target = "is_failed", num_vars = fail_num, cat_vars = fail_cat, task = "Payment failure")
print(fail_res$metrics)

# B. Buyer churn --------------------------------------------------------------
churn <- f$churn %>%
  mutate(across(c(spend_180d_eur, avg_order_eur, tenure_days), log1p))

churn_num <- c("recency_days", "tenure_days", "txns_60d", "txns_180d", "txns_365d", "txns_total",
               "spend_180d_eur", "avg_order_eur", "trend_60_vs_180", "failed_checkouts_180d", "declines_180d",
               "cancelled_180d", "lost_parcels", "late_deliveries_180d", "nad_refunds", "tickets_180d",
               "low_csat_180d", "avg_csat", "csat_missing", "sales_180d", "ato_victim", "phone_verified",
               "is_seller", "marketing_opt_in")
churn_cat <- c("age_band", "signup_channel", "email_domain_type")

churn_res <- run_task(train = filter(churn, snapshot < as.Date("2025-06-30")),
                      test = filter(churn, snapshot == as.Date("2025-06-30")),
                      target = "is_churned", num_vars = churn_num, cat_vars = churn_cat, task = "Buyer churn")
print(churn_res$metrics)

# Charts ----------------------------------------------------------------------
charts <- list()
keep_chart <- function(p, name, width = 10, height = 6) {
  charts[[name]] <<- p
  save_chart(p, paste0("06_", name), width = width, height = height)
}

imp_chart <- function(res, title) {
  res$importance %>%
    slice_head(n = 12) %>%
    mutate(feature = fct_reorder(feature, importance)) %>%
    ggplot(aes(importance, feature)) +
    geom_col(fill = pay$teal, width = 0.7) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, subtitle = "Random forest permutation importance, top 12 features",
         x = "Importance", y = NULL, caption = chart_caption)
}
decile_chart <- function(res, title, what) {
  res$deciles %>%
    ggplot(aes(factor(decile), rate)) +
    geom_col(aes(fill = decile <= 2), width = 0.7) +
    geom_hline(yintercept = res$rate_test, linetype = "dashed", colour = pay$grey) +
    geom_text(aes(label = percent(rate, accuracy = 1)), vjust = -0.5, size = 3.2) +
    scale_fill_manual(values = c(`TRUE` = pay$teal, `FALSE` = pay$grey_light), guide = "none") +
    scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.12))) +
    labs(title = title,
         subtitle = sprintf("Actual %s by predicted risk decile (%s, test set). Dashed line = average",
                            what, tolower(res$best_model)),
         x = "Predicted risk decile (1 = highest)", y = NULL, caption = chart_caption)
}

keep_chart(imp_chart(fail_res, "Payment failure: card age and card type matter most"), "failure_importance")
keep_chart(decile_chart(fail_res, "The model ranks risky attempts well", "failure rate"), "failure_deciles")
keep_chart(imp_chart(churn_res, "Churn: recency and recent activity dominate"), "churn_importance")
keep_chart(decile_chart(churn_res, "Churn risk deciles separate likely leavers from loyal buyers", "churn rate"), "churn_deciles")

rocs <- bind_rows(fail_res$rocs, churn_res$rocs)
p <- ggplot(rocs, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = pay$grey) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ task) +
  scale_colour_manual(values = c("Random forest" = pay$teal, "Logistic regression" = pay$navy)) +
  scale_x_continuous(labels = fmt_pct) + scale_y_continuous(labels = fmt_pct) +
  coord_equal() +
  labs(title = "ROC curves on the held-out test period",
       subtitle = "Payment failure: 2025 attempts. Churn: buyers active at 30 Jun 2025",
       x = "False positive rate", y = "True positive rate", caption = chart_caption)
keep_chart(p, "roc_curves", width = 11, height = 6)

# Save ------------------------------------------------------------------------
metrics <- bind_rows(fail_res$metrics, churn_res$metrics)
saveRDS(list(failure = fail_res, churn = churn_res, metrics = metrics), file.path(output_dir, "06_models.rds"))
saveRDS(charts, file.path(output_dir, "06_charts.rds"))
write_csv(metrics, file.path(output_dir, "06_model_metrics.csv"))
message("Failure and churn models done.")
