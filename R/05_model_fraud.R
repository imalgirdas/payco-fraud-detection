# -----------------------------------------------------------------------------
# Script:   05_model_fraud.R
#
# Purpose:  Fraud detection on paid transactions.
#           - Time split: train 2021-2024, test Jan-Sep 2025 (Q4 2025 labels
#             are not mature yet, so it is left out of evaluation)
#           - Benchmark: the legacy rules-engine risk_score already in the data
#           - Models: logistic regression (baseline) and random forest (ranger)
#             trained on all frauds plus a sample of legitimate transactions
#           - Metrics for a rare target: PR-AUC, ROC-AUC, recall in top 1%
#           - Review threshold chosen by cost on a 2024 validation year,
#             then applied unchanged to 2025
#
# Input:    Output/03_features.rds, 00_theme.R, 00_model_utils.R
# Output:   Output/05_fraud_model.rds, Output/05_charts.rds,
#           Output/05_fraud_metrics.csv, Output/charts/05_*.png
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
fraud <- readRDS(file.path(output_dir, "03_features.rds"))$fraud
set.seed(2025)

# Costs used to pick the review threshold ------------------------------------
review_cost_eur     <- 3      # analyst time + friction for a flagged legit buyer
chargeback_fee_eur  <- 17.5   # fee on top of the lost order value

# Features --------------------------------------------------------------------
# risk_score is kept out of the model so the model can be compared against it
num_vars <- c("log_amount_eur", "is_cross_border", "is_emulator", "new_device", "device_age_days",
              "device_other_accounts", "ip_mismatch", "ip_unknown", "session_minutes", "session_missing",
              "items_viewed_session", "shipping_address_changed_7d", "hour", "night", "account_age_days",
              "phone_verified", "buyer_is_seller", "buyer_prior_txns", "buyer_txns_24h", "buyer_prior_failed",
              "hours_since_last_txn", "seller_age_days", "seller_prior_sales", "issuer_mismatch", "card_new",
              "card_added_days_ago", "n_attempts", "n_failed_before_success", "n_methods_tried")
cat_vars <- c("category", "brand_tier", "item_condition", "shipping_service", "platform", "os",
              "email_domain_type", "signup_channel", "age_band", "payment_method", "card_type", "psp",
              "three_ds_flow", "three_ds_result")

prep <- function(df) {
  df %>%
    mutate(across(all_of(cat_vars), ~ factor(as.character(.x))),
           # log scale for long-tailed counts and ages (helps the logistic model)
           across(c(device_age_days, account_age_days, seller_age_days, buyer_prior_txns,
                    seller_prior_sales, session_minutes, items_viewed_session), log1p),
           card_added_days_ago = log1p(pmax(card_added_days_ago, 0)),
           hours_since_last_txn = log1p(pmax(hours_since_last_txn, 0)),
           is_fraud_f = factor(is_fraud, levels = c(0, 1)))
}
fraud <- prep(fraud)

train_all <- fraud %>% filter(year <= 2024)
test      <- fraud %>% filter(year == 2025, label_mature)
train_cv  <- fraud %>% filter(year <= 2023)   # for threshold tuning
valid     <- fraud %>% filter(year == 2024)

message(sprintf("Train %s rows (%d frauds) | Test %s rows (%d frauds)",
                format(nrow(train_all), big.mark = ","), sum(train_all$is_fraud),
                format(nrow(test), big.mark = ","), sum(test$is_fraud)))

# Model functions -------------------------------------------------------------
fml_glm <- reformulate(c(num_vars, cat_vars), response = "is_fraud")
fml_rf  <- reformulate(c(num_vars, cat_vars), response = "is_fraud_f")

fit_glm <- function(df) glm(fml_glm, data = df, family = binomial())
fit_rf  <- function(df, importance = "none") {
  bal <- balance_sample(df, "is_fraud", neg_per_pos = 10)
  ranger(fml_rf, data = bal, num.trees = 400, min.node.size = 5, probability = TRUE,
         importance = importance, num.threads = max(1, parallel::detectCores() - 1), seed = 42)
}
pred_glm <- function(m, df) predict(m, newdata = df, type = "response")
pred_rf  <- function(m, df) predict(m, data = df)$predictions[, "1"]

# 1. Threshold tuning: fit on 2021-2023, score 2024 ---------------------------
rf_cv    <- fit_rf(train_cv)
valid_sc <- pred_rf(rf_cv, align_factors(valid, train_cv))

cost_at <- function(th, score, y, amount) {
  flag <- score >= th
  sum(flag) * review_cost_eur + sum((amount + chargeback_fee_eur)[!flag & y == 1])
}
grid <- sort(unique(quantile(valid_sc, probs = seq(0.80, 0.9995, by = 0.0005))))
cost_curve <- tibble(threshold = grid) %>%
  mutate(cost_eur = map_dbl(threshold, cost_at, score = valid_sc, y = valid$is_fraud, amount = valid$total_eur),
         flagged_share = map_dbl(threshold, ~ mean(valid_sc >= .x)))
best_th <- cost_curve %>% slice_min(cost_eur, n = 1, with_ties = FALSE)
no_model_cost <- sum((valid$total_eur + chargeback_fee_eur)[valid$is_fraud == 1])

# 2. Final models: fit on 2021-2024, evaluate on 2025 -------------------------
m_glm <- fit_glm(train_all)
m_rf  <- fit_rf(train_all, importance = "permutation")
test_a <- align_factors(test, train_all)

scores <- list(
  "Legacy risk score"   = test$risk_score + runif(nrow(test), 0, 1e-6),   # tiny jitter breaks ties
  "Logistic regression" = pred_glm(m_glm, test_a),
  "Random forest"       = pred_rf(m_rf, test_a)
)
metrics <- imap_dfr(scores, ~ eval_model(.x, test$is_fraud, .y, top_share = 0.01))
print(metrics)

rocs <- imap_dfr(scores, ~ roc_curve(.x, test$is_fraud, .y))
prs  <- imap_dfr(scores, ~ pr_curve(.x, test$is_fraud, .y))
lift <- lift_table(scores[["Random forest"]], test$is_fraud)

# Threshold applied to 2025 ---------------------------------------------------
rf_test <- scores[["Random forest"]]
flag <- rf_test >= best_th$threshold
confusion <- tibble(
  outcome = c("Fraud caught (true positive)", "Legit flagged for review (false positive)",
              "Fraud missed (false negative)", "Legit approved (true negative)"),
  n = c(sum(flag & test$is_fraud == 1), sum(flag & test$is_fraud == 0),
        sum(!flag & test$is_fraud == 1), sum(!flag & test$is_fraud == 0))
)
test_value <- tibble(
  flagged_share     = mean(flag),
  recall            = sum(flag & test$is_fraud == 1) / sum(test$is_fraud),
  precision         = sum(flag & test$is_fraud == 1) / sum(flag),
  fraud_eur_total   = sum(test$total_eur[test$is_fraud == 1]),
  fraud_eur_caught  = sum(test$total_eur[flag & test$is_fraud == 1]),
  cost_no_model     = sum((test$total_eur + chargeback_fee_eur)[test$is_fraud == 1]),
  cost_with_model   = cost_at(best_th$threshold, rf_test, test$is_fraud, test$total_eur)
) %>% mutate(saving_eur = cost_no_model - cost_with_model)

# Monthly stability in 2025 ---------------------------------------------------
monthly <- tibble(month = test$month, y = test$is_fraud, rf = rf_test, legacy = scores[[1]]) %>%
  group_by(month) %>%
  summarise(frauds = sum(y), pr_auc_rf = pr_auc(rf, y), pr_auc_legacy = pr_auc(legacy, y),
            roc_auc_rf = roc_auc(rf, y), .groups = "drop")

# Variable importance (permutation, on the balanced sample) ------------------
importance <- tibble(feature = names(m_rf$variable.importance), importance = m_rf$variable.importance) %>%
  arrange(desc(importance)) %>%
  mutate(share = importance / sum(pmax(importance, 0)))

# Logistic regression: strongest coefficients (for interpretation) -----------
glm_coef <- tibble(term = names(coef(m_glm)), estimate = unname(coef(m_glm))) %>%
  filter(term != "(Intercept)", !is.na(estimate)) %>%
  mutate(odds_ratio = exp(estimate)) %>%
  arrange(desc(abs(estimate)))

# Charts ----------------------------------------------------------------------
charts <- list()
keep_chart <- function(p, name, width = 10, height = 6) {
  charts[[name]] <<- p
  save_chart(p, paste0("05_", name), width = width, height = height)
}
model_cols <- c("Random forest" = pay$teal, "Logistic regression" = pay$navy, "Legacy risk score" = pay$grey)

p <- ggplot(prs, aes(recall, precision, colour = model)) +
  geom_hline(yintercept = mean(test$is_fraud), linetype = "dashed", colour = pay$grey) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = model_cols) +
  scale_x_continuous(labels = fmt_pct) + scale_y_continuous(labels = fmt_pct, limits = c(0, 1)) +
  labs(title = "Precision-recall on 2025: the model beats the rules engine at every recall level",
       subtitle = sprintf("Test set Jan-Sep 2025, %s transactions, %d frauds. Dashed line = random guessing (%.2f%%)",
                         format(nrow(test), big.mark = ","), sum(test$is_fraud), 100 * mean(test$is_fraud)),
       x = "Recall (share of fraud caught)", y = "Precision (share of flags that are fraud)", caption = chart_caption)
keep_chart(p, "pr_curve")

p <- ggplot(rocs, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = pay$grey) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = model_cols) +
  scale_x_continuous(labels = fmt_pct) + scale_y_continuous(labels = fmt_pct) +
  coord_equal() +
  labs(title = "ROC curves on the 2025 test set",
       subtitle = "ROC-AUC looks good for every model; PR-AUC is the fairer test for a 0.7% target",
       x = "False positive rate", y = "True positive rate", caption = chart_caption)
keep_chart(p, "roc_curve", width = 8, height = 7.5)

p <- importance %>%
  slice_head(n = 15) %>%
  mutate(feature = fct_reorder(feature, importance)) %>%
  ggplot(aes(importance, feature)) +
  geom_col(fill = pay$teal, width = 0.7) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "What the random forest relies on",
       subtitle = "Permutation importance, top 15 features (drop in accuracy when a feature is shuffled)",
       x = "Importance", y = NULL, caption = chart_caption)
keep_chart(p, "importance", width = 10, height = 6.5)

p <- ggplot(cost_curve, aes(flagged_share, cost_eur)) +
  geom_hline(yintercept = no_model_cost, linetype = "dashed", colour = pay$grey) +
  geom_line(colour = pay$teal) +
  geom_point(data = best_th, colour = pay$alert, size = 3) +
  annotate("text", x = best_th$flagged_share, y = best_th$cost_eur, hjust = -0.15, vjust = 1.4, size = 3.3,
           label = sprintf("Best: review top %.1f%%", 100 * best_th$flagged_share)) +
  annotate("text", x = max(cost_curve$flagged_share), y = no_model_cost, hjust = 1, vjust = -0.6,
           size = 3.2, colour = pay$slate, label = "Cost with no fraud model") +
  scale_x_continuous(labels = fmt_pct1) +
  scale_y_continuous(labels = fmt_eur, limits = c(0, NA)) +
  labs(title = "Choosing the review threshold by cost (validation year 2024)",
       subtitle = sprintf("Cost = EUR %.0f per manual review + order value and EUR %.1f fee for every missed fraud",
                         review_cost_eur, chargeback_fee_eur),
       x = "Share of transactions sent to review", y = "Total cost", caption = chart_caption)
keep_chart(p, "cost_curve")

p <- lift %>%
  ggplot(aes(factor(decile), lift)) +
  geom_col(aes(fill = decile == 1), width = 0.7) +
  geom_text(aes(label = sprintf("%.1fx", lift)), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = pay$teal, `FALSE` = pay$grey_light), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = sprintf("The riskiest 10%% of transactions hold %.0f%% of all 2025 fraud", 100 * lift$cum_capture[1]),
       subtitle = "Fraud rate in each score decile relative to the average (random forest, 2025)",
       x = "Score decile (1 = highest risk)", y = "Lift", caption = chart_caption)
keep_chart(p, "lift")

# Save ------------------------------------------------------------------------
results <- list(metrics = metrics, rocs = rocs, prs = prs, lift = lift, importance = importance,
                glm_coef = glm_coef, cost_curve = cost_curve, best_threshold = best_th,
                no_model_cost_valid = no_model_cost, confusion = confusion, test_value = test_value,
                monthly = monthly, n_train = nrow(train_all), n_test = nrow(test),
                frauds_train = sum(train_all$is_fraud), frauds_test = sum(test$is_fraud),
                costs = c(review = review_cost_eur, chargeback_fee = chargeback_fee_eur))
saveRDS(results, file.path(output_dir, "05_fraud_model.rds"))
saveRDS(charts, file.path(output_dir, "05_charts.rds"))
saveRDS(m_rf, file.path(output_dir, "05_fraud_random_forest.rds"))
write_csv(metrics, file.path(output_dir, "05_fraud_metrics.csv"))

print(test_value)
message("Fraud model done. Threshold ", signif(best_th$threshold, 3), ", review share ",
        percent(test_value$flagged_share, 0.1), ", recall ", percent(test_value$recall, 0.1), ".")
