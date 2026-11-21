# -----------------------------------------------------------------------------
# Script:   00_model_utils.R
# Purpose:  Small helpers shared by the model scripts (05, 06): evaluation
#           metrics for imbalanced targets, curves for plotting, lift tables
#           and a balanced sampler for the random forest.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ROC-AUC (Mann-Whitney, fast on large data) ---------------------------------
roc_auc <- function(score, y) {
  r  <- rank(score)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * n0)
}

# PR-AUC as average precision -------------------------------------------------
pr_auc <- function(score, y) {
  o  <- order(score, decreasing = TRUE)
  y  <- y[o]
  tp <- cumsum(y)
  precision <- tp / seq_along(y)
  sum(precision[y == 1]) / sum(y)
}

# Recall and precision when the top k% of scores is flagged -------------------
at_top <- function(score, y, share) {
  k <- ceiling(length(score) * share)
  top <- order(score, decreasing = TRUE)[seq_len(k)]
  c(recall = sum(y[top]) / sum(y), precision = mean(y[top]))
}

# One-row summary for a model -------------------------------------------------
eval_model <- function(score, y, model, top_share = 0.01) {
  t1 <- at_top(score, y, top_share)
  tibble(model = model, roc_auc = roc_auc(score, y), pr_auc = pr_auc(score, y),
         base_rate = mean(y), recall_top = t1[["recall"]], precision_top = t1[["precision"]],
         top_share = top_share)
}

# Curves for plotting (thinned to ~400 points) --------------------------------
roc_curve <- function(score, y, model) {
  o <- order(score, decreasing = TRUE); y <- y[o]
  tpr <- cumsum(y) / sum(y); fpr <- cumsum(1 - y) / sum(1 - y)
  keep <- unique(round(seq(1, length(y), length.out = 400)))
  tibble(model = model, fpr = c(0, fpr[keep]), tpr = c(0, tpr[keep]))
}
pr_curve <- function(score, y, model) {
  o <- order(score, decreasing = TRUE); y <- y[o]
  tp <- cumsum(y)
  keep <- unique(round(seq(1, length(y), length.out = 400)))
  tibble(model = model, recall = tp[keep] / sum(y), precision = tp[keep] / keep)
}

# Lift by score decile ---------------------------------------------------------
lift_table <- function(score, y, groups = 10) {
  tibble(score = score, y = y) %>%
    mutate(decile = ntile(-score, groups)) %>%
    group_by(decile) %>%
    summarise(rows = n(), positives = sum(y), rate = mean(y), avg_score = mean(score), .groups = "drop") %>%
    mutate(lift = rate / mean(y), cum_capture = cumsum(positives) / sum(positives))
}

# Keep all positives and a sample of negatives (for the random forest) -------
balance_sample <- function(df, target, neg_per_pos = 10, seed = 42) {
  set.seed(seed)
  pos <- df[df[[target]] == 1, ]
  neg <- df[df[[target]] == 0, ]
  n_neg <- min(nrow(neg), nrow(pos) * neg_per_pos)
  bind_rows(pos, neg[sample(nrow(neg), n_neg), ])
}

# Match factor levels of new data to the training data ------------------------
align_factors <- function(new, train) {
  for (v in names(train)[vapply(train, is.factor, logical(1))]) {
    x <- as.character(new[[v]])
    x[!x %in% levels(train[[v]])] <- levels(train[[v]])[1]
    new[[v]] <- factor(x, levels = levels(train[[v]]))
  }
  new
}
