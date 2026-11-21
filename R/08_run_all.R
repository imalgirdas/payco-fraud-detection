# -----------------------------------------------------------------------------
# Script:   08_run_all.R
# Purpose:  Run the full pipeline in order - load, clean, features, explore,
#           fraud model, failure and churn models - and render the HTML
#           dashboard. Each step runs in its own environment and the run
#           stops at the first error.
#
# Input:    Data/*.csv
# Output:   Output/*, 07_dashboard.html, Output/08_run_log.csv
# -----------------------------------------------------------------------------

# Settings --------------------------------------------------------------------
script_dir <- if (dir.exists("R") && file.exists("R/01_load.R")) "R" else "."
open_dashboard <- interactive()   # open the HTML in the browser when run from RStudio

steps <- c(
  "01_load.R",
  "02_clean.R",
  "03_features.R",
  "04_explore.R",
  "05_model_fraud.R",
  "06_model_failure_churn.R",
  "07_dashboard.Rmd"
)

if (!dir.exists("Data")) {
  stop("Folder 'Data' not found. Open the Payco RStudio project or setwd() to the Payco folder.")
}
missing_files <- steps[!file.exists(file.path(script_dir, steps))]
if (length(missing_files) > 0) stop("Missing script(s): ", paste(missing_files, collapse = ", "))
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")

# Run -------------------------------------------------------------------------
run_log <- data.frame(step = character(), status = character(), seconds = numeric())
start_all <- Sys.time()

for (i in seq_along(steps)) {
  step <- steps[i]
  path <- file.path(script_dir, step)
  message(sprintf("\n[%d/%d] %s  (started %s)", i, length(steps), step, format(Sys.time(), "%H:%M:%S")))
  t0 <- Sys.time()

  result <- tryCatch({
    if (grepl("\\.Rmd$", step)) {
      out <- rmarkdown::render(path, output_dir = ".", envir = new.env(), quiet = TRUE,
                               knit_root_dir = getwd())
      message("      Dashboard written to ", normalizePath(out))
    } else {
      source(path, local = new.env(), echo = FALSE)
    }
    "ok"
  }, error = function(e) {
    message("      FAILED: ", conditionMessage(e))
    "failed"
  })

  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  run_log <- rbind(run_log, data.frame(step = step, status = result, seconds = secs))
  message(sprintf("      %s in %.1f s", if (result == "ok") "Done" else "Stopped", secs))

  if (result == "failed") break
}

# Summary ---------------------------------------------------------------------
total <- round(as.numeric(difftime(Sys.time(), start_all, units = "secs")), 1)
message("\nPipeline summary")
print(run_log, row.names = FALSE)
message(sprintf("Total time: %.1f s", total))

dir.create("Output", showWarnings = FALSE)
write.csv(transform(run_log, run_at = format(start_all, "%Y-%m-%d %H:%M:%S")),
          file.path("Output", "08_run_log.csv"), row.names = FALSE)

if (all(run_log$status == "ok") && nrow(run_log) == length(steps)) {
  message("All steps completed. Dashboard: 07_dashboard.html")
  if (open_dashboard) utils::browseURL("07_dashboard.html")
} else {
  stop("Pipeline stopped at ", run_log$step[run_log$status == "failed"], ". See the message above.")
}
