options(stringsAsFactors = FALSE)

# Locate the archive relative to this script when
# run from the paired reproducibility folder.
source('R/maritime_core_functions.R')
inputs <- readRDS('data/paired_experiment_inputs.rds')
reference <- readRDS('data/paired_detector_results.rds')

stopifnot(length(inputs$paired_base_runs) == 24L)

recreated <- list()
index <- 0L

for (i in seq_along(inputs$paired_base_runs)) {
  base_run <- inputs$paired_base_runs[[i]]

  for (condition in names(inputs$paired_noise_settings)) {
    settings <- inputs$paired_noise_settings[[condition]]
    shifted <- apply_sensor_shift(
      base_run,
      settings = settings,
      seed = 940000L + i
    )

    shifted$stress_condition <- condition
    shifted$Pair_ID <- i
    shifted$Original_Run_ID <- shifted$run_id

    shifted$GPS_INS_Disagreement <- sqrt(
      (shifted$GPS_Position_X - shifted$Position_X)^2 +
      (shifted$GPS_Position_Y - shifted$Position_Y)^2
    )
    shifted$Fusion_INS_Disagreement <- sqrt(
      (shifted$Fused_Position_X - shifted$Position_X)^2 +
      (shifted$Fused_Position_Y - shifted$Position_Y)^2
    )
    shifted$Depth_Deviation <- abs(shifted$Depth - 100)
    shifted$Course_Deviation <- abs(
      shifted$Course -
        atan2(shifted$Velocity_Y, shifted$Velocity_X) *
        180 / pi
    )

    index <- index + 1L
    recreated[[index]] <- shifted
  }
}

recreated <- dplyr::bind_rows(recreated)

# Assign a unique run ID to each pair-condition sequence.
recreated$run_id <- as.integer(factor(paste(
  recreated$Pair_ID,
  recreated$stress_condition,
  sep = '_'
)))

noise_thresholds <- inputs$noise_thresholds
noise_feature_names <- inputs$noise_feature_names

recreated <- noise_aware_detector(recreated)
recreated <- apply_persistence(recreated, k = 2)

stopifnot(nrow(recreated) == 36000L)

fields <- c(
  'Pair_ID', 'stress_condition', 'Time',
  'Attack', 'predicted_attack'
)

stopifnot(all(fields %in% names(recreated)))
stopifnot(all(fields %in% names(reference)))

# Compare on stable matching keys, not row ordering.
sort_rows <- function(x) {
  x[order(x$Pair_ID, x$stress_condition, x$Time),
    fields, drop = FALSE]
}

actual <- sort_rows(recreated)
expected <- sort_rows(reference)

pass <- isTRUE(all.equal(
  as.data.frame(actual),
  as.data.frame(expected),
  check.attributes = FALSE,
  tolerance = 1e-8
))

cat('Observations verified:', nrow(recreated), '\n')
cat('Predictions match saved reference:', pass, '\n')

if (!pass) {
  stop('PAIRED REPRODUCTION: FAILED')
}

cat('PAIRED REPRODUCTION: PASS\n')
