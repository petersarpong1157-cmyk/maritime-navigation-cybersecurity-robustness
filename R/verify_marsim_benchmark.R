# MARSIM held-out benchmark verification
# Run from the repository root.

d <- readRDS('data/marsim_benchmark_final.rds')
frozen <- readRDS('data/marsim_frozen_training_thresholds.rds')
selection <- readRDS('data/marsim_frozen_detector_selection.rds')
reference <- read.csv('results/marsim_heldout_test_results.csv')

stopifnot(nrow(d) == 85680L)
stopifnot(length(unique(d$recording_id)) == 720L)

test <- d[d$partition == 'Test', , drop = FALSE]
test <- test[order(test$recording_id, test$second), ]
stopifnot(nrow(test) == 28560L)

lower <- selection$primary$lower_threshold_m
upper <- selection$primary$upper_threshold_m

raw <- test$disagreement_m < lower |
       test$disagreement_m > upper

persist <- function(alarm, id, k) {
  result <- logical(length(alarm))
  for (ix in split(seq_along(alarm), id)) {
    a <- alarm[ix]
    if (k == 1L) {
      result[ix] <- a
    } else {
      result[ix] <- a & c(FALSE, head(a, -1L))
    }
  }
  result
}

evaluate <- function(k) {
  pred <- persist(raw, test$recording_id, k)
  truth <- test$attack_present == 1L
  TP <- sum(pred & truth)
  TN <- sum(!pred & !truth)
  FP <- sum(pred & !truth)
  FN <- sum(!pred & truth)
  c(TP = TP, TN = TN, FP = FP, FN = FN)
}

actual <- rbind(evaluate(1L), evaluate(2L))
expected <- as.matrix(reference[, c('TP','TN','FP','FN')])

pass <- isTRUE(all.equal(
  unname(actual), unname(expected),
  check.attributes = FALSE
))

cat('Test observations:', nrow(test), '\n')
cat('Confusion counts match reference:', pass, '\n')

if (!pass) stop('MARSIM VERIFICATION: FAILED')
cat('MARSIM VERIFICATION: PASS\n')
