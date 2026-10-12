
# ============================================================
# MARITIME OT CYBERSECURITY RESEARCH
# Independent multi-sensor simulation in R
# ============================================================

# Disabled for reproduction: install.packages(c("tidyverse", "caret", "randomForest", "pROC"))

library(tidyverse)
library(caret)
library(randomForest)
library(pROC)


# ============================================================
# 1. DEFINE SCENARIOS
# ============================================================

scenarios <- c(
  "Normal",
  "INS_Attack",
  "Sonar_Attack",
  "Depth_Attack",
  "GPS_Attack",
  "Collision_Attack",
  "Autopilot_Attack",
  "SensorFusion_Attack"
)

# ============================================================
# 2. DEFINE SINGLE SIMULATION RUN
# ============================================================

simulate_navigation <- function(
    scenario,
    run_id,
    seed,
    n_steps = 500,
    attack_intensity = 1
) {
  
  set.seed(seed)
  
  Time <- seq(0, n_steps - 1)
  
  # Baseline vessel motion
  Velocity_X <- rnorm(n_steps, mean = 5, sd = 0.2)
  Velocity_Y <- rnorm(n_steps, mean = 2, sd = 0.2)
  
  true_x <- cumsum(Velocity_X)
  true_y <- cumsum(Velocity_Y)
  
  # Navigation measurements
  Position_X <- true_x + rnorm(n_steps, 0, 0.8)
  Position_Y <- true_y + rnorm(n_steps, 0, 0.8)
  
  GPS_Position_X <- true_x + rnorm(n_steps, 0, 1)
  GPS_Position_Y <- true_y + rnorm(n_steps, 0, 1)
  
  Sonar_Detections <- rpois(n_steps, lambda = 0.1)
  
  Depth <- 100 + rnorm(n_steps, 0, 0.5)
  
  Course <- atan2(Velocity_Y, Velocity_X) * 180 / pi
  
  Evasive_Action <- as.integer(
    Sonar_Detections > 2
  )
  
  Fused_Position_X <- (
    Position_X + GPS_Position_X
  ) / 2
  
  Fused_Position_Y <- (
    Position_Y + GPS_Position_Y
  ) / 2
  
  # Attack begins halfway through the run
  
  # Attack begins at a randomly selected time
  # between 30% and 70% of the simulation
  
  attack_start <- sample(
    seq(
      floor(n_steps * 0.30),
      floor(n_steps * 0.70)
    ),
    size = 1
  )
  
  attack_rows <- attack_start:n_steps
  
  
  if (scenario == "INS_Attack") {
    Position_X[attack_rows] <-
      Position_X[attack_rows] +
      cumsum(rnorm(length(attack_rows),
                   0.4 * attack_intensity, 0.3))
  }
  
  if (scenario == "GPS_Attack") {
    GPS_Position_X[attack_rows] <-
      GPS_Position_X[attack_rows] +
      seq(0, 15 * attack_intensity,
          length.out = length(attack_rows))
  }
  
  if (scenario == "Sonar_Attack") {
    Sonar_Detections[attack_rows] <-
      Sonar_Detections[attack_rows] +
      rpois(length(attack_rows),
            lambda = 2 * attack_intensity)
  }
  
  if (scenario == "Depth_Attack") {
    Depth[attack_rows] <-
      Depth[attack_rows] +
      5 * attack_intensity
  }
  
  if (scenario == "Collision_Attack") {
    Evasive_Action[attack_rows] <-
      rbinom(length(attack_rows), 1, 0.5)
  }
  
  if (scenario == "Autopilot_Attack") {
    Course[attack_rows] <-
      Course[attack_rows] +
      rnorm(length(attack_rows),
            10 * attack_intensity, 3)
  }
  
  if (scenario == "SensorFusion_Attack") {
    Fused_Position_X[attack_rows] <-
      Fused_Position_X[attack_rows] +
      8 * attack_intensity
  }
  
  # Recalculate normal sensor fusion for scenarios
  # without a direct sensor-fusion attack.
  # This is a simplified engineering assumption.
  if (scenario != "SensorFusion_Attack") {
    Fused_Position_X <- (
      Position_X + GPS_Position_X
    ) / 2
    
    Fused_Position_Y <- (
      Position_Y + GPS_Position_Y
    ) / 2
  }
  
  tibble(
    run_id = run_id,
    seed = seed,
    Time = Time,
    
    Position_X = Position_X,
    Position_Y = Position_Y,
    
    Velocity_X = Velocity_X,
    Velocity_Y = Velocity_Y,
    
    Sonar_Detections = Sonar_Detections,
    Depth = Depth,
    
    GPS_Position_X = GPS_Position_X,
    GPS_Position_Y = GPS_Position_Y,
    
    Course = Course,
    Evasive_Action = Evasive_Action,
    
    Fused_Position_X = Fused_Position_X,
    Fused_Position_Y = Fused_Position_Y,
    
    Scenario = scenario,
    
    Attack = as.integer(
      scenario != "Normal" &
        Time >= (attack_start - 1)
    ),
    
    attack_intensity = attack_intensity
  )
}


# ============================================================
# 3. GENERATE PILOT DATASET
# ============================================================

pilot_results <- list()
counter <- 1

for (scenario in scenarios) {
  
  for (run in 1:3) {
    
    simulation_seed <- 10000 + counter
    
    pilot_results[[counter]] <-
      simulate_navigation(
        scenario = scenario,
        run_id = paste0(scenario, "_", run),
        seed = simulation_seed,
        n_steps = 500,
        attack_intensity = 1
      )
    
    counter <- counter + 1
  }
}

pilot_data <- bind_rows(pilot_results)

# ============================================================
# 4. VERIFY PILOT
# ============================================================

cat("Total observations:", nrow(pilot_data), "\n")
cat("Independent runs:",
    n_distinct(pilot_data$run_id), "\n")

pilot_data %>%
  group_by(Scenario) %>%
  summarise(
    runs = n_distinct(run_id),
    observations = n(),
    attack_observations = sum(Attack),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ============================================================
# 5. SAVE PILOT
# ============================================================

write_csv(
  pilot_data,
  "maritime_navigation_pilot.csv"
)

cat("Pilot dataset saved successfully.\n")


# =====================================================
# STEP 4: PILOT DATA QUALITY CHECKS
# =====================================================

library(dplyr)
library(ggplot2)

# 1. Missing values
cat("Total missing values:",
    sum(is.na(pilot_data)), "\n")

# 2. Duplicated records
cat("Fully duplicated rows:",
    sum(duplicated(pilot_data)), "\n")

# 3. Confirm each run contains 500 observations
pilot_data %>%
  count(Scenario, run_id, name = "n_steps") %>%
  summarise(
    min_steps = min(n_steps),
    max_steps = max(n_steps)
  ) %>%
  print()

# 4. Check attack timing
pilot_data %>%
  group_by(Scenario) %>%
  summarise(
    first_attack_time = if (any(Attack == 1)) {
      min(Time[Attack == 1])
    } else {
      NA_real_
    },
    attack_rows = sum(Attack),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# 5. Cross-sensor position disagreement
pilot_data <- pilot_data %>%
  mutate(
    GPS_INS_Disagreement = sqrt(
      (GPS_Position_X - Position_X)^2 +
        (GPS_Position_Y - Position_Y)^2
    ),
    
    Fusion_INS_Disagreement = sqrt(
      (Fused_Position_X - Position_X)^2 +
        (Fused_Position_Y - Position_Y)^2
    )
  )

# 6. Summarise disagreement by scenario
pilot_data %>%
  group_by(Scenario, Attack) %>%
  summarise(
    mean_GPS_INS = mean(GPS_INS_Disagreement),
    sd_GPS_INS = sd(GPS_INS_Disagreement),
    mean_Fusion_INS = mean(Fusion_INS_Disagreement),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# 7. Visualise cross-sensor disagreement
ggplot(
  pilot_data,
  aes(x = Time,
      y = GPS_INS_Disagreement,
      colour = factor(Attack))
) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Cross-Sensor Position Disagreement",
    subtitle = "Three pilot runs per scenario",
    x = "Simulation time (seconds)",
    y = "GPS–INS position disagreement",
    colour = "Attack status"
  ) +
  theme_minimal()



# Generate five independent GPS attack runs

test_runs <- lapply(1:5, function(i) {
  
  simulate_navigation(
    scenario = "GPS_Attack",
    run_id = paste0("GPS_Test_", i),
    seed = 2000 + i,
    n_steps = 500,
    attack_intensity = 1
  )
  
})

test_data <- dplyr::bind_rows(test_runs)

# Check the attack starting times

test_data %>%
  dplyr::filter(Attack == 1) %>%
  dplyr::group_by(run_id) %>%
  dplyr::summarise(
    first_attack_time = min(Time),
    .groups = "drop"
  ) %>%
  print()


# ============================================================
# STEP 5: GENERATE FULL EXPERIMENTAL DATASET
# ============================================================

library(dplyr)
library(readr)

scenarios <- c(
  "Normal",
  "INS_Attack",
  "Sonar_Attack",
  "Depth_Attack",
  "GPS_Attack",
  "Collision_Attack",
  "Autopilot_Attack",
  "SensorFusion_Attack"
)

# Proposed simulation intensities
intensity_levels <- c(0.5, 1.0, 1.5)

full_results <- vector("list", 240)

counter <- 1

for (scenario in scenarios) {
  
  for (run in 1:30) {
    
    # Ten independent runs at each intensity
    intensity <- intensity_levels[
      ceiling(run / 10)
    ]
    
    # Unique reproducible seed
    simulation_seed <- 30000 + counter
    
    full_results[[counter]] <-
      simulate_navigation(
        scenario = scenario,
        run_id = paste0(
          scenario, "_",
          sprintf("%03d", run)
        ),
        seed = simulation_seed,
        n_steps = 500,
        attack_intensity = intensity
      )
    
    counter <- counter + 1
  }
}

full_data <- bind_rows(full_results)

# ============================================================
# VALIDATE DATASET STRUCTURE
# ============================================================

cat("Total rows:", nrow(full_data), "\n")

cat(
  "Independent runs:",
  n_distinct(full_data$run_id),
  "\n"
)

cat(
  "Missing values:",
  sum(is.na(full_data)),
  "\n"
)

full_data %>%
  group_by(Scenario, attack_intensity) %>%
  summarise(
    runs = n_distinct(run_id),
    observations = n(),
    attack_rows = sum(Attack),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ============================================================
# SAVE DATA
# ============================================================

write_csv(
  full_data,
  "maritime_navigation_full_240_runs.csv"
)

cat("Full experimental dataset saved.\n")


# =====================================================
# STEP 6: RUN-LEVEL TRAIN / VALIDATION / TEST SPLIT
# =====================================================

library(dplyr)
library(readr)

# Confirm that each run has one scenario and intensity
run_check <- full_data %>%
  group_by(run_id) %>%
  summarise(
    scenarios = n_distinct(Scenario),
    intensities = n_distinct(attack_intensity),
    .groups = "drop"
  )

stopifnot(
  all(run_check$scenarios == 1),
  all(run_check$intensities == 1)
)

# Construct a table with one row per simulation run
run_table <- full_data %>%
  distinct(run_id, Scenario, attack_intensity) %>%
  arrange(Scenario, attack_intensity, run_id)

set.seed(20261012)

run_table <- run_table %>%
  group_by(Scenario, attack_intensity) %>%
  mutate(
    random_order = sample.int(n()),
    data_split = case_when(
      random_order <= 6 ~ "Train",
      random_order <= 8 ~ "Validation",
      TRUE ~ "Test"
    )
  ) %>%
  ungroup() %>%
  select(-random_order)

# Attach the run-level split to every observation
experiment_data <- full_data %>%
  left_join(
    run_table,
    by = c("run_id", "Scenario", "attack_intensity")
  )

# Confirm no run belongs to multiple splits
stopifnot(
  all(
    experiment_data %>%
      group_by(run_id) %>%
      summarise(n_splits = n_distinct(data_split),
                .groups = "drop") %>%
      pull(n_splits) == 1
  )
)

# Check split sizes
experiment_data %>%
  group_by(data_split) %>%
  summarise(
    runs = n_distinct(run_id),
    observations = n(),
    attack_rows = sum(Attack),
    .groups = "drop"
  ) %>%
  print()

# Save the experiment with its assigned splits
write_csv(
  experiment_data,
  "maritime_navigation_run_level_splits.csv"
)

write_csv(
  run_table,
  "maritime_navigation_run_assignments.csv"
)

cat("Run-level data split completed.\n")


# ==========================================================
# STEP 7: CROSS-SENSOR BASELINE DETECTOR
# ==========================================================

library(dplyr)
library(readr)

# 1. Calculate operational cross-sensor features
experiment_data <- experiment_data %>%
  mutate(
    GPS_INS_Disagreement = sqrt(
      (GPS_Position_X - Position_X)^2 +
        (GPS_Position_Y - Position_Y)^2
    ),
    
    Fusion_INS_Disagreement = sqrt(
      (Fused_Position_X - Position_X)^2 +
        (Fused_Position_Y - Position_Y)^2
    )
  )

# 2. Separate dataset partitions
train_data <- experiment_data %>%
  filter(data_split == "Train")

validation_data <- experiment_data %>%
  filter(data_split == "Validation")

test_data <- experiment_data %>%
  filter(data_split == "Test")

# 3. Estimate thresholds from training observations
#    labeled normal (Attack == 0).

normal_train <- train_data %>%
  filter(Attack == 0)

gps_threshold <- quantile(
  normal_train$GPS_INS_Disagreement,
  probs = 0.99,
  na.rm = TRUE
)

fusion_threshold <- quantile(
  normal_train$Fusion_INS_Disagreement,
  probs = 0.99,
  na.rm = TRUE
)

cat("GPS-INS threshold:", gps_threshold, "\n")
cat("Fusion-INS threshold:", fusion_threshold, "\n")

# 4. Define baseline decision rule
baseline_detector <- function(data) {
  
  data %>%
    mutate(
      predicted_attack = as.integer(
        GPS_INS_Disagreement > gps_threshold |
          Fusion_INS_Disagreement > fusion_threshold
      )
    )
}

# 5. Apply baseline without changing thresholds
validation_predictions <- baseline_detector(
  validation_data
)

test_predictions <- baseline_detector(
  test_data
)

# 6. Define classification metrics
calculate_metrics <- function(data) {
  
  TP <- sum(data$Attack == 1 &
              data$predicted_attack == 1)
  
  TN <- sum(data$Attack == 0 &
              data$predicted_attack == 0)
  
  FP <- sum(data$Attack == 0 &
              data$predicted_attack == 1)
  
  FN <- sum(data$Attack == 1 &
              data$predicted_attack == 0)
  
  precision <- if ((TP + FP) > 0) {
    TP / (TP + FP)
  } else {
    NA_real_
  }
  
  recall <- if ((TP + FN) > 0) {
    TP / (TP + FN)
  } else {
    NA_real_
  }
  
  specificity <- if ((TN + FP) > 0) {
    TN / (TN + FP)
  } else {
    NA_real_
  }
  
  f1 <- if (!is.na(precision) &&
            !is.na(recall) &&
            (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  
  tibble(
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    Specificity = specificity,
    False_Positive_Rate = if ((FP + TN) > 0) {
      FP / (FP + TN)
    } else {
      NA_real_
    }
  )
}

# 7. Validation performance
cat("\nVALIDATION PERFORMANCE\n")
validation_metrics <- calculate_metrics(
  validation_predictions
)
print(validation_metrics)

# 8. Test performance
cat("\nTEST PERFORMANCE\n")
test_metrics <- calculate_metrics(
  test_predictions
)
print(test_metrics)

# 9. Performance across attack scenarios
scenario_metrics <- test_predictions %>%
  filter(Attack == 1) %>%
  group_by(Scenario) %>%
  summarise(
    Attack_Observations = n(),
    Detected_Attacks = sum(predicted_attack == 1),
    Detection_Recall = mean(predicted_attack == 1),
    .groups = "drop"
  )

cat("\nTEST RECALL BY ATTACK SCENARIO\n")
print(scenario_metrics, n = Inf)

# 10. Save reproducible evaluation outputs
write_csv(
  validation_metrics,
  "baseline_validation_metrics.csv"
)

write_csv(
  test_metrics,
  "baseline_test_metrics.csv"
)

write_csv(
  scenario_metrics,
  "baseline_test_scenario_recall.csv"
)

cat("\nBaseline evaluation completed.\n")


# ======================================================
# STEP 8: MULTI-SENSOR ATTACK SIGNATURE DIAGNOSTICS
# ======================================================

library(dplyr)
library(readr)

# Work only with the held-out test runs.
# This is exploratory diagnosis, not model training.
sensor_diagnostics <- test_data %>%
  group_by(Scenario, Attack) %>%
  summarise(
    Mean_Sonar = mean(Sonar_Detections),
    SD_Depth = sd(Depth),
    Mean_Depth = mean(Depth),
    SD_Course = sd(Course),
    Mean_Course = mean(Course),
    Evasive_Action_Rate = mean(Evasive_Action),
    .groups = "drop"
  )

print(sensor_diagnostics, n = Inf)

write_csv(
  sensor_diagnostics,
  "sensor_attack_diagnostics.csv"
)

# Compare before and after attack for each scenario
attack_changes <- sensor_diagnostics %>%
  filter(Scenario != "Normal") %>%
  select(
    Scenario, Attack,
    Mean_Sonar, Mean_Depth, Mean_Course,
    Evasive_Action_Rate
  ) %>%
  tidyr::pivot_wider(
    names_from = Attack,
    values_from = c(
      Mean_Sonar, Mean_Depth, Mean_Course,
      Evasive_Action_Rate
    ),
    names_sep = "_"
  ) %>%
  mutate(
    Sonar_Change = Mean_Sonar_1 - Mean_Sonar_0,
    Depth_Change = Mean_Depth_1 - Mean_Depth_0,
    Course_Change = Mean_Course_1 - Mean_Course_0,
    Evasive_Rate_Change =
      Evasive_Action_Rate_1 -
      Evasive_Action_Rate_0
  ) %>%
  select(
    Scenario,
    Sonar_Change,
    Depth_Change,
    Course_Change,
    Evasive_Rate_Change
  )

cat("\nATTACK-RELATED SENSOR CHANGES\n")
print(attack_changes, n = Inf)

write_csv(
  attack_changes,
  "attack_sensor_changes.csv"
)




# ==========================================================
# STEP 9: MULTI-SENSOR BASELINE DETECTOR
# ==========================================================

library(dplyr)
library(readr)

# Add detection features to all partitions.
# Nominal depth and course correspond to the current
# simplified R simulator, not universal vessel limits.

add_sensor_features <- function(data) {
  data %>%
    mutate(
      GPS_INS_Disagreement = sqrt(
        (GPS_Position_X - Position_X)^2 +
          (GPS_Position_Y - Position_Y)^2
      ),
      
      Fusion_INS_Disagreement = sqrt(
        (Fused_Position_X - Position_X)^2 +
          (Fused_Position_Y - Position_Y)^2
      ),
      
      Depth_Deviation = abs(Depth - 100),
      
      Nominal_Course = atan2(
        Velocity_Y, Velocity_X
      ) * 180 / pi,
      
      # Circular difference in degrees
      Course_Deviation = abs(
        (Course - Nominal_Course + 180) %% 360 - 180
      )
    )
}

train_ms <- add_sensor_features(
  experiment_data %>% filter(data_split == "Train")
)

validation_ms <- add_sensor_features(
  experiment_data %>% filter(data_split == "Validation")
)

test_ms <- add_sensor_features(
  experiment_data %>% filter(data_split == "Test")
)

# Calibrate thresholds using trusted-normal
# training observations only.
normal_ms <- train_ms %>% filter(Attack == 0)

feature_names <- c(
  "GPS_INS_Disagreement",
  "Fusion_INS_Disagreement",
  "Sonar_Detections",
  "Depth_Deviation",
  "Course_Deviation"
)

thresholds <- sapply(
  normal_ms[feature_names],
  function(x) as.numeric(quantile(
    x, probs = 0.99, na.rm = TRUE
  ))
)

print(thresholds)

# Flag observations crossing any threshold or
# reporting an evasive action.
multi_sensor_detector <- function(data) {
  
  alarm_matrix <- sapply(
    feature_names,
    function(feature) {
      data[[feature]] > thresholds[[feature]]
    }
  )
  
  data$predicted_attack <- as.integer(
    rowSums(alarm_matrix) > 0 |
      data$Evasive_Action == 1
  )
  
  data
}

validation_ms_pred <- multi_sensor_detector(validation_ms)
test_ms_pred <- multi_sensor_detector(test_ms)

cat("\nMULTI-SENSOR VALIDATION METRICS\n")
print(calculate_metrics(validation_ms_pred))

cat("\nMULTI-SENSOR TEST METRICS\n")
print(calculate_metrics(test_ms_pred))

cat("\nTEST RECALL BY ATTACK TYPE\n")

ms_scenario_recall <- test_ms_pred %>%
  filter(Attack == 1) %>%
  group_by(Scenario) %>%
  summarise(
    Attack_Observations = n(),
    Detected = sum(predicted_attack),
    Recall = mean(predicted_attack),
    .groups = "drop"
  )

print(ms_scenario_recall, n = Inf)

write_csv(
  calculate_metrics(validation_ms_pred),
  "multisensor_validation_metrics.csv"
)

write_csv(
  calculate_metrics(test_ms_pred),
  "multisensor_test_metrics.csv"
)

write_csv(
  ms_scenario_recall,
  "multisensor_test_scenario_recall.csv"
)



# =====================================================
# STEP 10: DETECTOR RULE DIAGNOSTICS
# =====================================================

library(dplyr)
library(tidyr)
library(readr)

# Use the existing test predictions.
# No model retraining or threshold adjustment.

rule_diagnostics <- test_ms_pred %>%
  mutate(
    GPS_INS_Rule =
      GPS_INS_Disagreement >
      thresholds[["GPS_INS_Disagreement"]],
    
    Fusion_INS_Rule =
      Fusion_INS_Disagreement >
      thresholds[["Fusion_INS_Disagreement"]],
    
    Sonar_Rule =
      Sonar_Detections >
      thresholds[["Sonar_Detections"]],
    
    Depth_Rule =
      Depth_Deviation >
      thresholds[["Depth_Deviation"]],
    
    Course_Rule =
      Course_Deviation >
      thresholds[["Course_Deviation"]],
    
    Evasive_Rule = Evasive_Action == 1
  )

# Count rule activations during each attack type
rule_summary <- rule_diagnostics %>%
  filter(Attack == 1) %>%
  group_by(Scenario) %>%
  summarise(
    GPS_INS = mean(GPS_INS_Rule),
    Fusion_INS = mean(Fusion_INS_Rule),
    Sonar = mean(Sonar_Rule),
    Depth = mean(Depth_Rule),
    Course = mean(Course_Rule),
    Evasive = mean(Evasive_Rule),
    Overall_Recall = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nRULE ACTIVATION BY ATTACK TYPE\n")
print(rule_summary, n = Inf)

# False alarms by individual rule
false_alarm_rules <- rule_diagnostics %>%
  filter(Attack == 0) %>%
  summarise(
    GPS_INS = mean(GPS_INS_Rule),
    Fusion_INS = mean(Fusion_INS_Rule),
    Sonar = mean(Sonar_Rule),
    Depth = mean(Depth_Rule),
    Course = mean(Course_Rule),
    Evasive = mean(Evasive_Rule),
    Combined_FPR = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nNORMAL OBSERVATION RULE ACTIVATION\n")
print(false_alarm_rules)

write_csv(
  rule_summary,
  "detector_rule_diagnostics.csv"
)

write_csv(
  false_alarm_rules,
  "detector_false_alarm_rules.csv"
)



# =====================================================
# STEP 11: RUN-LEVEL FALSE ALARM DIAGNOSTICS
# =====================================================

library(dplyr)
library(readr)

# Evaluate only periods labeled as normal
normal_periods <- test_ms_pred %>%
  filter(Attack == 0)

run_false_alarms <- normal_periods %>%
  group_by(run_id, Scenario) %>%
  summarise(
    normal_observations = n(),
    false_alarm_count = sum(predicted_attack == 1),
    false_alarm_rate = mean(predicted_attack == 1),
    any_false_alarm = as.integer(
      any(predicted_attack == 1)
    ),
    .groups = "drop"
  )

cat("\nRUN-LEVEL FALSE ALARM SUMMARY\n")

run_false_alarms %>%
  summarise(
    normal_segments = n(),
    segments_with_false_alarm =
      sum(any_false_alarm),
    proportion_with_false_alarm =
      mean(any_false_alarm),
    mean_false_alarms_per_segment =
      mean(false_alarm_count),
    median_false_alarms_per_segment =
      median(false_alarm_count)
  ) %>%
  print()

cat("\nFALSE ALARMS BY SCENARIO\n")

run_false_alarms %>%
  group_by(Scenario) %>%
  summarise(
    segments = n(),
    mean_false_alarm_rate =
      mean(false_alarm_rate),
    segments_with_any_alarm =
      sum(any_false_alarm),
    .groups = "drop"
  ) %>%
  print(n = Inf)

write_csv(
  run_false_alarms,
  "run_level_false_alarm_diagnostics.csv"
)


# ==========================================================
# STEP 12: FALSE-ALARM EPISODE ANALYSIS
# ==========================================================

library(dplyr)
library(readr)

# Work only with normal-labeled observations
# in the held-out test runs.

false_alarm_episodes <- test_ms_pred %>%
  filter(Attack == 0) %>%
  arrange(run_id, Time) %>%
  group_by(run_id, Scenario) %>%
  mutate(
    previous_alarm = lag(
      predicted_attack,
      default = 0
    ),
    
    new_alarm_episode = as.integer(
      predicted_attack == 1 &
        previous_alarm == 0
    )
  ) %>%
  summarise(
    normal_observations = n(),
    false_positive_samples = sum(predicted_attack),
    alarm_episodes = sum(new_alarm_episode),
    .groups = "drop"
  )

cat("\nFALSE ALARM EPISODE SUMMARY\n")

false_alarm_episodes %>%
  summarise(
    runs = n(),
    mean_episodes = mean(alarm_episodes),
    median_episodes = median(alarm_episodes),
    minimum_episodes = min(alarm_episodes),
    maximum_episodes = max(alarm_episodes),
    runs_with_any_episode =
      sum(alarm_episodes > 0)
  ) %>%
  print()

cat("\nFALSE ALARM EPISODES BY SCENARIO\n")

false_alarm_episodes %>%
  group_by(Scenario) %>%
  summarise(
    runs = n(),
    mean_episodes = mean(alarm_episodes),
    median_episodes = median(alarm_episodes),
    .groups = "drop"
  ) %>%
  print(n = Inf)

write_csv(
  false_alarm_episodes,
  "false_alarm_episode_analysis.csv"
)


# ==========================================================
# STEP 13: ALARM PERSISTENCE EXPERIMENT
# ==========================================================

library(dplyr)
library(tidyr)
library(readr)

# Apply a consecutive-observation alarm rule.
# No future observations are used.

apply_persistence <- function(data, k = 1) {
  
  data %>%
    arrange(run_id, Time) %>%
    group_by(run_id) %>%
    mutate(
      raw_alarm = as.integer(predicted_attack),
      
      consecutive_count = {
        x <- raw_alarm
        counts <- integer(length(x))
        streak <- 0L
        
        for (i in seq_along(x)) {
          if (x[i] == 1L) {
            streak <- streak + 1L
          } else {
            streak <- 0L
          }
          counts[i] <- streak
        }
        
        counts
      },
      
      predicted_attack = as.integer(
        consecutive_count >= k
      )
    ) %>%
    ungroup()
}

# Count false-alarm episodes during normal periods.
# Counts are computed on full sequences, then restricted
# to Attack == 0 so the temporal ordering is preserved.

evaluate_persistence <- function(data, k) {
  
  evaluated <- apply_persistence(data, k)
  
  episodes <- evaluated %>%
    arrange(run_id, Time) %>%
    group_by(run_id) %>%
    mutate(
      previous_alarm = lag(predicted_attack, default = 0L),
      new_episode = as.integer(
        predicted_attack == 1L &
          previous_alarm == 0L
      )
    ) %>%
    ungroup()
  
  normal_stats <- episodes %>%
    filter(Attack == 0) %>%
    group_by(run_id) %>%
    summarise(
      normal_seconds = n(),
      false_alarm_episodes = sum(new_episode),
      .groups = "drop"
    )
  
  metrics <- calculate_metrics(evaluated)
  
  tibble(
    k = k,
    Precision = metrics$Precision,
    Recall = metrics$Recall,
    F1 = metrics$F1,
    False_Positive_Rate = metrics$False_Positive_Rate,
    Mean_False_Alarm_Episodes =
      mean(normal_stats$false_alarm_episodes),
    Episodes_Per_100_Normal_Seconds =
      100 * sum(normal_stats$false_alarm_episodes) /
      sum(normal_stats$normal_seconds)
  )
}

# Compare persistence rules using VALIDATION data only.
validation_persistence <- bind_rows(
  lapply(1:3, function(k) {
    evaluate_persistence(validation_ms_pred, k)
  })
)

cat("\nVALIDATION: ALARM PERSISTENCE COMPARISON\n")
print(validation_persistence, n = Inf)

write_csv(
  validation_persistence,
  "validation_alarm_persistence_comparison.csv"
)


# ==========================================================
# STEP 14: FINAL TEST — PERSISTENCE k = 2
# ==========================================================

library(dplyr)
library(readr)

selected_k <- 2L

# Apply the already selected rule to held-out test runs
final_test <- apply_persistence(
  test_ms_pred,
  k = selected_k
)

# 1. Overall test performance
final_metrics <- calculate_metrics(final_test)

cat("\nFINAL TEST METRICS — k = 2\n")
print(final_metrics)

# 2. Recall by attack type
final_scenario_recall <- final_test %>%
  filter(Attack == 1) %>%
  group_by(Scenario) %>%
  summarise(
    Attack_Observations = n(),
    Detected_Observations = sum(predicted_attack),
    Recall = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nTEST RECALL BY ATTACK TYPE\n")
print(final_scenario_recall, n = Inf)

# 3. Attack detection by simulation run
# Only attack-designated runs are included.
run_detection <- final_test %>%
  filter(Scenario != "Normal") %>%
  group_by(run_id, Scenario) %>%
  summarise(
    onset_time = min(Time[Attack == 1]),
    first_post_onset_alarm = if (
      any(Attack == 1 & predicted_attack == 1)
    ) {
      min(Time[Attack == 1 & predicted_attack == 1])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    detected = !is.na(first_post_onset_alarm),
    detection_delay = first_post_onset_alarm - onset_time
  )

cat("\nRUN-LEVEL ATTACK DETECTION\n")

run_detection %>%
  group_by(Scenario) %>%
  summarise(
    Runs = n(),
    Detected_Runs = sum(detected),
    Detection_Rate = mean(detected),
    Median_Delay_Seconds = if (any(detected)) {
      median(detection_delay[detected])
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  print(n = Inf)

# 4. Save final test outputs
write_csv(
  final_metrics,
  "final_test_persistence_metrics.csv"
)

write_csv(
  final_scenario_recall,
  "final_test_persistence_scenario_recall.csv"
)

write_csv(
  run_detection,
  "final_test_run_detection_delay.csv"
)

cat("\nFinal test evaluation completed.\n")


# =====================================================
# STEP 15: RUN-LEVEL BOOTSTRAP UNCERTAINTY
# =====================================================

library(dplyr)
library(readr)

# Existing held-out test predictions
# k = 1: test_ms_pred
# k = 2: final_test

count_by_run <- function(data, suffix) {
  data %>%
    group_by(run_id) %>%
    summarise(
      TP = sum(Attack == 1 & predicted_attack == 1),
      TN = sum(Attack == 0 & predicted_attack == 0),
      FP = sum(Attack == 0 & predicted_attack == 1),
      FN = sum(Attack == 1 & predicted_attack == 0),
      .groups = "drop"
    ) %>%
    rename_with(
      ~ paste0(.x, "_", suffix),
      -run_id
    )
}

run_counts <- inner_join(
  count_by_run(test_ms_pred, "k1"),
  count_by_run(final_test, "k2"),
  by = "run_id"
)

score_counts <- function(x, suffix) {
  tp <- sum(x[[paste0("TP_", suffix)]])
  tn <- sum(x[[paste0("TN_", suffix)]])
  fp <- sum(x[[paste0("FP_", suffix)]])
  fn <- sum(x[[paste0("FN_", suffix)]])
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  f1 <- 2 * precision * recall / (precision + recall)
  
  c(Recall = recall, FPR = fpr, F1 = f1)
}

set.seed(20261012)
B <- 2000

bootstrap_differences <- replicate(B, {
  sampled <- run_counts[
    sample(
      seq_len(nrow(run_counts)),
      size = nrow(run_counts),
      replace = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  score_counts(sampled, "k2") -
    score_counts(sampled, "k1")
})

ci_results <- data.frame(
  Metric = rownames(bootstrap_differences),
  Difference = as.numeric(
    score_counts(run_counts, "k2") -
      score_counts(run_counts, "k1")
  ),
  Lower_95 = apply(
    bootstrap_differences, 1, quantile,
    probs = 0.025, na.rm = TRUE
  ),
  Upper_95 = apply(
    bootstrap_differences, 1, quantile,
    probs = 0.975, na.rm = TRUE
  )
)

print(ci_results)

write_csv(
  ci_results,
  "run_bootstrap_persistence_comparison.csv"
)


# ============================================================
# STEP 16A: INDEPENDENT SENSOR-SHIFT STRESS TEST
# ============================================================

library(dplyr)
library(readr)

# Define two new operating conditions

stress_conditions <- data.frame(
  condition = c("Mild_Shift", "Strong_Shift"),
  position_noise = c(0.5, 1.5),
  depth_noise = c(0.3, 1.0),
  course_noise = c(0.3, 1.5),
  depth_bias_sd = c(0.5, 2.0)
)

# Apply additional measurement disturbances to a
# complete simulation run.
#
# Preserve the simulator's existing fusion residual,
# so injected sensor-fusion disturbances are not erased.

apply_sensor_shift <- function(data, settings, seed) {
  
  set.seed(seed)
  
  n <- nrow(data)
  
  original_fusion_residual_x <-
    data$Fused_Position_X -
    (data$Position_X + data$GPS_Position_X) / 2
  
  original_fusion_residual_y <-
    data$Fused_Position_Y -
    (data$Position_Y + data$GPS_Position_Y) / 2
  
  # Small run-specific nominal depth variation
  depth_bias <- rnorm(
    1, mean = 0,
    sd = settings$depth_bias_sd
  )
  
  data$Position_X <- data$Position_X +
    rnorm(n, 0, settings$position_noise)
  
  data$Position_Y <- data$Position_Y +
    rnorm(n, 0, settings$position_noise)
  
  data$GPS_Position_X <- data$GPS_Position_X +
    rnorm(n, 0, settings$position_noise)
  
  data$GPS_Position_Y <- data$GPS_Position_Y +
    rnorm(n, 0, settings$position_noise)
  
  data$Depth <- data$Depth +
    depth_bias +
    rnorm(n, 0, settings$depth_noise)
  
  data$Course <- data$Course +
    rnorm(n, 0, settings$course_noise)
  
  # Reconstruct fusion measurements while preserving
  # any pre-existing fusion-specific residual.
  data$Fused_Position_X <-
    (data$Position_X + data$GPS_Position_X) / 2 +
    original_fusion_residual_x
  
  data$Fused_Position_Y <-
    (data$Position_Y + data$GPS_Position_Y) / 2 +
    original_fusion_residual_y
  
  data$stress_condition <- settings$condition
  
  data
}

# ============================================================
# GENERATE INDEPENDENT RUNS
# ============================================================

robustness_results <- list()
counter <- 1

for (condition_index in 1:nrow(stress_conditions)) {
  
  settings <- stress_conditions[
    condition_index, , drop = FALSE
  ]
  
  for (scenario in scenarios) {
    
    for (run in 1:3) {
      
      sim_seed <- 90000 + counter * 10
      
      sim_data <- simulate_navigation(
        scenario = scenario,
        run_id = paste0(
          settings$condition, "_",
          scenario, "_", run
        ),
        seed = sim_seed,
        n_steps = 500,
        attack_intensity = 1
      )
      
      robustness_results[[counter]] <-
        apply_sensor_shift(
          data = sim_data,
          settings = settings,
          seed = sim_seed + 1
        )
      
      counter <- counter + 1
    }
  }
}

robustness_data <- bind_rows(robustness_results)

cat(
  "New runs:",
  n_distinct(robustness_data$run_id), "\n"
)

cat(
  "Total observations:",
  nrow(robustness_data), "\n"
)

cat(
  "Missing values:",
  sum(is.na(robustness_data)), "\n"
)

write_csv(
  robustness_data,
  "maritime_independent_robustness_data.csv"
)


# ============================================================
# STEP 16B: TEST ORIGINAL DETECTOR ON NEW CONDITIONS
# ============================================================

library(dplyr)
library(readr)

# 1. Create features using the ORIGINAL feature rules
robustness_features <- add_sensor_features(
  robustness_data
)

# 2. Apply the ORIGINAL training-calibrated thresholds
robustness_raw <- multi_sensor_detector(
  robustness_features
)

# 3. Apply the previously selected persistence rule
robustness_predictions <- apply_persistence(
  robustness_raw,
  k = 2L
)

# 4. Calculate metrics separately for each new condition
robustness_metrics <- robustness_predictions %>%
  group_by(stress_condition) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

cat("\nINDEPENDENT ROBUSTNESS RESULTS\n")
print(robustness_metrics, width = Inf)

# 5. Report attack recall by scenario
robustness_attack_recall <- robustness_predictions %>%
  filter(Attack == 1) %>%
  group_by(stress_condition, Scenario) %>%
  summarise(
    attack_observations = n(),
    detected_observations = sum(predicted_attack),
    recall = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nATTACK RECALL UNDER SENSOR SHIFT\n")
print(robustness_attack_recall, n = Inf)

# 6. Save outputs
write_csv(
  robustness_metrics,
  "independent_robustness_metrics.csv"
)

write_csv(
  robustness_attack_recall,
  "independent_robustness_attack_recall.csv"
)


# ===========================================================
# STEP 17: DIAGNOSE ROBUSTNESS FAILURE
# ===========================================================

library(dplyr)
library(readr)

robustness_rule_diagnostics <- robustness_features %>%
  mutate(
    GPS_INS_Rule = as.integer(
      GPS_INS_Disagreement >
        thresholds[["GPS_INS_Disagreement"]]
    ),
    
    Fusion_INS_Rule = as.integer(
      Fusion_INS_Disagreement >
        thresholds[["Fusion_INS_Disagreement"]]
    ),
    
    Sonar_Rule = as.integer(
      Sonar_Detections >
        thresholds[["Sonar_Detections"]]
    ),
    
    Depth_Rule = as.integer(
      Depth_Deviation >
        thresholds[["Depth_Deviation"]]
    ),
    
    Course_Rule = as.integer(
      Course_Deviation >
        thresholds[["Course_Deviation"]]
    ),
    
    Evasive_Rule = as.integer(
      Evasive_Action == 1
    )
  )

rule_failure_summary <- robustness_rule_diagnostics %>%
  filter(Attack == 0) %>%
  group_by(stress_condition) %>%
  summarise(
    GPS_INS_Rate = mean(GPS_INS_Rule),
    Fusion_INS_Rate = mean(Fusion_INS_Rule),
    Sonar_Rate = mean(Sonar_Rule),
    Depth_Rate = mean(Depth_Rule),
    Course_Rate = mean(Course_Rule),
    Evasive_Rate = mean(Evasive_Rule),
    .groups = "drop"
  )

cat("\nNORMAL-PERIOD RULE ACTIVATIONS UNDER SHIFT\n")
print(rule_failure_summary, width = Inf)

write_csv(
  rule_failure_summary,
  "robustness_rule_failure_diagnostics.csv"
)


# ============================================================
# STEP 18A: NEW NOISE-AWARE EXPERIMENTAL DATASET
# ============================================================

library(dplyr)
library(readr)

set.seed(20261013)

noise_conditions <- data.frame(
  condition = c("Low_Noise", "Moderate_Noise", "High_Noise"),
  position_noise = c(0.2, 0.5, 1.0),
  depth_noise = c(0.1, 0.3, 0.7),
  course_noise = c(0.1, 0.3, 0.8),
  depth_bias_sd = c(0.2, 0.5, 1.0)
)

new_results <- vector("list", 240)

counter <- 1L

for (scenario in scenarios) {
  
  for (run in 1:30) {
    
    condition_index <- ((run - 1) %% 3) + 1
    
    settings <- noise_conditions[
      condition_index, , drop = FALSE
    ]
    
    sim_seed <- 150000 + counter * 10
    
    sim_data <- simulate_navigation(
      scenario = scenario,
      run_id = paste0(
        "NEW_", scenario, "_",
        sprintf("%03d", run)
      ),
      seed = sim_seed,
      n_steps = 500,
      attack_intensity = 1
    )
    
    new_results[[counter]] <-
      apply_sensor_shift(
        data = sim_data,
        settings = settings,
        seed = sim_seed + 1
      )
    
    counter <- counter + 1L
  }
}

noise_aware_data <- bind_rows(new_results)

# Each scenario has 10 runs per noise condition.
# Assign 6 train, 2 validation, 2 test.

run_assignments <- noise_aware_data %>%
  distinct(
    run_id,
    Scenario,
    stress_condition
  ) %>%
  group_by(Scenario, stress_condition) %>%
  mutate(
    shuffled = sample.int(n()),
    data_split = case_when(
      shuffled <= 6 ~ "Train",
      shuffled <= 8 ~ "Validation",
      TRUE ~ "Test"
    )
  ) %>%
  ungroup() %>%
  select(-shuffled)

noise_aware_data <- noise_aware_data %>%
  left_join(
    run_assignments,
    by = c(
      "run_id",
      "Scenario",
      "stress_condition"
    )
  )

# Verify dataset
cat(
  "New runs:",
  n_distinct(noise_aware_data$run_id), "\n"
)

cat(
  "Observations:",
  nrow(noise_aware_data), "\n"
)

noise_aware_data %>%
  group_by(data_split) %>%
  summarise(
    runs = n_distinct(run_id),
    observations = n(),
    .groups = "drop"
  ) %>%
  print()

write_csv(
  noise_aware_data,
  "maritime_noise_aware_experiment.csv"
)

write_csv(
  run_assignments,
  "maritime_noise_aware_run_assignments.csv"
)


# ======================================================
# STEP 18B: NOISE-AWARE DETECTOR CALIBRATION
# ======================================================

library(dplyr)
library(readr)

# Reuse the feature function from Step 9
noise_aware_features <- add_sensor_features(
  noise_aware_data
)

# Partition complete simulation runs
noise_train <- noise_aware_features %>%
  filter(data_split == "Train")

noise_validation <- noise_aware_features %>%
  filter(data_split == "Validation")

noise_test <- noise_aware_features %>%
  filter(data_split == "Test")

# Normal training observations only
normal_noise_train <- noise_train %>%
  filter(Attack == 0)

noise_feature_names <- c(
  "GPS_INS_Disagreement",
  "Fusion_INS_Disagreement",
  "Sonar_Detections",
  "Depth_Deviation",
  "Course_Deviation"
)

# Estimate 99th-percentile thresholds
# on noise-aware normal training conditions
noise_thresholds <- sapply(
  normal_noise_train[noise_feature_names],
  function(x) as.numeric(
    quantile(x, 0.99, na.rm = TRUE)
  )
)

cat("\nNOISE-AWARE DETECTION THRESHOLDS\n")
print(noise_thresholds)

# Compare with the original thresholds
threshold_comparison <- data.frame(
  Feature = noise_feature_names,
  Original_Threshold = as.numeric(
    thresholds[noise_feature_names]
  ),
  Noise_Aware_Threshold = as.numeric(
    noise_thresholds[noise_feature_names]
  )
)

cat("\nORIGINAL VERSUS NOISE-AWARE THRESHOLDS\n")
print(threshold_comparison)

write_csv(
  threshold_comparison,
  "noise_aware_threshold_comparison.csv"
)


# ======================================================
# STEP 18C: NOISE-AWARE VALIDATION
# ======================================================

# Detector uses the new thresholds.
# The old detector and thresholds remain unchanged.

noise_aware_detector <- function(data) {
  
  alarm_matrix <- sapply(
    noise_feature_names,
    function(feature) {
      data[[feature]] >
        noise_thresholds[[feature]]
    }
  )
  
  data$predicted_attack <- as.integer(
    rowSums(alarm_matrix) > 0 |
      data$Evasive_Action == 1
  )
  
  data
}

noise_validation_raw <- noise_aware_detector(
  noise_validation
)

noise_validation_k1 <- apply_persistence(
  noise_validation_raw, k = 1L
)

noise_validation_k2 <- apply_persistence(
  noise_validation_raw, k = 2L
)

noise_validation_comparison <- bind_rows(
  calculate_metrics(noise_validation_k1) %>%
    mutate(Persistence = 1L),
  
  calculate_metrics(noise_validation_k2) %>%
    mutate(Persistence = 2L)
) %>%
  select(
    Persistence, Precision, Recall, F1,
    False_Positive_Rate, TP, TN, FP, FN
  )

cat("\nNOISE-AWARE VALIDATION RESULTS\n")
print(noise_validation_comparison, width = Inf)

write_csv(
  noise_validation_comparison,
  "noise_aware_validation_comparison.csv"
)


# =====================================================
# STEP 19: NOISE-AWARE HELD-OUT TEST EVALUATION
# =====================================================

library(dplyr)
library(readr)

selected_k <- 2L

# Apply training-calibrated thresholds
noise_test_raw <- noise_aware_detector(noise_test)

# Apply validation-selected persistence
noise_test_final <- apply_persistence(
  noise_test_raw,
  k = selected_k
)

# Overall held-out test performance
noise_test_metrics <- calculate_metrics(noise_test_final)

cat("\nNOISE-AWARE FINAL TEST RESULTS\n")
print(noise_test_metrics, width = Inf)

# Evaluate recall by attack scenario
noise_test_recall <- noise_test_final %>%
  filter(Attack == 1) %>%
  group_by(Scenario) %>%
  summarise(
    Attack_Observations = n(),
    Detected = sum(predicted_attack == 1),
    Recall = mean(predicted_attack == 1),
    .groups = "drop"
  )

cat("\nNOISE-AWARE TEST RECALL BY SCENARIO\n")
print(noise_test_recall, n = Inf)

# Assess performance under each noise condition
noise_condition_metrics <- noise_test_final %>%
  group_by(stress_condition) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

cat("\nTEST PERFORMANCE BY NOISE CONDITION\n")
print(noise_condition_metrics, width = Inf)

# Save all results
write_csv(
  noise_test_metrics,
  "noise_aware_final_test_metrics.csv"
)

write_csv(
  noise_test_recall,
  "noise_aware_final_test_scenario_recall.csv"
)

write_csv(
  noise_condition_metrics,
  "noise_aware_test_noise_condition_metrics.csv"
)

cat("\nStep 19 completed.\n")



# ==========================================================
# STEP 20: NOISE-AWARE RUN-LEVEL BOOTSTRAP
# ==========================================================

library(dplyr)
library(readr)

# Build confusion counts for each independent test run
noise_run_counts <- noise_test_final %>%
  group_by(run_id, Scenario, stress_condition) %>%
  summarise(
    TP = sum(Attack == 1 & predicted_attack == 1),
    TN = sum(Attack == 0 & predicted_attack == 0),
    FP = sum(Attack == 0 & predicted_attack == 1),
    FN = sum(Attack == 1 & predicted_attack == 0),
    .groups = "drop"
  )

score_runs <- function(x) {
  
  tp <- sum(x$TP)
  tn <- sum(x$TN)
  fp <- sum(x$FP)
  fn <- sum(x$FN)
  
  precision <- if ((tp + fp) > 0) {
    tp / (tp + fp)
  } else NA_real_
  
  recall <- if ((tp + fn) > 0) {
    tp / (tp + fn)
  } else NA_real_
  
  fpr <- if ((fp + tn) > 0) {
    fp / (fp + tn)
  } else NA_real_
  
  f1 <- if (
    !is.na(precision) &&
    !is.na(recall) &&
    (precision + recall) > 0
  ) {
    2 * precision * recall / (precision + recall)
  } else NA_real_
  
  c(
    Precision = precision,
    Recall = recall,
    F1 = f1,
    False_Positive_Rate = fpr
  )
}

# Preserve the scenario and noise-condition composition
# through stratified run-level resampling.

set.seed(20261013)

B <- 2000

strata <- split(
  seq_len(nrow(noise_run_counts)),
  interaction(
    noise_run_counts$Scenario,
    noise_run_counts$stress_condition,
    drop = TRUE
  )
)

bootstrap_scores <- replicate(B, {
  
  sampled_indices <- unlist(
    lapply(strata, function(indices) {
      sample(
        indices,
        size = length(indices),
        replace = TRUE
      )
    }),
    use.names = FALSE
  )
  
  score_runs(
    noise_run_counts[sampled_indices, ]
  )
})

point_estimates <- score_runs(noise_run_counts)

noise_ci <- tibble(
  Metric = names(point_estimates),
  Estimate = as.numeric(point_estimates),
  Lower_95 = apply(
    bootstrap_scores,
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  ),
  Upper_95 = apply(
    bootstrap_scores,
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
)

cat("\nNOISE-AWARE TEST: RUN-LEVEL CONFIDENCE INTERVALS\n")
print(noise_ci, n = Inf)

write_csv(
  noise_ci,
  "noise_aware_run_level_confidence_intervals.csv"
)


# =====================================================
# STEP 21: FROZEN DETECTOR UNDER UNSEEN SENSOR NOISE
# =====================================================

library(dplyr)
library(readr)

# New noise conditions, outside the development range
unseen_settings <- data.frame(
  condition = c("Unseen_A", "Unseen_B"),
  position_noise = c(1.5, 2.5),
  depth_noise = c(1.0, 1.5),
  course_noise = c(1.2, 2.0),
  depth_bias_sd = c(1.5, 2.5)
)

# Generate fresh simulation runs
unseen_results <- list()
counter <- 1L

for (j in seq_len(nrow(unseen_settings))) {
  
  settings <- unseen_settings[j, , drop = FALSE]
  
  for (scenario in scenarios) {
    
    for (run in 1:3) {
      
      new_seed <- 250000 + counter * 10
      
      one_run <- simulate_navigation(
        scenario = scenario,
        run_id = paste0(
          "OOD_", settings$condition, "_",
          scenario, "_", run
        ),
        seed = new_seed,
        n_steps = 500,
        attack_intensity = 1
      )
      
      unseen_results[[counter]] <-
        apply_sensor_shift(
          data = one_run,
          settings = settings,
          seed = new_seed + 1
        )
      
      counter <- counter + 1L
    }
  }
}

unseen_data <- bind_rows(unseen_results)

cat("New runs:",
    n_distinct(unseen_data$run_id), "\n")

cat("Observations:",
    nrow(unseen_data), "\n")

cat("Missing values:",
    sum(is.na(unseen_data)), "\n")

# =====================================================
# EVALUATE THE FROZEN NOISE-AWARE DETECTOR
# No new thresholds, tuning, or calibration.
# =====================================================

unseen_features <- add_sensor_features(unseen_data)

unseen_raw <- noise_aware_detector(unseen_features)

unseen_predictions <- apply_persistence(
  unseen_raw,
  k = 2L
)

unseen_metrics <- unseen_predictions %>%
  group_by(stress_condition) %>%
  group_modify(~ calculate_metrics(.x)) %>%
  ungroup()

cat("\nUNSEEN-NOISE ROBUSTNESS RESULTS\n")
print(unseen_metrics, width = Inf)

# Recall by attack category
unseen_recall <- unseen_predictions %>%
  filter(Attack == 1) %>%
  group_by(stress_condition, Scenario) %>%
  summarise(
    Attack_Observations = n(),
    Detected = sum(predicted_attack),
    Recall = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nUNSEEN-NOISE ATTACK RECALL\n")
print(unseen_recall, n = Inf)

# Preserve all outputs
write_csv(
  unseen_metrics,
  "unseen_noise_robustness_metrics.csv"
)

write_csv(
  unseen_recall,
  "unseen_noise_attack_recall.csv"
)

write_csv(
  unseen_data,
  "unseen_noise_simulation_data.csv"
)

cat("\nStep 21 completed.\n")



# ============================================================
# STEP 22: UNSEEN-NOISE FALSE-ALARM DIAGNOSTICS
# ============================================================

library(dplyr)
library(readr)

# Use the frozen thresholds from Step 18.
# Examine only observations without active attacks.

unseen_rule_summary <- unseen_features %>%
  filter(Attack == 0) %>%
  mutate(
    GPS_Rule = GPS_INS_Disagreement >
      noise_thresholds[["GPS_INS_Disagreement"]],
    
    Fusion_Rule = Fusion_INS_Disagreement >
      noise_thresholds[["Fusion_INS_Disagreement"]],
    
    Sonar_Rule = Sonar_Detections >
      noise_thresholds[["Sonar_Detections"]],
    
    Depth_Rule = Depth_Deviation >
      noise_thresholds[["Depth_Deviation"]],
    
    Course_Rule = Course_Deviation >
      noise_thresholds[["Course_Deviation"]],
    
    Evasive_Rule = Evasive_Action == 1
  ) %>%
  group_by(stress_condition) %>%
  summarise(
    GPS = mean(GPS_Rule),
    Fusion = mean(Fusion_Rule),
    Sonar = mean(Sonar_Rule),
    Depth = mean(Depth_Rule),
    Course = mean(Course_Rule),
    Evasive = mean(Evasive_Rule),
    .groups = "drop"
  )

cat("\nUNSEEN-NOISE RULE ACTIVATION RATES\n")
print(unseen_rule_summary, width = Inf)

# Run-level false-positive rates after persistence k = 2
unseen_run_fpr <- unseen_predictions %>%
  filter(Attack == 0) %>%
  group_by(stress_condition, run_id, Scenario) %>%
  summarise(
    normal_samples = n(),
    false_positive_samples = sum(predicted_attack),
    run_fpr = mean(predicted_attack),
    .groups = "drop"
  )

cat("\nRUN-LEVEL FALSE-POSITIVE SUMMARY\n")

unseen_run_fpr %>%
  group_by(stress_condition) %>%
  summarise(
    runs = n(),
    median_run_fpr = median(run_fpr),
    min_run_fpr = min(run_fpr),
    max_run_fpr = max(run_fpr),
    runs_with_false_alarms =
      sum(false_positive_samples > 0),
    .groups = "drop"
  ) %>%
  print(width = Inf)

write_csv(
  unseen_rule_summary,
  "unseen_noise_rule_diagnostics.csv"
)

write_csv(
  unseen_run_fpr,
  "unseen_noise_run_false_positive_rates.csv"
)



# ==========================================================
# STEP 23: RUN-LEVEL UNCERTAINTY UNDER UNSEEN NOISE
# ==========================================================

library(dplyr)
library(readr)

# Run-level confusion counts
ood_run_counts <- unseen_predictions %>%
  group_by(stress_condition, run_id) %>%
  summarise(
    TP = sum(Attack == 1 & predicted_attack == 1),
    TN = sum(Attack == 0 & predicted_attack == 0),
    FP = sum(Attack == 0 & predicted_attack == 1),
    FN = sum(Attack == 1 & predicted_attack == 0),
    .groups = "drop"
  )

# Score using aggregated confusion counts
score_ood <- function(data) {
  tp <- sum(data$TP)
  tn <- sum(data$TN)
  fp <- sum(data$FP)
  fn <- sum(data$FN)
  
  c(
    Recall = if (tp + fn > 0) tp / (tp + fn) else NA_real_,
    FPR = if (fp + tn > 0) fp / (fp + tn) else NA_real_
  )
}

set.seed(20261014)
B <- 2000

ood_ci <- lapply(
  split(ood_run_counts, ood_run_counts$stress_condition),
  function(group_data) {
    
    point <- score_ood(group_data)
    
    boot <- replicate(B, {
      sampled <- group_data[
        sample(
          seq_len(nrow(group_data)),
          replace = TRUE
        ),
        ,
        drop = FALSE
      ]
      
      score_ood(sampled)
    })
    
    tibble(
      Metric = names(point),
      Estimate = as.numeric(point),
      Lower_95 = apply(
        boot, 1, quantile,
        probs = 0.025, na.rm = TRUE
      ),
      Upper_95 = apply(
        boot, 1, quantile,
        probs = 0.975, na.rm = TRUE
      )
    )
  }
)

ood_ci_results <- bind_rows(
  ood_ci,
  .id = "stress_condition"
)

cat("\nUNSEEN-NOISE BOOTSTRAP RESULTS\n")
print(ood_ci_results, n = Inf)

write_csv(
  ood_ci_results,
  "unseen_noise_bootstrap_confidence_intervals.csv"
)

