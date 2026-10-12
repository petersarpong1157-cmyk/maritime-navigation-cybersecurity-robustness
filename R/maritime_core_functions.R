simulate_navigation <- 
function (scenario, run_id, seed, n_steps = 500, attack_intensity = 1) 
{
    set.seed(seed)
    Time <- seq(0, n_steps - 1)
    Velocity_X <- rnorm(n_steps, mean = 5, sd = 0.2)
    Velocity_Y <- rnorm(n_steps, mean = 2, sd = 0.2)
    true_x <- cumsum(Velocity_X)
    true_y <- cumsum(Velocity_Y)
    Position_X <- true_x + rnorm(n_steps, 0, 0.8)
    Position_Y <- true_y + rnorm(n_steps, 0, 0.8)
    GPS_Position_X <- true_x + rnorm(n_steps, 0, 1)
    GPS_Position_Y <- true_y + rnorm(n_steps, 0, 1)
    Sonar_Detections <- rpois(n_steps, lambda = 0.1)
    Depth <- 100 + rnorm(n_steps, 0, 0.5)
    Course <- atan2(Velocity_Y, Velocity_X) * 180/pi
    Evasive_Action <- as.integer(Sonar_Detections > 2)
    Fused_Position_X <- (Position_X + GPS_Position_X)/2
    Fused_Position_Y <- (Position_Y + GPS_Position_Y)/2
    attack_start <- sample(seq(floor(n_steps * 0.3), floor(n_steps * 0.7)), size = 1)
    attack_rows <- attack_start:n_steps
    if (scenario == "INS_Attack") {
        Position_X[attack_rows] <- Position_X[attack_rows] + cumsum(rnorm(length(attack_rows), 0.4 * attack_intensity, 0.3))
    }
    if (scenario == "GPS_Attack") {
        GPS_Position_X[attack_rows] <- GPS_Position_X[attack_rows] + seq(0, 15 * attack_intensity, length.out = length(attack_rows))
    }
    if (scenario == "Sonar_Attack") {
        Sonar_Detections[attack_rows] <- Sonar_Detections[attack_rows] + rpois(length(attack_rows), lambda = 2 * attack_intensity)
    }
    if (scenario == "Depth_Attack") {
        Depth[attack_rows] <- Depth[attack_rows] + 5 * attack_intensity
    }
    if (scenario == "Collision_Attack") {
        Evasive_Action[attack_rows] <- rbinom(length(attack_rows), 1, 0.5)
    }
    if (scenario == "Autopilot_Attack") {
        Course[attack_rows] <- Course[attack_rows] + rnorm(length(attack_rows), 10 * attack_intensity, 3)
    }
    if (scenario == "SensorFusion_Attack") {
        Fused_Position_X[attack_rows] <- Fused_Position_X[attack_rows] + 8 * attack_intensity
    }
    if (scenario != "SensorFusion_Attack") {
        Fused_Position_X <- (Position_X + GPS_Position_X)/2
        Fused_Position_Y <- (Position_Y + GPS_Position_Y)/2
    }
    tibble(run_id = run_id, seed = seed, Time = Time, Position_X = Position_X, Position_Y = Position_Y, Velocity_X = Velocity_X, 
        Velocity_Y = Velocity_Y, Sonar_Detections = Sonar_Detections, Depth = Depth, GPS_Position_X = GPS_Position_X, GPS_Position_Y = GPS_Position_Y, 
        Course = Course, Evasive_Action = Evasive_Action, Fused_Position_X = Fused_Position_X, Fused_Position_Y = Fused_Position_Y, 
        Scenario = scenario, Attack = as.integer(scenario != "Normal" & Time >= (attack_start - 1)), attack_intensity = attack_intensity)
}


apply_sensor_shift <- 
function (data, settings, seed) 
{
    set.seed(seed)
    n <- nrow(data)
    original_fusion_residual_x <- data$Fused_Position_X - (data$Position_X + data$GPS_Position_X)/2
    original_fusion_residual_y <- data$Fused_Position_Y - (data$Position_Y + data$GPS_Position_Y)/2
    depth_bias <- rnorm(1, mean = 0, sd = settings$depth_bias_sd)
    data$Position_X <- data$Position_X + rnorm(n, 0, settings$position_noise)
    data$Position_Y <- data$Position_Y + rnorm(n, 0, settings$position_noise)
    data$GPS_Position_X <- data$GPS_Position_X + rnorm(n, 0, settings$position_noise)
    data$GPS_Position_Y <- data$GPS_Position_Y + rnorm(n, 0, settings$position_noise)
    data$Depth <- data$Depth + depth_bias + rnorm(n, 0, settings$depth_noise)
    data$Course <- data$Course + rnorm(n, 0, settings$course_noise)
    data$Fused_Position_X <- (data$Position_X + data$GPS_Position_X)/2 + original_fusion_residual_x
    data$Fused_Position_Y <- (data$Position_Y + data$GPS_Position_Y)/2 + original_fusion_residual_y
    data$stress_condition <- settings$condition
    data
}


noise_aware_detector <- 
function (data) 
{
    alarm_matrix <- sapply(noise_feature_names, function(feature) {
        data[[feature]] > noise_thresholds[[feature]]
    })
    data$predicted_attack <- as.integer(rowSums(alarm_matrix) > 0 | data$Evasive_Action == 1)
    data
}


apply_persistence <- 
function (data, k = 1) 
{
    data %>% arrange(run_id, Time) %>% group_by(run_id) %>% mutate(raw_alarm = as.integer(predicted_attack), consecutive_count = {
        x <- raw_alarm
        counts <- integer(length(x))
        streak <- 0L
        for (i in seq_along(x)) {
            if (x[i] == 1L) {
                streak <- streak + 1L
            }
            else {
                streak <- 0L
            }
            counts[i] <- streak
        }
        counts
    }, predicted_attack = as.integer(consecutive_count >= k)) %>% ungroup()
}


