# =============================================================================
# The Effects of Information Pollution on Misinformation Susceptibility:
# A Signal Detection Theory Approach
# =============================================================================

library(tidyverse)
library(ggdist)
library(emmeans)
library(afex)
library(patchwork)
library(car)
library(lme4)

all_data <- read_csv("clean_data.csv", show_col_types = FALSE)

# =============================================================================
# Exclusions
# =============================================================================

attention_check <- all_data %>%
  filter(stimulus_type == "attention_check") %>%
  select(participant_id, post_id, passed = accurate) %>%
  pivot_wider(
    names_from  = post_id,
    values_from = passed,
    names_prefix = "passed_"
  ) %>%
  mutate(
    num_passed = rowSums(select(., starts_with("passed_")), na.rm = TRUE)
  )

failed_both <- attention_check %>%
  filter(num_passed == 0)

cat("Participants who failed both attention checks:", nrow(failed_both), "\n")

# Exclude participants who failed both attention checks from main data
all_data <- anti_join(all_data, failed_both, by = "participant_id")

# =============================================================================
# SDT parameters
# =============================================================================

# -----------------------------------------------------------------------------
# Correction for extreme proportions
# -----------------------------------------------------------------------------

correct_proportion <- function(hits, n) {
  p <- ifelse(n > 0, hits / n, NA_real_)
  ifelse(p == 0, 1 / (2 * n),
         ifelse(p == 1, 1 - 1 / (2 * n), p))
}

# -----------------------------------------------------------------------------
# Function to calculate SDT parameters for a single participant
# -----------------------------------------------------------------------------

calculate_sdt_participant <- function(participant_data) {

  targets <- participant_data %>%
    filter(stimulus_type == "target")

  # Overall d' and c
  signal_trials <- targets %>% filter(true_value_logical == TRUE)
  n_signal <- nrow(signal_trials)
  hits <- sum(signal_trials$response_logical == TRUE)

  noise_trials <- targets %>% filter(true_value_logical == FALSE)
  n_noise <- nrow(noise_trials)
  false_alarms <- sum(noise_trials$response_logical == TRUE)

  hit_rate <- correct_proportion(hits, n_signal)
  fa_rate <- correct_proportion(false_alarms, n_noise)

  z_hit <- qnorm(hit_rate)
  z_fa <- qnorm(fa_rate)

  d_prime <- z_hit - z_fa
  c_overall <- -0.5 * (z_hit + z_fa)

  # c for belief-congruent stimuli
  congruent <- targets %>% filter(congruence == "congruent")

  if (nrow(congruent) > 0) {
    cong_signal <- congruent %>% filter(true_value_logical == TRUE)
    cong_noise <- congruent %>% filter(true_value_logical == FALSE)

    cong_hits <- sum(cong_signal$response_logical == TRUE)
    cong_fa <- sum(cong_noise$response_logical == TRUE)

    cong_hr <- correct_proportion(cong_hits, nrow(cong_signal))
    cong_far <- correct_proportion(cong_fa, nrow(cong_noise))

    c_congruent <- -0.5 * (qnorm(cong_hr) + qnorm(cong_far))
  } else {
    c_congruent <- NA
  }

  # c for belief-incongruent stimuli
  incongruent <- targets %>% filter(congruence == "incongruent")

  if (nrow(incongruent) > 0) {
    incong_signal <- incongruent %>% filter(true_value_logical == TRUE)
    incong_noise <- incongruent %>% filter(true_value_logical == FALSE)

    incong_hits <- sum(incong_signal$response_logical == TRUE)
    incong_fa <- sum(incong_noise$response_logical == TRUE)

    incong_hr <- correct_proportion(incong_hits, nrow(incong_signal))
    incong_far <- correct_proportion(incong_fa, nrow(incong_noise))

    c_incongruent <- -0.5 * (qnorm(incong_hr) + qnorm(incong_far))
  } else {
    c_incongruent <- NA
  }

  delta_c <- c_congruent - c_incongruent

  tibble(
    d_prime = d_prime,
    c_overall = c_overall,
    c_congruent = c_congruent,
    c_incongruent = c_incongruent,
    delta_c = delta_c,
    hit_rate_raw = hits / n_signal,
    fa_rate_raw = false_alarms / n_noise
  )
}

# -----------------------------------------------------------------------------
# Function to calculate SDT parameters for all participants
# -----------------------------------------------------------------------------

calculate_sdt_all <- function(trial_data) {

  participant_info <- trial_data %>%
    distinct(
      participant_id,
      nuclear_stance_numeric,
      condition,
      condition_quality,
      condition_quantity,
      rt_m
    )

  sdt_results <- trial_data %>%
    group_by(participant_id) %>%
    group_modify(~ calculate_sdt_participant(.x)) %>%
    ungroup()

  participant_info %>%
    left_join(sdt_results, by = "participant_id")
}

condition_labels <- c(
  "normal_normal" = "Low pollution",
  "normal_high"   = "High quantity",
  "low_normal"    = "Low quality",
  "low_high"      = "High pollution"
)

sdt_data <- calculate_sdt_all(all_data) %>%
  mutate(
    prior_belief_strength = abs(nuclear_stance_numeric),
    condition_label    = factor(condition_labels[condition],
                                levels = unname(condition_labels)),
    condition_quality  = factor(condition_quality,  levels = c("normal", "low")),
    condition_quantity = factor(condition_quantity, levels = c("normal", "high"))
  )

# =============================================================================
# Descriptive statistics
# =============================================================================

cat("\n=== SDT parameters by condition ===\n")
sdt_means_by_condition <- sdt_data %>%
  group_by(condition_label) %>%
  summarise(
    n          = n(),
    d_prime_M  = mean(d_prime),
    d_prime_SD = sd(d_prime),
    delta_c_M  = mean(delta_c),
    delta_c_SD = sd(delta_c),
    c_M        = mean(c_overall),
    c_SD       = sd(c_overall),
    .groups = "drop"
  )

print(sdt_means_by_condition)

cat("\n=== Response rates and SDT parameters (all participants) ===\n")
report_vars <- c("hit_rate_raw", "fa_rate_raw", "d_prime", "c_overall",
                 "c_congruent", "c_incongruent", "delta_c")
for (v in report_vars) {
  cat(sprintf("%-14s M = %7.3f, SD = %6.3f\n",
              v, mean(sdt_data[[v]]), sd(sdt_data[[v]])))
}

# Equals (HR + FAR) / 2 because every participant saw 20 true and 20 false target posts
cat(sprintf("\nProportion of target posts judged 'true': %.4f\n",
            mean((sdt_data$hit_rate_raw + sdt_data$fa_rate_raw) / 2)))

cat("\n=== One-sample t-tests against zero ===\n")
for (v in c("d_prime", "c_overall", "delta_c")) {
  tt <- t.test(sdt_data[[v]])
  cat(sprintf("%-10s t(%d) = %6.2f, p = %.3g, 95%% CI [%.3f, %.3f]\n",
              v, tt$parameter, tt$statistic, tt$p.value,
              tt$conf.int[1], tt$conf.int[2]))
}

# -----------------------------------------------------------------------------
# Time spent on the feed
# -----------------------------------------------------------------------------

summarise_feed_time <- function(data) {
  data %>%
    summarise(
      n       = n(),
      M_min   = mean(rt_m),
      SD_min  = sd(rt_m),
      Mdn_min = median(rt_m),
      .groups = "drop"
    )
}

feed_time_by_quantity <- bind_rows(
  sdt_data %>%
    group_by(group = as.character(condition_quantity)) %>%
    summarise_feed_time(),
  sdt_data %>%
    mutate(group = "all participants") %>%
    group_by(group) %>%
    summarise_feed_time()
)

cat("\n=== Feed time by quantity (minutes) ===\n")
feed_time_by_quantity %>%
  mutate(across(where(is.double), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print()

# =============================================================================
# Figure 2: SDT parameters by quality and quantity
# =============================================================================

okabe_ito       <- c("normal" = "#0072B2", "low" = "#D55E00")
quality_labels  <- c("normal" = "Normal", "low"  = "Low")
quantity_labels <- c("normal" = "Normal", "high" = "High")

y_labels <- list(
  d_prime   = expression("Truth discernment (" * italic(d) * "'" * ")"),
  delta_c   = expression("Prior-belief reliance (" * Delta * italic(c) * ")"),
  c_overall = expression("Response bias (" * italic(c) * ")")
)

panel_raincloud <- function(outcome) {

  m <- aov_ez(id = "participant_id", dv = outcome, data = sdt_data,
              between = c("condition_quality", "condition_quantity"))
  emm <- as.data.frame(emmeans(m, ~ condition_quality * condition_quantity))
  pd  <- position_dodge(width = 0.7)

  ggplot(sdt_data, aes(x = condition_quantity, y = .data[[outcome]],
                       fill = condition_quality)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    stat_slab(position = pd, side = "right", scale = 0.45,
              alpha = 0.30, colour = NA, normalize = "groups") +
    stat_dots(position = pd, side = "left", scale = 0.45,
              dotsize = 0.9, alpha = 0.55, colour = NA) +
    geom_pointinterval(
      data = emm,
      aes(x = condition_quantity, y = emmean, ymin = lower.CL, ymax = upper.CL,
          group = condition_quality),
      position = position_dodge(width = 0.7),
      colour = "grey15", linewidth = 1.1, point_size = 2.6,
      inherit.aes = FALSE) +
    scale_fill_manual(values = okabe_ito, labels = quality_labels) +
    scale_x_discrete(labels = quantity_labels) +
    labs(x = "Quantity of pollution", y = y_labels[[outcome]], fill = "Quality") +
    theme_ggdist() +
    theme(panel.grid.major.x = element_blank(), legend.position = "bottom")
}

fig2 <- wrap_plots(lapply(c("d_prime", "delta_c", "c_overall"), panel_raincloud),
                   nrow = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "1") &
  theme(legend.position = "bottom",
        text = element_text(size = 16.5),
        axis.text.x = element_text(size = 16, colour = "black"),
        plot.tag.location = "panel",
        plot.tag.position = "topleft",
        plot.tag = element_text(margin = margin(t = 2, l = 8)))

print(fig2)

# =============================================================================
# Hypothesis tests: 2 x 2 ANOVAs
# =============================================================================

# η² = SS_effect / SS_total
eta_squared <- function(model) {
  ss <- summary(model)[[1]][["Sum Sq"]]
  eta_sq <- ss / sum(ss)
  names(eta_sq) <- rownames(summary(model)[[1]])
  eta_sq
}

# H1: d' ~ quality * quantity
h1_model <- aov(d_prime ~ condition_quality * condition_quantity, data = sdt_data)
cat("\n=== H1: Truth discernment (d') ===\n")
print(summary(h1_model))
cat("η²:\n"); print(round(eta_squared(h1_model), 4))

# H2: Δc ~ quality * quantity
h2_model <- aov(delta_c ~ condition_quality * condition_quantity, data = sdt_data)
cat("\n=== H2: Prior-belief reliance (Δc) ===\n")
print(summary(h2_model))
cat("η²:\n"); print(round(eta_squared(h2_model), 4))

# H3: c ~ quality * quantity
h3_model <- aov(c_overall ~ condition_quality * condition_quantity, data = sdt_data)
cat("\n=== H3: Response bias (c) ===\n")
print(summary(h3_model))
cat("η²:\n"); print(round(eta_squared(h3_model), 4))

# -----------------------------------------------------------------------------
# Assumption checks
# -----------------------------------------------------------------------------

cat("\n=== Shapiro-Wilk tests on residuals ===\n")
cat("H1 (d'):\n"); print(shapiro.test(residuals(h1_model)))
cat("H2 (Δc):\n"); print(shapiro.test(residuals(h2_model)))
cat("H3 (c):\n");  print(shapiro.test(residuals(h3_model)))

par(mfrow = c(1, 3))
qqnorm(residuals(h1_model), main = "Q-Q: d' residuals");  qqline(residuals(h1_model))
qqnorm(residuals(h2_model), main = "Q-Q: Δc residuals");  qqline(residuals(h2_model))
qqnorm(residuals(h3_model), main = "Q-Q: c residuals");   qqline(residuals(h3_model))
par(mfrow = c(1, 1))

cat("\n=== Levene's tests for homogeneity of variance ===\n")
cat("H1 (d'):\n"); print(leveneTest(d_prime ~ condition_quality * condition_quantity, data = sdt_data))
cat("H2 (Δc):\n"); print(leveneTest(delta_c ~ condition_quality * condition_quantity, data = sdt_data))
cat("H3 (c):\n");  print(leveneTest(c_overall ~ condition_quality * condition_quantity, data = sdt_data))

# -----------------------------------------------------------------------------
# d' by quality
# -----------------------------------------------------------------------------

cat("\n=== d' by quality ===\n")
sdt_data %>%
  group_by(condition_quality) %>%
  summarise(n = n(), M = mean(d_prime), SD = sd(d_prime), .groups = "drop") %>%
  print()

# -----------------------------------------------------------------------------
# Filler posts judged 'true' by quality
# -----------------------------------------------------------------------------

filler_data <- all_data %>%
  filter(stimulus_type == "filler") %>%
  group_by(participant_id, condition_quality) %>%
  summarise(true_ratio = mean(response_logical, na.rm = TRUE), .groups = "drop")

cat("\n=== Proportion of filler posts judged 'true' by quality ===\n")
filler_data %>%
  group_by(condition_quality) %>%
  summarise(n = n(), M = mean(true_ratio), SD = sd(true_ratio), .groups = "drop") %>%
  print()

# =============================================================================
# Exploratory analysis: prior-belief strength as moderator of Δc
# =============================================================================

belief_model <- aov(delta_c ~ condition_quality * condition_quantity * prior_belief_strength,
                    data = sdt_data)
cat("\n=== Exploratory: prior-belief strength and Δc ===\n")
print(summary(belief_model))
cat("η²:\n"); print(round(eta_squared(belief_model), 4))

# =============================================================================
# Robustness check: trial-level probit GLMM
# =============================================================================

glmm_data <- all_data %>%
  filter(stimulus_type == "target") %>%
  mutate(
    Response   = as.integer(response_logical),
    Veracity   = ifelse(true_value_logical == TRUE,  0.5, -0.5),
    Congruence = ifelse(congruence == "congruent",   0.5, -0.5),
    Quantity   = ifelse(condition_quantity == "high", 0.5, -0.5),
    Quality    = ifelse(condition_quality == "low",   0.5, -0.5)
  )

glmm <- glmer(
  Response ~
    Veracity * Congruence +
    Veracity:Quantity +            # H1b: quantity impairs d'
    Veracity:Quality +             # H1a: quality impairs d'
    Congruence:Quantity +          # H2b: quantity increases Δc
    Congruence:Quality +           # H2a: quality increases Δc
    Quantity +                     # H3b: quantity shifts c
    Quality +                      # H3a: quality shifts c
    Veracity:Quantity:Quality +    # Quantity×Quality interaction on d'
    Congruence:Quantity:Quality +  # Quantity×Quality interaction on Δc
    Quantity:Quality +             # Quantity×Quality interaction on c
    (1 + Veracity + Congruence | participant_id) +
    (1 | post_id),
  data = glmm_data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
)

cat("\n=== GLMM summary ===\n")
print(summary(glmm))

# -----------------------------------------------------------------------------
# Table 1: GLMM fixed effects
# -----------------------------------------------------------------------------

table1_terms <- c(
  "(Intercept)", "Veracity", "Congruence", "Veracity:Congruence",
  "Quality", "Quantity",
  "Veracity:Quality", "Veracity:Quantity",
  "Congruence:Quality", "Congruence:Quantity",
  "Quantity:Quality", "Veracity:Quantity:Quality", "Congruence:Quantity:Quality"
)

cat("\n=== Table 1: GLMM fixed effects ===\n")
print(round(coef(summary(glmm))[table1_terms, ], 3))
