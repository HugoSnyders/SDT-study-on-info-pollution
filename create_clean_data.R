# =============================================================================
# Create clean trial-level data from anonymized raw data file
# =============================================================================

library(tidyverse)
library(jsonlite)

# -----------------------------------------------------------------------------
# Read raw data
# -----------------------------------------------------------------------------

raw_data <- read_csv("raw_data.csv", show_col_types = FALSE)

# Get unique participants
participants <- raw_data %>%
  distinct(participant_id)

cat("Found", nrow(participants), "participants in raw_data.csv\n")

# -----------------------------------------------------------------------------
# Parse each participant's data into trial-level rows
# -----------------------------------------------------------------------------

all_trials <- map_dfr(participants$participant_id, function(pid) {

  pdata <- raw_data %>% filter(participant_id == pid)

  # ---------------------------------------------------------------------------
  # Extract nuclear stance from slider response
  # ---------------------------------------------------------------------------
  slider_row <- pdata %>%
    filter(trial_type == "html-slider-response")

  nuclear_stance_numeric <- as.numeric(slider_row$response[1])

  nuclear_stance_binary <- case_when(
    nuclear_stance_numeric > 0 ~ "pro",
    nuclear_stance_numeric < 0 ~ "anti",
    TRUE ~ "neutral"
  )

  # ---------------------------------------------------------------------------
  # Extract condition information from survey-html-form row
  # ---------------------------------------------------------------------------
  survey_row <- pdata %>%
    filter(trial_type == "survey-html-form")

  condition <- survey_row$condition[1]
  condition_quality <- survey_row$condition_quality[1]
  condition_quantity <- survey_row$condition_quantity[1]
  num_fillers <- survey_row$num_fillers[1]
  age <- pdata$age[1]
  response_time <- survey_row$rt[1]

  # ---------------------------------------------------------------------------
  # Parse trial-level data from post_responses JSON
  # ---------------------------------------------------------------------------
  post_responses_json <- survey_row$post_responses[1]

  if (is.na(post_responses_json) || post_responses_json == "") {
    warning(paste("Participant", pid, ": no post_responses data, skipping"))
    return(NULL)
  }

  trials_df <- fromJSON(post_responses_json) %>%
    as_tibble() %>%
    mutate(true_value = as.character(true_value))

  # ---------------------------------------------------------------------------
  # Add participant-level variables
  # ---------------------------------------------------------------------------
  trials_df <- trials_df %>%
    mutate(
      participant_id = pid,
      nuclear_stance_numeric = nuclear_stance_numeric,
      nuclear_stance_binary = nuclear_stance_binary,
      condition = condition,
      condition_quality = condition_quality,
      condition_quantity = condition_quantity,
      num_fillers = num_fillers,
      age = age,
      rt = response_time / 1000,
      rt_m = rt / 60,
      trial_number = row_number()
    )

  # ---------------------------------------------------------------------------
  # Derived variables
  # ---------------------------------------------------------------------------
  trials_df <- trials_df %>%
    mutate(
      # Ensure true_value is logical
      true_value_logical = case_when(
        true_value == "true" | true_value == TRUE ~ TRUE,
        true_value == "false" | true_value == FALSE ~ FALSE,
        TRUE ~ NA
      ),
      # Response as logical
      response_logical = response == "true",
      # Accuracy
      accurate = response_logical == true_value_logical,
      # Position: order among relevant (non-filler) stimuli, NA for fillers
      position = if_else(
        stimulus_type != "filler",
        cumsum(stimulus_type != "filler"),
        NA_integer_
      ),
      # Congruence (target stimuli only)
      congruence = case_when(
        stimulus_type != "target" ~ NA_character_,
        nuclear_stance_binary == "pro" & stance == "pro" ~ "congruent",
        nuclear_stance_binary == "pro" & stance == "con" ~ "incongruent",
        nuclear_stance_binary == "anti" & stance == "con" ~ "congruent",
        nuclear_stance_binary == "anti" & stance == "pro" ~ "incongruent",
        nuclear_stance_binary == "neutral" ~ "neutral",
        TRUE ~ NA_character_
      )
    )

  # ---------------------------------------------------------------------------
  # Reorder columns
  # ---------------------------------------------------------------------------
  trials_df %>%
    select(
      participant_id,
      nuclear_stance_numeric,
      nuclear_stance_binary,
      condition,
      condition_quality,
      condition_quantity,
      num_fillers,
      age,
      rt,
      rt_m,
      trial_number,
      position,
      post_id,
      stimulus_type,
      topic,
      stance,
      congruence,
      source,
      true_value,
      true_value_logical,
      response,
      response_logical,
      accurate
    )
})

# -----------------------------------------------------------------------------
# Write clean data
# -----------------------------------------------------------------------------

cat("Writing", nrow(all_trials), "rows from", n_distinct(all_trials$participant_id),
    "participants to clean_data.csv\n")
write_csv(all_trials, "clean_data.csv")
cat("Done.\n")
