process_data_file <- function(file_path, ALL_PARTICIPANTS_METADATA, only_during_intervention = TRUE) {
    # Extract filename and convert to lowercase for matching
    file_name <- tolower(basename(file_path))

    # Read data
    data <- read_csv(file_path, col_types = cols()) %>%
    rename_with(~ str_trim(.)) %>%
    rename(pseudonym = participant_pseudonym) %>%
    mutate(
        # Remove the timezone part (e.g., " +04:00") and convert to timestamp assuming UTC
        start_date = ymd_hms(gsub("\\s\\+\\d{2}\\d{2}$", "", start_date), tz = "GMT"),
        end_date = ymd_hms(gsub("\\s\\+\\d{2}\\d{2}$", "", end_date), tz = "GMT"),
        date = as.Date(start_date) # for determining treatment
    )

    # Join with pseudonym df
    data <- data %>%
        left_join(
            ALL_PARTICIPANTS_METADATA %>%
            select(pseudonym, period_id, purifier_group),
                by = "pseudonym"
        )

    return(
        add_treatment_to_df(
            data = data,
            only_during_intervention = only_during_intervention
        )
    )
}

remove_duplicate_locations <- function(data) {
    # If any row has the same `pseudonym`, `start_date`, and `end_date`
    # but more than 1 location_method,
    # then retain the row with `location_method` = SURVEY, 
    # remove the other row (`location_method` = `ble`) completely
    data <- data %>%
        group_by(pseudonym, start_date, end_date) %>%
        arrange(desc(location_method == SURVEY)) %>%  # Put SURVEY method first if it exists
        slice(1) %>%  # Keep only the first (i.e., SURVEY if available)
        ungroup()
    
    return(data)
}

remove_travel_dates <- function(data, participants_travel_dates) {
  # Add date column only if missing
  if (!"date" %in% names(data)) {
    data$date <- as.Date(data$start_date)
    cleanup <- TRUE
  } else {
    cleanup <- FALSE
  }

  data <- anti_join(data, participants_travel_dates, by = c("pseudonym", "period_id", "date"))

  if (cleanup) data$date <- NULL
  return(data)
}

identify_data_during_travel <- function(data, participants_travel_dates) {
    data %>%
        mutate(date = as.Date(start_date)) %>%
        left_join(
            participants_travel_dates %>%
                mutate(is_travel = TRUE),
            by = c("pseudonym", "period_id", "date")
        ) %>%
        mutate(is_travel = ifelse(is.na(is_travel), FALSE, is_travel)) %>%
        select(-date)
}

calculate_interval_between_consecutive_samples <- function(
  data,
  should_group_by_sample_type_id = TRUE
) {
  grouping_vars <- if (should_group_by_sample_type_id) {
    c("pseudonym", "period_id", "sample_type_id")
  } else {
    c("pseudonym", "period_id")
  }

  data |>
    arrange(across(all_of(c(grouping_vars, "start_date", "end_date")))) |>
    group_by(across(all_of(grouping_vars))) |>
    mutate(
      prev_start_date = lag(start_date),
      interval_start_start_sec = as.numeric(
        difftime(start_date, prev_start_date, units = "secs")
      ),
      prev_end_date = lag(end_date),
      interval_end_start_sec = as.numeric(
        difftime(start_date, prev_end_date, units = "secs")
      )
    ) |>
    ungroup()
}
