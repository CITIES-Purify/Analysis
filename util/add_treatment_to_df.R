TREATMENT_DATES <- read_csv("../raw-data/treatment_dates.csv", col_names = TRUE)

add_treatment_to_df <- function(data, only_during_intervention = TRUE) {
  treatment_dates_filtered <- if (only_during_intervention == TRUE) {
    TREATMENT_DATES |>
      filter(!treatment_boundary %in% c("before_start_first_half", "before_start_second_half")) |>
      filter(treatment != "washout")
  } else {
    TREATMENT_DATES
  }

  return(
    data |>
      left_join(
        treatment_dates_filtered,
        by = c("purifier_group", "period_id", "date")
      ) |>
      filter(!is.na(treatment)) |>
      relocate(treatment, .after = purifier_group)
  )
}
