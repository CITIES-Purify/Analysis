AGGREGATION_MINUTES = 5
BEFORE_FAN_ON_MINUTES = 1 * 60
AFTER_FAN_ON_MINUTES = 1 * 60

# AGGREGATION_MINUTES = 30
# BEFORE_FAN_ON_MINUTES = 3 * 60
# AFTER_FAN_ON_MINUTES = 3 * 60

detect_fan_on_events <- function(
    data,
    id_cols,
    validation_minutes = BEFORE_FAN_ON_MINUTES,
    max_transition_gap_minutes = BEFORE_FAN_ON_MINUTES / 2,
    min_validation_rows = 2L
) {
    ordered <- data |>
        filter(!is.na(start_date)) |>
        arrange(across(all_of(c(id_cols, "start_date")))) |>
        group_by(across(all_of(id_cols))) |>
        mutate(
        .fan = as.logical(is_fan_on),
        .previous_fan = lag(.fan),
        .previous_time = lag(start_date),
        .transition_gap_minutes = as.numeric(
            difftime(
            start_date,
            .previous_time,
            units = "mins"
            )
        )
        ) |>
        ungroup()

    candidate_events <- ordered |>
        filter(
        !is.na(.previous_fan),
        !is.na(.fan),
        .previous_fan == FALSE,
        .fan == TRUE,
        .transition_gap_minutes <= max_transition_gap_minutes
        ) |>
        group_by(across(all_of(id_cols))) |>
        mutate(
        event_number = row_number(),
        event_id = paste(
            cur_group_id(),
            event_number,
            sep = "__"
        ),
        event_time = start_date
        ) |>
        ungroup() |>
        select(
        all_of(id_cols),
        event_id,
        event_number,
        event_time
        )

    validation_rows <- candidate_events |>
        inner_join(
        ordered,
        by = id_cols,
        relationship = "many-to-many"
        ) |>
        mutate(
        minutes_from_fan_on = as.numeric(
            difftime(
            start_date,
            event_time,
            units = "mins"
            )
        )
        ) |>
        filter(
        minutes_from_fan_on >= -validation_minutes,
        minutes_from_fan_on <= validation_minutes
        )

    state_is_valid <- function(state, expected_state) {
        length(state) >= min_validation_rows &&
        !anyNA(state) &&
        all(state == expected_state)
    }

    valid_events <- validation_rows |>
        group_by(
        across(
            all_of(
            c(id_cols, "event_id", "event_number", "event_time")
            )
        )
        ) |>
        summarise(
        valid_before = state_is_valid(
            .fan[
            minutes_from_fan_on >= -validation_minutes &
                minutes_from_fan_on < 0
            ],
            FALSE
        ),
        valid_after = state_is_valid(
            .fan[
            minutes_from_fan_on >= 0 &
                minutes_from_fan_on <= validation_minutes
            ],
            TRUE
        ),
        .groups = "drop"
        ) |>
        filter(valid_before, valid_after)

    candidate_events |>
        semi_join(
        valid_events,
        by = c(id_cols, "event_id")
        )
}

attach_measurements_to_events <- function(
    data,
    events,
    id_cols,
    before_minutes = BEFORE_FAN_ON_MINUTES,
    after_minutes = AFTER_FAN_ON_MINUTES
) {
    events |>
        inner_join(
        data,
        by = id_cols,
        relationship = "many-to-many"
        ) |>
        mutate(
        minutes_from_fan_on = as.numeric(
            difftime(
            start_date,
            event_time,
            units = "mins"
            )
        )
        ) |>
        filter(
        minutes_from_fan_on >= -before_minutes,
        minutes_from_fan_on <= after_minutes
        )
}

mean_or_na <- function(x) {
    if (all(is.na(x))) {
        NA_real_
    } else {
        mean(x, na.rm = TRUE)
    }
}

aggregate_event_bins <- function(
    event_rows,
    id_cols,
    value_cols,
    bin_minutes,
    baseline = c("last_pre_bin", "last_pre_observation")
) {
    baseline <- match.arg(baseline)

    event_keys <- c(
        id_cols,
        "event_id",
        "event_number",
        "event_time"
    )

    rows_with_bins <- event_rows |>
        mutate(
        relative_bin_start =
            ceiling(minutes_from_fan_on / bin_minutes) *
            bin_minutes,
        relative_bin_end =
            relative_bin_start + bin_minutes
        )

    binned <- rows_with_bins |>
        group_by(
        across(
            all_of(
            c(
                event_keys,
                "relative_bin_start",
                "relative_bin_end"
            )
            )
        )
        ) |>
        summarise(
        across(
            all_of(value_cols),
            mean_or_na
        ),
        across(
            all_of(value_cols),
            ~ sum(!is.na(.x)),
            .names = "n_{.col}"
        ),
        mean_measurement_minute = mean(
            minutes_from_fan_on,
            na.rm = TRUE
        ),
        .groups = "drop"
        )

    baselines <- rows_with_bins |>
        filter(minutes_from_fan_on < 0) |>
        group_by(
        across(all_of(c(id_cols, "event_id")))
        ) |>
        slice_max(
            order_by = start_date,
            n = 1,
            with_ties = FALSE
        ) |>
        ungroup() |>
        select(
        all_of(c(id_cols, "event_id")),
        all_of(value_cols)
        )

    baselines <- baselines |>
        rename_with(
        ~ paste0("baseline_", .x),
        all_of(value_cols)
        )

    output <- binned |>
        left_join(
        baselines,
        by = c(id_cols, "event_id")
        )

    for (value_col in value_cols) {
        output[[paste0("delta_", value_col)]] <-
        output[[value_col]] -
        output[[paste0("baseline_", value_col)]]
    }

    output
}

add_raw_event_deltas <- function(
    event_rows,
    id_cols,
    value_cols
) {
  event_keys <- c(id_cols, "event_id")

  baselines <- event_rows |>
    filter(minutes_from_fan_on < 0) |>
    group_by(across(all_of(event_keys))) |>
    slice_max(
      order_by = start_date,
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup() |>
    select(
      all_of(event_keys),
      all_of(value_cols)
    ) |>
    rename_with(
      ~ paste0("baseline_", .x),
      all_of(value_cols)
    )

  output <- event_rows |>
    left_join(
      baselines,
      by = event_keys
    )

  for (value_col in value_cols) {
    output[[paste0("delta_", value_col)]] <-
      output[[value_col]] -
      output[[paste0("baseline_", value_col)]]
  }

  output
}

prepare_fan_metric <- function(
    event_rows,
    id_cols,
    value_cols,
    bin_minutes = NULL
) {
    if (is.null(bin_minutes)) {
        add_raw_event_deltas(
        event_rows = event_rows,
        id_cols = id_cols,
        value_cols = value_cols
        )
    } else {
        aggregate_event_bins(
        event_rows = event_rows,
        id_cols = id_cols,
        value_cols = value_cols,
        bin_minutes = bin_minutes
        )
    }
}

plot_fan_on <- function(
  df,
  x,
  y,
  ylabel,
  fileName,
  group = NULL,
  groupValues = TREATMENT_FILL_VALUES,
  groupLabels = TREATMENT_LABELS,
  yBreaks = NULL,
  minute_mark_to_calc_avg = NULL
) {
  x_min <- min(df[[x]], na.rm = TRUE)
  x_max <- max(df[[x]], na.rm = TRUE)

  half_axis_minutes <- max(abs(c(x_min, x_max)))

  tick_interval <- case_when(
    half_axis_minutes <= 30 ~ 5,
    half_axis_minutes <= 60 ~ 15,
    TRUE                    ~ 30
  )

  x_breaks <- seq(
    from = ceiling(x_min / tick_interval) * tick_interval,
    to = floor(x_max / tick_interval) * tick_interval,
    by = tick_interval
  )

  x_breaks <- sort(unique(c(x_breaks, 0)))

  # Calculate the same summaries displayed by stat_summary().
  summary_group_cols <- c(x, group)

  ci_summary <- df |>
    filter(
      !is.na(.data[[x]]),
      !is.na(.data[[y]])
    ) |>
    group_by(
      across(all_of(summary_group_cols))
    ) |>
    group_modify(
      ~ as_tibble(
        as.list(mean_ci95(.x[[y]]))
      )
    ) |>
    ungroup()

  ci_min <- min(ci_summary$ymin, na.rm = TRUE)
  ci_max <- max(ci_summary$ymax, na.rm = TRUE)

  if (!is.finite(ci_min) || !is.finite(ci_max)) {
    stop("Could not calculate finite 95% confidence interval limits.")
  }

  # Calculate optional post-mark averages.
  average_line_data <- NULL

  if (!is.null(minute_mark_to_calc_avg)) {
    if (
      length(minute_mark_to_calc_avg) != 1L ||
      !is.finite(minute_mark_to_calc_avg)
    ) {
      stop("minute_mark_to_calc_avg must be one finite number.")
    }

    if (minute_mark_to_calc_avg > x_max) {
      stop(
        "minute_mark_to_calc_avg is greater than the maximum x value."
      )
    }

    average_start <- if (minute_mark_to_calc_avg < 0) {
    x_min
    } else {
    max(
        minute_mark_to_calc_avg,
        x_min
    )
    }

    average_end <- if (minute_mark_to_calc_avg < 0) {
    min(
        minute_mark_to_calc_avg,
        x_max
    )
    } else {
    x_max
    }

    average_source <- df |>
    filter(
        .data[[x]] >= average_start,
        .data[[x]] <= average_end,
        !is.na(.data[[y]])
    )

    if (nrow(average_source) == 0L) {
      stop(
        "No non-missing y values exist at or after ",
        minute_mark_to_calc_avg,
        "."
      )
    }

    if (is.null(group)) {
      average_line_data <- average_source |>
        summarise(
          average_y = mean(.data[[y]], na.rm = TRUE)
        )
    } else {
      average_line_data <- average_source |>
        filter(!is.na(.data[[group]])) |>
        group_by(
          across(all_of(group))
        ) |>
        summarise(
          average_y = mean(.data[[y]], na.rm = TRUE),
          .groups = "drop"
        )
    }

    average_line_data <- average_line_data |>
      mutate(
        line_start = average_start,
        line_end = average_end,
        label_x = (average_start + average_end) / 2,
        average_label = sprintf("%.1f", average_y)
      )
  }

  # Set the visible y range using the CI rather than raw outliers.
  visible_y_values <- c(
    ci_min,
    ci_max,
    if (!is.null(average_line_data)) {
      average_line_data$average_y
    }
  )

  visible_y_min <- min(visible_y_values, na.rm = TRUE)
  visible_y_max <- max(visible_y_values, na.rm = TRUE)
  visible_y_range <- visible_y_max - visible_y_min

  y_limits <- c(
    visible_y_min,
    visible_y_max 
  )

  annotation_y <- visible_y_max

  p <- ggplot(
    df,
    aes(
      x = .data[[x]],
      y = .data[[y]],
      color = if (is.null(group)) NULL else .data[[group]],
      fill = if (is.null(group)) NULL else .data[[group]]
    )
  ) +
    geom_vline(
      xintercept = 0,
      color = "black",
      linewidth = 1
    ) +
    stat_summary(
      fun.data = mean_ci95,
      geom = "ribbon",
      alpha = 0.2,
      linewidth = 0
    ) +
    stat_summary(
      fun = mean,
      geom = "line",
      alpha = 0.7,
      linewidth = 1.2
    ) +
    labs(
      x = "Minutes Since Fans Turned On",
      y = ylabel
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0.01, 0.01)),
    ) +
    coord_cartesian(
      ylim = y_limits
    ) +
    THEME_MINIMAL

    if (x_min < 0) {
    p <- p +
        annotate(
        "text",
        x = x_min / 2,
        y = annotation_y,
        label = "Fan Off",
        vjust = 1.2,
        size = 8
        )
    }

    if (x_max > 0) {
    p <- p +
        annotate(
        "text",
        x = x_max / 2,
        y = annotation_y,
        label = "Fan On",
        vjust = 1.2,
        size = 8
        )
    }

  # Draw the average line and label.
  if (!is.null(average_line_data)) {
    p <- p +
    geom_segment(
        data = average_line_data,
        aes(
        x = line_start,
        xend = line_end,
        y = average_y,
        yend = average_y
        ),
        inherit.aes = FALSE,
        color = "gray",
        linewidth = 1,
        linetype = "dashed",
        show.legend = FALSE
    ) +
    geom_text(
        data = average_line_data,
        aes(
        x = label_x,
        y = average_y,
        label = average_label
        ),
        inherit.aes = FALSE,
        vjust = -0.6,
        size = 8,
        show.legend = FALSE,
        color = "gray"
    )
  }

  if (!is.null(yBreaks)) {
    p <- p +
      scale_y_continuous(
        breaks = yBreaks,
        expand = expansion(mult = 0)
      )
  }

  if (!is.null(group)) {
    p <- p +
      scale_color_manual(
        name = NULL,
        values = groupValues,
        labels = groupLabels
      ) +
      scale_fill_manual(
        name = NULL,
        values = groupValues,
        labels = groupLabels
      )
  }

  output_dir <- "../visualization/fan_on"

  dir.create(
    output_dir,
    showWarnings = FALSE,
    recursive = TRUE
  )

  ggsave(
    file.path(output_dir, fileName),
    plot = p,
    width = dimension$width,
    height = dimension$height,
    dpi = 300
  )

  p
}