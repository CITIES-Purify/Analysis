source("../util/outliers_processing.R")
source("../util/samples_processing.R")

PARTICIPANTS_TRAVEL_DATES <- read_rds("../R-table/PARTICIPANTS_TRAVEL_DATES.rds")

# Summarize each metric for each participant
process_and_summarize_each_metric_each_participant <- function(
    data, 
    group_by_cols,
    should_remove_travel = TRUE,
    should_remove_iqr_outlier = TRUE
    ) {

    # Remove travel dates once, before looping through metrics
    if (should_remove_travel) {
        data <- remove_travel_dates(
            data = data,
            participants_travel_dates = PARTICIPANTS_TRAVEL_DATES
        ) |> 
        ungroup()
    }

    # Dynamically generate numeric columns, excluding grouping columns
    numeric_metrics <- data |>
        select(where(is.numeric), -any_of(group_by_cols)) |>
        names()

    cleaned_list <- lapply(numeric_metrics, function(metric) {

        data_metric <- data

        # Optionally remove outliers for this metric
        if (should_remove_iqr_outlier) {
            data_metric <- remove_outliers_iqr_method(
                data = data_metric,
                value_column = metric,
                group_by_column = group_by_cols
            ) |> 
            ungroup()
        }

        # Always summarise, regardless of whether outliers were removed
        data_metric |>
            group_by(across(any_of(group_by_cols))) |>
            summarise(
                !!metric := mean(.data[[metric]], na.rm = TRUE),
                .groups = "drop"
            )
    })

    Reduce(
        function(x, y) {
            full_join(
                x,
                y,
                by = group_by_cols
            )
        },
        cleaned_list
    )
}

# Calculate pair-wise difference for each metric for each participant
treatment_diff <- function(
  summary_per_participant,
  summarise_by
) {

  cols <- c("pseudonym", "type_id")

  treatments <- unique(summary_per_participant$treatment)

  if (length(treatments) != 2) {
    stop(
      "Expected exactly two treatment levels, found: ",
      paste(treatments, collapse = ", ")
    )
  }

  treatment_a <- treatments[1]
  treatment_b <- treatments[2]

  df <- summary_per_participant |>
    group_by(across(any_of(cols)), treatment) |>
    summarise(
      across(any_of(summarise_by), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    group_by(across(any_of(cols))) |>
    filter(n_distinct(treatment) > 1) |>
    ungroup() |>
    pivot_wider(
      names_from = treatment,
      values_from = any_of(summarise_by),
      names_glue = "{.value}_{treatment}"
    ) |>
    mutate(across(
      ends_with(paste0("_", treatment_b)),
      ~ .x - get(str_replace(
        cur_column(),
        paste0("_", treatment_b, "$"),
        paste0("_", treatment_a)
      )),
      .names = "{.col}_diff"
    )) |>
    rename_with(
      ~ str_replace(.x, paste0("_", treatment_b, "_diff$"), "_diff"),
      ends_with(paste0("_", treatment_b, "_diff"))
    ) |>
    select(
      any_of(cols),
      ends_with(paste0("_", treatment_a)),
      ends_with(paste0("_", treatment_b)),
      ends_with("_diff")
    )

  return(df)
}

# Shapiro-Wilk test
calc_shapiro_wilk <- function(treatment_diff) {
  results <- list()
  i <- 1

  for (type in unique(treatment_diff$type_id)) {

    df <- treatment_diff |>
      filter(type_id == type)

    if (nrow(df) == 0) next

    for (metric in names(df)[grepl("_diff$", names(df))]) {

      differences <- df[[metric]]
      non_na_values <- differences[!is.na(differences)]

      n_non_na <- length(non_na_values)
      n_unique <- length(unique(non_na_values))

      # Shapiro-Wilk requires at least 3 non-NA values
      if (n_non_na < 3) {
        results[[i]] <- data.frame(
          type_id = type,
          metric = metric,
          W = NA_real_,
          p_value = NA_real_,
          normality = "",
          note = "fewer than 3 non-NA differences",
          stringsAsFactors = FALSE
        )
        i <- i + 1
        next
      }

      # Shapiro-Wilk fails when all values are identical
      if (n_unique < 2) {
        results[[i]] <- data.frame(
          type_id = type,
          metric = metric,
          W = NA_real_,
          p_value = NA_real_,
          normality = "",
          note = "all differences identical",
          stringsAsFactors = FALSE
        )
        i <- i + 1
        next
      }

      test_result <- shapiro.test(non_na_values)

      results[[i]] <- data.frame(
        type_id = type,
        metric = metric,
        W = as.numeric(test_result$statistic),
        p_value = test_result$p.value,
        normality = ifelse(test_result$p.value > SIG_LEVEL, "v", ""),
        note = "",
        stringsAsFactors = FALSE
      )

      i <- i + 1
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      type_id = character(),
      metric = character(),
      W = numeric(),
      p_value = numeric(),
      normality = character(),
      note = character()
    ))
  }

  df <- do.call(rbind, results)
  rownames(df) <- NULL
  return(df)
}

# Paired-test depending on normality + Effect Size if significant
# normal: paired t-test
# non-normal: wilcoxon signed rank
calc_paired_stat_tests_from_diff <- function(
  treatment_diff,
  shapiro_diff_results,
  type_labels
) {

  results <- list()
  i <- 1

  has_metric_col <- "metric" %in% names(shapiro_diff_results)

  # --- PHASE 1: Run Statistical Tests & Collect Raw P-values ---
  for (type in unique(treatment_diff$type_id)) {

    df <- treatment_diff |>
      filter(type_id == type)

    if (nrow(df) == 0) next

    for (current_metric_diff in names(df)[grepl("_diff$", names(df))]) {

      differences <- df[[current_metric_diff]]
      non_na_values <- differences[!is.na(differences)]

      # Need at least 3 paired observations
      if (length(non_na_values) < 3) next

      # Look up normality of differences
      shapiro_sub <- shapiro_diff_results |>
        filter(type_id == type)

      if (has_metric_col) {
        shapiro_sub <- shapiro_sub |>
          filter(metric == current_metric_diff)
      }

      normality_val <- shapiro_sub |>
        pull(normality)

      # Important: use isTRUE() so NA does not break if()
      is_normal <- length(normality_val) == 1 && isTRUE(normality_val == "v")

      n_unique <- length(unique(non_na_values))

      # Case 1: all differences are zero
      if (n_unique == 1 && unique(non_na_values) == 0) {

        results[[i]] <- data.frame(
          type_id = type,
          type_label = type_labels[type],
          metric_diff_col = current_metric_diff,
          metric = sub("_diff$", "", current_metric_diff),
          is_normal = FALSE,
          test = "constant zero difference",
          statistic = 0,
          p_value = 1,
          stringsAsFactors = FALSE
        )

        i <- i + 1
        next
      }

      # Case 2: run paired-difference test safely
      test_result <- tryCatch(
        {
          if (is_normal && n_unique > 1) {
            t.test(non_na_values, mu = 0)
          } else {
            wilcox.test(
              non_na_values,
              mu = 0,
              exact = FALSE,
              correct = FALSE
            )
          }
        },
        error = function(e) NULL
      )

      if (is.null(test_result)) {
        results[[i]] <- data.frame(
          type_id = type,
          type_label = type_labels[type],
          metric_diff_col = current_metric_diff,
          metric = sub("_diff$", "", current_metric_diff),
          is_normal = is_normal,
          test = "test failed",
          statistic = NA_real_,
          p_value = NA_real_,
          stringsAsFactors = FALSE
        )

        i <- i + 1
        next
      }

      results[[i]] <- data.frame(
        type_id = type,
        type_label = type_labels[type],
        metric_diff_col = current_metric_diff,
        metric = sub("_diff$", "", current_metric_diff),
        is_normal = is_normal,
        test = ifelse(is_normal, "paired t-test", "wilcoxon signed-rank"),
        statistic = unname(test_result$statistic),
        p_value = test_result$p.value,
        stringsAsFactors = FALSE
      )

      i <- i + 1
    }
  }

  if (length(results) == 0) {
    return(data.frame())
  }

  # --- PHASE 2: Adjust P-values ---
  df_out <- do.call(rbind, results)
  rownames(df_out) <- NULL

  df_out$p_adjusted <- p.adjust(df_out$p_value, method = "BH")

  # --- PHASE 3: Calculate Effect Size only for significant, valid adjusted p-values ---
  df_out$effect_size <- NA_real_
  df_out$effect_size_type <- NA_character_

  for (k in 1:nrow(df_out)) {

    # Critical fix: skip NA adjusted p-values
    if (is.na(df_out$p_adjusted[k])) next

    if (df_out$p_adjusted[k] < SIG_LEVEL) {

      curr_type <- df_out$type_id[k]
      curr_diff_col <- df_out$metric_diff_col[k]
      curr_normal <- df_out$is_normal[k]

      base_metric <- str_remove(curr_diff_col, "_diff$")

      candidate_cols <- names(treatment_diff)[
        str_starts(names(treatment_diff), paste0(base_metric, "_")) &
          !str_ends(names(treatment_diff), "_diff")
      ]

      if (length(candidate_cols) != 2) {
        warning(
          "Expected exactly 2 treatment columns for metric ",
          base_metric,
          ", found: ",
          paste(candidate_cols, collapse = ", ")
        )
        next
      }

      col_t1 <- candidate_cols[1]
      col_t2 <- candidate_cols[2]

      df_raw <- treatment_diff |>
        filter(type_id == curr_type)

      vec_t1 <- df_raw[[col_t1]]
      vec_t2 <- df_raw[[col_t2]]

      valid_pairs <- !is.na(vec_t1) & !is.na(vec_t2)
      v1 <- vec_t1[valid_pairs]
      v2 <- vec_t2[valid_pairs]

      # Skip effect size if there are too few valid pairs
      if (length(v1) < 3 || length(v2) < 3) next

      # Skip effect size if both vectors are identical
      if (all(v1 == v2, na.rm = TRUE)) next

      if (isTRUE(curr_normal)) {
        es <- cohen.d(v1, v2, paired = TRUE)
        df_out$effect_size[k] <- es$estimate
        df_out$effect_size_type[k] <- "Cohen's d"
      } else {
        es <- cliff.delta(v1, v2)
        df_out$effect_size[k] <- es$estimate
        df_out$effect_size_type[k] <- "Cliff's delta"
      }
    }
  }

  df_out <- df_out |>
    select(-type_id, -metric_diff_col, -is_normal)

  df_out
}

# Summarize each metric across all participant
summarize_across_all_participants <- function(
    data,
    summarise_by
    ) {
  summary <- data |> 
    group_by(type_id, treatment) |>
    summarise(
        across(
            any_of(summarise_by),
            list(
                mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE),
                median = ~ median(.x, na.rm = TRUE),
                q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
                q75 = ~ quantile(.x, 0.75, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        ),
      .groups = "drop"
    ) |>
  pivot_longer(
    cols = -c(type_id, treatment),
    names_to = c("metric", "statistic"),
    names_sep = "_",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) |>
  filter(
    if_all(where(is.numeric), ~ !is.nan(.x) & !is.na(.x))
  ) |>
  ungroup()

  # Drop metric column if only one variable was summarized
  if (length(summarise_by) == 1) {
    summary <- summary |> select(-metric)
  }

  return(summary)
}

#  ---- Format result table for Latex ----
# format output from summarize_across_all_participants()
format_summary <- function(df, type_labels, precision = 2){
  
  # Create the format string dynamically, e.g., "%.1f" if precision is 1
  fmt <- paste0("%.", precision, "f")

  # 1. Format the strings
  summary_formatted <- df |> 
    group_by(type_id) |>
    mutate(
      mean_sd = sprintf(paste0(fmt, " $\\pm$ ", fmt), mean, sd),
      median_iqr = sprintf(paste0(fmt, " [", fmt, ", ", fmt, "]"), median, q25, q75),
      type_label = type_labels[type_id]
    ) |>
    ungroup()

  # 2. Pivot to wide format
  # We select ONLY the ID cols and value cols before pivoting.
  # This drops 'mean', 'sd', etc., allowing the rows to merge.
  summary_wide <- summary_formatted |> 
    select(
      any_of(c("type_label", "metric")),
      treatment,                               # The column to pivot
      mean_sd, median_iqr                      # The values
    ) |>
    pivot_wider(
      names_from = treatment,
      values_from = c(mean_sd, median_iqr),
      names_sep = "_"
    ) |>
    arrange(type_label)

  return(summary_wide)
}

# format output from format_summary()
make_latex_summary_table <- function(formatted_summary, stat_test_results, caption, label, align, colnames, metric_labels = NULL) {
    # Determine join keys dynamically
    join_keys <- "type_label"
    if ("metric" %in% names(formatted_summary) && "metric" %in% names(stat_test_results)) {
    join_keys <- c("type_label", "metric")
    }

    df <- formatted_summary |>
    left_join(
        stat_test_results |> select(any_of(c("type_label", "metric", "test", "p_adjusted"))),
        by = join_keys
    )

    df <- df |> mutate(
      # Significance stars
      star = case_when(
        p_adjusted < 0.001 ~ "$^{***}$",
        p_adjusted < 0.01  ~ "$^{**}$",
        p_adjusted < 0.05  ~ "$^*$",
        TRUE               ~ ""
      ),
      
      # P-value text
      pval_text = case_when(
        p_adjusted < 0.001 ~ "<0.001",
        TRUE               ~ sprintf("%.3f", p_adjusted)
      ),
      
      # Test type notes
      test_note = case_when(
        test == "paired t-test" ~ "\\tnote{t}",
        test == "wilcoxon signed-rank" ~ "\\tnote{w}",
        TRUE ~ ""
      ),
      
      # Final column with bolding if p < 0.05
      `P value` = case_when(
        p_adjusted < 0.05 ~ paste0("\\textbf{", pval_text, star, test_note, "}"),
        TRUE ~ paste0(pval_text, star, test_note)
      )
    )

    df <- df |>
      select(-pval_text, -test_note, -star, -test, -p_adjusted)

    # Add metric_label if provided
    if (!is.null(metric_labels)) {
        df <- df |>
        mutate(
            metric = metric_labels[metric]
        ) |> 
        rename(metric_label = metric)
    }

    colnames(df) <- colnames

    # Calculate number of left-aligned column
    n_left <- sum(strsplit(align, "")[[1]] == "l")

    # header vector structure: [ID cols], [Mean group], [Median group], [P-val]
    header_vector <- setNames(
      c(n_left, 2, 2, 1), 
      c(" ", MEAN_PM_SD, MEDIAN_Q1_Q3, " ")
    )
                          
    latex_tbl <- kable(
      df, "latex", booktabs = TRUE, escape = FALSE,
      caption = caption,
      align = align,
      label = label
                      ) |>
      add_header_above(header_vector, escape = FALSE) |>
      kable_styling(latex_options = c("hold_position", "repeat_header"))

    print(latex_tbl)

    return(df)
}