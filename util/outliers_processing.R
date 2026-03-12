remove_outliers_iqr_method <- function(data, value_column, group_by_column = NULL) {
    # If group_by_column is NULL, create a dummy column for grouping
    if (is.null(group_by_column)) {
        data <- data %>% mutate(.group_dummy = 1)
        group_by_column <- ".group_dummy"
        drop_dummy <- TRUE
    } else {
        drop_dummy <- FALSE
    }

    result <- data %>%
        group_by(across(all_of(group_by_column))) %>%
        mutate(
            Q1 = quantile(.data[[value_column]], 0.25, na.rm = TRUE),
            Q3 = quantile(.data[[value_column]], 0.75, na.rm = TRUE),
            IQR = Q3 - Q1,
            lower_bound = Q1 - 1.5 * IQR,
            upper_bound = Q3 + 1.5 * IQR
        ) %>%
        filter(.data[[value_column]] >= lower_bound & .data[[value_column]] <= upper_bound) %>%
        ungroup() %>%
        select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)

    if (drop_dummy) {
        result <- result %>% select(-.group_dummy)
    }

    return(result)
}