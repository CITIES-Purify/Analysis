fit_lme_models <- function(main_fixed_effect, optional_filter_by_location_type = NULL, metric_dfs) {
  imap(metric_dfs, function(df, metric_name) {
    df_filtered <- df |> mutate(
      period_id = as.factor(period_id)
    )

    # Early return for RHR and RR outside of home location
    if (!is.null(optional_filter_by_location_type) && optional_filter_by_location_type != "home" && (metric_name == RHR || metric_name == RR)){
      return(NULL)
    }

    # Early return for RHR if main_fixed_effect is treatment (only focus on home location, but one RHR measurement spans all locations)
    if (metric_name == RHR && main_fixed_effect == "treatment"){
      return(NULL)
    }

    # ---- FIXED EFFECTS ----
    # Use natural cubic splines with 3 degrees of freedom for temperature, humidity, and co2
    fixed_effects <- c(main_fixed_effect, "bmi", "biological_sex", "ns(temperature, df = 3)", "ns(humidity, df = 3)", "ns(co2, df = 3)", "dow", "heart_rate_motion_context")
    random_effects <- c("pseudonym", "date")

    # For metrics other than RHR and sleep stages
    if (!metric_name %in% c(SLEEP_STAGES_FOR_ANALYSIS) && metric_name != RHR) {
      fixed_effects <- c(fixed_effects, "ns(hour, df = 3)")

      # Filter by optional_filter_by_location_type if presented (for LME model 1 and LME model 2 sensivity model)
      if (!is.null(optional_filter_by_location_type)) {
        df_filtered <- df_filtered |> filter(location_type == optional_filter_by_location_type)
      } 
      # Else, if no optional_filter_by_location_type is applied
      else{
         # Only add "location_type" as fixed effect for "pm2.5" as main fixed effect
        if (main_fixed_effect == "pm25"){
          fixed_effects <- c(fixed_effects, "location_type")
        }   
      }     
    }

    # Scale barometric_pressure if SPO2 by z-score (mean = 0, SD = 1) --> Help convergence 
    if (metric_name == SPO2){
      df_filtered$barometric_pressure <- scale(df_filtered$barometric_pressure, center = TRUE, scale = TRUE)
      fixed_effects <- c(fixed_effects, "ns(barometric_pressure, df = 3)")
    }

    # Construct the LME formula
    fixed_formula <- paste(fixed_effects, collapse = " + ")
    random_formula <- paste(random_effects, collapse = " + ")
    formula <- as.formula(paste("log(value) ~", fixed_formula, "+ (1 |", random_formula, ")"))
    
    print(paste("Fitting model for metric:", metric_name))

    # Calculate IQR of the main_fixed_effect if it's a continuous variable to aid with interpretation
    if (!is.factor(df_filtered[[main_fixed_effect]])) {
      main_fixed_effect_iqr <- IQR(df_filtered[[main_fixed_effect]], na.rm = TRUE)
    } else {
      main_fixed_effect_iqr <- NA_real_
    }

    # Scale PM2.5 exposure by IQR to median --> Help convergence and aid with interpretation
    pm25_iqr <- IQR(df_filtered$pm25, na.rm = TRUE)
    pm25_med <- median(df_filtered$pm25, na.rm = TRUE)
    df_filtered$pm25 <- (df_filtered$pm25 - pm25_med) / pm25_iqr

    # Scale all other continuous variables by z-score (mean = 0, SD = 1) --> Help convergence 
    df_filtered$bmi <- scale(df_filtered$bmi, center = TRUE, scale = TRUE)
    df_filtered$temperature <- scale(df_filtered$temperature, center = TRUE, scale = TRUE)
    df_filtered$humidity <- scale(df_filtered$humidity, center = TRUE, scale = TRUE)
    df_filtered$co2 <- scale(df_filtered$co2, center = TRUE, scale = TRUE)
    df_filtered$heart_rate_motion_context <- scale(df_filtered$heart_rate_motion_context, center = TRUE, scale = TRUE)

    model <- lmer(formula = formula, data = df_filtered)

    return(list(model = model, main_fixed_effect_iqr = main_fixed_effect_iqr, df_used = df_filtered))
  }) |> compact()
}

extract_model_info <- function(model, main_fixed_effect_iqr, df_used, metric_name, main_fixed_effect = NULL) {
  formular <- formula(model)

  # Confidence intervals
  lme_model_confint <- as.data.frame(confint(model, method = "Wald")) # use 'profile' results in error in heart-rate, too unstable
  lme_model_confint$term <- rownames(lme_model_confint)
  lme_model_confint$metric <- metric_name
  
  # Fixed effects
  fixed_effects <- broom.mixed::tidy(model, effects = "fixed", conf.int = FALSE)
  
  fixed_effects <- fixed_effects %>%
    left_join(
      lme_model_confint %>% select(term, `2.5 %`, `97.5 %`),
      by = "term"
    ) %>%
    relocate(`2.5 %`, `97.5 %`, .after = estimate) %>%
    mutate(
      percent_change = ifelse(term != "(Intercept)", (exp(estimate) - 1) * 100, NA_real_),
      percent_change_low = ifelse(term != "(Intercept)", (exp(`2.5 %`) - 1) * 100, NA_real_),
      percent_change_high = ifelse(term != "(Intercept)", (exp(`97.5 %`) - 1) * 100, NA_real_)
    )

  if (!is.null(main_fixed_effect)) {
    fixed_effects <- fixed_effects %>%
      mutate(
        percent_change_iqr = ifelse(term == main_fixed_effect,
                                        (exp(estimate ) - 1)*100,
                                        NA_real_),
        percent_change_iqr_low = ifelse(term == main_fixed_effect,
                                        (exp(`2.5 %`   ) - 1)*100,
                                        NA_real_),
        percent_change_iqr_high= ifelse(term == main_fixed_effect,
                                        (exp(`97.5 %`  ) - 1)*100,
                                        NA_real_)
      )
  }

  # Random effects
  random_effects <- broom.mixed::tidy(model, effects = "ran_pars", conf.int = FALSE) %>%
    rename(std.dev = estimate) %>%
    mutate(variance = std.dev^2, .before = std.dev)
  
  # Now match confidence intervals by row number
  random_effects <- random_effects %>%
    bind_cols(
      lme_model_confint[1:nrow(.), c("2.5 %", "97.5 %")]
    ) %>%
    relocate(`2.5 %`, `97.5 %`, .after = std.dev)
  
  # General info
  glance_info <- broom.mixed::glance(model)
  general_info <- tibble::tibble(
    metric = metric_name,
    REMLcrit = glance_info$REMLcrit,
    residual_stddev = summary(model)$sigma,
    nobs = nobs(model)
  )

  # Residuals
  conditional_resid <- resid(model)

  # Extract all random effects as a named list
  random_effects_list <- ranef(model)

  emm_exp <- NULL
  if (!is.null(main_fixed_effect) && main_fixed_effect == "treatment") {
    # Get estimated marginal means (on log scale)
    emm <- emmeans(model, ~ treatment)
    # Back-transform to original scale
    emm_exp <- summary(emm, type = "response")
  }

  # Overall adjusted marginal mean (response scale)
  emm_overall <- emmeans(model, ~ 1)
  emm_overall_resp <- summary(emm_overall, type = "response")

  print(paste("Adjusted overall mean for", metric_name, "=", 
              round(emm_overall_resp$response, 2)))

  list(
    model = model,
    main_fixed_effect_iqr = main_fixed_effect_iqr,
    df_used = df_used,
    formular = formular,
    general_info = general_info,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    conditional_resid = conditional_resid,
    random_effects_list = random_effects_list,
    emmeans = emm_exp
  )
}

adjust_pvalues_bh <- function(lme_summaries, main_fixed_effect = NULL) {
  if (is.null(main_fixed_effect)) {
    return(lme_summaries)
  }
  
  # Collect p-values for the main_fixed_effect across all metrics
  pvals <- sapply(lme_summaries, function(summary) {
    fe <- summary$fixed_effects
    row <- fe %>% filter(term == main_fixed_effect)
    if (nrow(row) == 0) return(NA_real_)
    row$p.value
  })
  
  # Perform BH adjustment only on non-NA p-values
  valid_idx <- which(!is.na(pvals))
  adj_pvals <- rep(NA_real_, length(pvals))
  adj_pvals[valid_idx] <- p.adjust(pvals[valid_idx], method = "BH")
  
  # Add adjusted p-values to each summary
  for (i in seq_along(lme_summaries)) {
    fe <- lme_summaries[[i]]$fixed_effects
    
    # Initialize adjusted p-value column if missing
    if (!"adjusted_p.value" %in% colnames(fe)) {
      fe$adjusted_p.value <- NA_real_
    }
    
    # Update only the target term's adjusted p-value
    idx <- which(fe$term == main_fixed_effect)
    if (length(idx) == 1) {
      fe$adjusted_p.value[idx] <- adj_pvals[i]
    }
    
    lme_summaries[[i]]$fixed_effects <- fe
  }
  
  lme_summaries
}

view_lme_summary_details <- function(lme_summaries) {
    for (metric_name in names(lme_summaries)) {
        lme_summaries[[metric_name]]$formular |> View()
        lme_summaries[[metric_name]]$general_info |> View()
        lme_summaries[[metric_name]]$fixed_effects |> View()
        lme_summaries[[metric_name]]$random_effects |> View()
        lme_summaries[[metric_name]]$emmeans |> View()
        print(paste("main_fixed_effect_iqr:", lme_summaries[[metric_name]]$main_fixed_effect_iqr))
    }
}

plot_fixed_effect_percentage <- function(main_fixed_effect, summaries, health_metric_names, is_black_bg, title, flip_direction = FALSE, optional_filter_by_location_type = NULL, file_name) {
    # Collect data for effect across all metrics
    effects <- lapply(names(summaries), function(metric_name) {
        fe <- summaries[[metric_name]]$fixed_effects
        row <- fe |> filter(term == main_fixed_effect)
        if (nrow(row) == 0) return(NULL)
        data.frame(
            metric_name = metric_name,
            percent_change = row$percent_change,
            percent_change_low = row$percent_change_low,
            percent_change_high = row$percent_change_high,
            p.value = row$adjusted_p.value
        )
    }) |> bind_rows()

    # Significance logic
    effects <- effects |>
      mutate(
        stars = case_when(p.value < 0.001 ~ "***", p.value < 0.01  ~ "**", p.value < 0.05  ~ "*", TRUE ~ ""),
        sig = case_when(p.value < 0.001 ~ "< 0.001", TRUE ~ paste0("= ", sprintf("%.3f", p.value))),
        p_string = paste0("p ", sig, stars),
        metric_label = paste0(health_metric_names[metric_name], " (", p_string, ")")
      ) |>
      arrange(desc(metric_label)) |>
      mutate(metric_label = factor(metric_label, levels = unique(metric_label)))

    # Theme colors
    point_color <- if (is_black_bg) "white" else "black"
    text_color <- point_color
    grid_color <- if (is_black_bg) "gray30" else "gray80"
    bg_fill <- if (is_black_bg) "black" else "white"

    if (flip_direction) {
        effects <- effects |> mutate(
            percent_change = -percent_change,
            tmp_low = -percent_change_high,
            percent_change_high = -percent_change_low,
            percent_change_low = tmp_low
        ) |> select(-tmp_low)
    }

    # --- AXIS LOGIC ---
    min_val <- min(effects$percent_change_low, na.rm = TRUE)
    max_val <- max(effects$percent_change_high, na.rm = TRUE)

    # Determine step based on distance from 0
    max_abs_dist <- max(abs(c(min_val, max_val)))
    major_step <- if (max_abs_dist >= 5) 5 else 1

    # Ensure both major and minor sequences cover the exact same expanded range
    limit_low <- floor(min_val / major_step) * major_step
    limit_high <- ceiling(max_val / major_step) * major_step

    major_breaks <- seq(limit_low, limit_high, by = major_step)
    all_units    <- seq(limit_low, limit_high, by = 1) # Minor ticks now "catch up"

    # Plot
    p <- ggplot(effects, aes(x = metric_label, y = percent_change)) +
        # Major grid lines: crossing the whole graph
        geom_hline(yintercept = major_breaks, color = grid_color, size = 0.5) +
        geom_point(size = 5, color = point_color) +
        geom_errorbar(aes(ymin = percent_change_low, ymax = percent_change_high), 
                      width = 0.5, color = point_color, size = 1) +
        # The zero line
        geom_hline(yintercept = 0, linetype = "solid", color = text_color, size = 1) +
        coord_flip() +
        labs(x = NULL, y = title) +
        scale_y_continuous(
                breaks = all_units, 
                labels = function(x) ifelse(x %in% major_breaks, as.character(x), ""),
                limits = c(limit_low, limit_high) # Force the axis to show the full range
            ) +
        theme_minimal(base_size = 30) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),

            # Match tick color to the grid line color
            axis.ticks.x = element_line(color = grid_color, size = 0.5), 
            axis.ticks.length.x = unit(0.5, "cm"),
            axis.text.x = element_text(angle = 0, hjust = 0.5, color = text_color),

            plot.background = element_rect(fill = bg_fill, color = NA),
            axis.text = element_text(color = text_color),
            axis.title = element_text(color = text_color)
        )

    # Save logic
    dir.create("../visualization", showWarnings = FALSE)
    ggsave(filename = file.path("../visualization", paste0(file_name)), 
           plot = p, width = 15, height = 5, dpi = 300)
    print("Saved image")
    options(repr.plot.width = 15, repr.plot.height = 5)
    p
}

plot_lme_residuals_vs_fitted <- function(lme_summary_list) {
    for (metric_name in names(lme_summary_list)) {
        main_title <- metric_name
        print(plot(lme_summary_list[[metric_name]]$model, which = 3, main = main_title))
    }
}

plot_lme_diagnostics <- function(lme_summary_list) {
  par(mfrow = c(2, 2)) # 2x2 plot layout

  for (i in seq_along(lme_summary_list)) {
    conditional_resid <- lme_summary_list[[i]]$conditional_resid
    metric_name <- names(lme_summary_list)[i]

    hist(conditional_resid, breaks = 30,
       main = paste("Conditional Residuals:", metric_name),
       xlab = "Residuals", col = "lightblue")
    qqnorm(conditional_resid, main = paste("QQ Plot:", metric_name))
    qqline(conditional_resid, col = "red")

    # Random intercepts for each grouping factor
    random_effects_list <- lme_summary_list[[i]]$random_effects_list
    for (re_name in names(random_effects_list)) {
      re_vals <- random_effects_list[[re_name]][, 1]
      hist(re_vals,
         main = paste("Random Intercepts (", re_name, "):", metric_name),
         xlab = "Intercept Deviation", col = "salmon")
      qqnorm(re_vals, main = paste("Q-Q Plot of Random Intercepts (", re_name, "):", metric_name))
      qqline(re_vals, col = "darkred")
    }
  }

  par(mfrow = c(1, 1)) # reset layout
}

make_reporting_treatment_df <- function(lme_summaries,
                                        trt1 = "treatment1",
                                        trt2 = "treatment2",
                                        trt2_term = "treatmenttreatment2",
                                        metric_labels
                                        ) {
  
  df <- map_dfr(names(lme_summaries), function(metric) {
    sumry <- lme_summaries[[metric]]
    
    # 1) Estimated marginal means ± SE for treatment1 and treatment2
    emm_df <- sumry$emmeans
    row1 <- emm_df %>% filter(treatment == trt1)
    row2 <- emm_df %>% filter(treatment == trt2)
    
    em1 <- row1$response
    se1 <- row1$SE
    em2 <- row2$response
    se2 <- row2$SE
    
    em1_str <- sprintf("$%.2f~\\pm~%.2f$", em1, se1)
    em2_str <- sprintf("$%.2f~\\pm~%.2f$", em2, se2)
    
    # 2) Percent change with 95% CI
    fe <- sumry$fixed_effects
    pct_row <- fe %>% filter(term == trt2_term)
    
    pc   <- pct_row$percent_change
    low  <- pct_row$percent_change_low
    high <- pct_row$percent_change_high
    pct_str <- sprintf("$%+1.2f\\%%$ $[%+.2f, %+.2f]$", pc, low, high)
    
    # 3) Adjusted p-value (formatted)
    # Format p-value, stars, and LaTeX-safe string
    p_adjusted <- pct_row$adjusted_p.value

    star <- case_when(
      is.na(p_adjusted)      ~ "",
      p_adjusted < 0.001     ~ "$^{***}$",
      p_adjusted < 0.01      ~ "$^{**}$",
      p_adjusted < 0.05      ~ "$^*$",
      TRUE                   ~ ""
    )

    pval_text <- case_when(
      is.na(p_adjusted)      ~ "NA",
      p_adjusted < 0.001     ~ "<0.001",
      TRUE                   ~ sprintf("%.3f", p_adjusted)
    )

    p_value_str <- case_when(
      is.na(p_adjusted)      ~ "NA",
      p_adjusted < 0.05      ~ paste0("\\textbf{", pval_text, star, "}"),
      TRUE                   ~ paste0(pval_text, star)
    )
    
    data.frame(
      metric = metric_labels[metric],
      emmean1_pm_se = em1_str,
      emmean2_pm_se = em2_str,
      pct_change_iqr = pct_str,
      p_value = p_value_str,
      stringsAsFactors = FALSE
    )
  })
  
  rownames(df) <- NULL  # Ensure no row names sneak in
  df <- df |> arrange(metric)
}

make_reporting_pm25_df <- function(lme_summaries, pm25_term = "pm25", flip_direction = FALSE, metric_labels) {
  df <- map_dfr(names(lme_summaries), function(metric) {
    sumry <- lme_summaries[[metric]]
    fe <- sumry$fixed_effects

    pct_row <- fe %>% filter(term == pm25_term)

    # Flip the direction of the numerical value only if flip_direction is TRUE
    if (flip_direction) {
      pc   <- -pct_row$percent_change
      low  <- -pct_row$percent_change_high
      high <- -pct_row$percent_change_low
    } else {
      pc   <- pct_row$percent_change
      low  <- pct_row$percent_change_low
      high <- pct_row$percent_change_high
    }

    pct_str <- sprintf("$%+1.2f\\%%$ $[%+.2f, %+.2f]$", pc, low, high)

    # Format p-value, stars, and LaTeX-safe string
    p_adjusted <- pct_row$adjusted_p.value

    star <- case_when(
      is.na(p_adjusted)      ~ "",
      p_adjusted < 0.001     ~ "$^{***}$",
      p_adjusted < 0.01      ~ "$^{**}$",
      p_adjusted < 0.05      ~ "$^*$",
      TRUE                   ~ ""
    )

    pval_text <- case_when(
      is.na(p_adjusted)      ~ "NA",
      p_adjusted < 0.001     ~ "<0.001",
      TRUE                   ~ sprintf("%.3f", p_adjusted)
    )

    p_value_str <- case_when(
      is.na(p_adjusted)      ~ "NA",
      p_adjusted < 0.05      ~ paste0("\\textbf{", pval_text, star, "}"),
      TRUE                   ~ paste0(pval_text, star)
    )

    data.frame(
      metric = metric_labels[metric],
      main_fixed_effect_iqr = round(sumry$main_fixed_effect_iqr, 1),
      pct_change_iqr = pct_str,
      p_value = p_value_str,
      stringsAsFactors = FALSE
    )
  })

  rownames(df) <- NULL
  df <- df |> arrange(metric)
}
