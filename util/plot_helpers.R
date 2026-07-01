plot_dimension <- function(is_vertical) {
    if (is_vertical) {
        list(width = 12, height = 15)
    } else {
        list(width = 15, height = 8)
    }
}

THEME_MINIMAL <- theme_minimal(base_size = 30) +
  theme(
    axis.ticks.x = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.5, "cm"),
    axis.title.x = element_text(
      margin = margin(t = 25, l = 25)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.key.size = unit(1, "cm"),
    legend.position = "top",
    legend.box = "vertical",
    legend.box.just  = "center",
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0, "cm")
  )

mean_ci95 <- function(x) {
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  t <- qt(0.975, df = sum(!is.na(x)) - 1)

  data.frame(
    y = m,
    ymin = m - t * se,
    ymax = m + t * se
  )
}