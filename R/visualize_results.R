#' Visualize Results
#' @export
visualize_results <- function(output_path, result_type, metric) {
  library(ggplot2)
  library(readr)

  # Load the data
  file_path <- file.path(output_path, metric, paste0(result_type, ".csv"))
  data <- read_csv(file_path)

  # Define the custom theme
  custom_theme <- theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", face = "bold", angle = 0, hjust = 0.4),
    axis.text.y = element_text(size = 8, color = "black", face = "bold"),
    axis.title.y = element_text(size = 8, color = "black", face = "bold"),
    plot.title = element_text(size = 10, color = "darkred", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "darkgreen", face = "italic"),
    plot.caption = element_text(size = 10, color = "purple", face = "italic"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

  # Create the appropriate plot based on the result type
  plot <- switch(
    result_type,
    "total_annual_jobs" = ggplot(data, aes(x = Year, y = jobs, fill = region)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
      labs(
        title = paste("Total Annual Jobs (", metric, ")", sep = ""),
        x = "Year",
        y = "Jobs"
      ) +
      custom_theme,

    "cumulative_jobs" = ggplot(data, aes(x = scenario, y = jobs, fill = region)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      labs(
        title = paste("Cumulative Jobs (", metric, ")", sep = ""),
        x = "Scenario",
        y = "Jobs"
      ) +
      custom_theme,

    "annual_jobs_by_technology" = ggplot(data, aes(x = Year, y = jobs, fill = main_technology)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      facet_wrap(~ main_technology, scales = "free") +
      labs(
        title = paste("Annual Jobs by Technology (", metric, ")", sep = ""),
        x = "Year",
        y = "Jobs"
      ) +
      custom_theme,

    "cumulative_jobs_by_technology" = ggplot(data, aes(x = main_technology, y = jobs, fill = region)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      labs(
        title = paste("Cumulative Jobs by Technology (", metric, ")", sep = ""),
        x = "Technology",
        y = "Jobs"
      ) +
      custom_theme,

    "global_total_annual_jobs" = ggplot(data, aes(x = Year, y = jobs, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
      labs(
        title = paste("Global Total Annual Jobs (", metric, ")", sep = ""),
        x = "Year",
        y = "Jobs"
      ) +
      custom_theme,

    "global_cumulative_jobs" = ggplot(data, aes(x = scenario, y = jobs, fill = region)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      labs(
        title = paste("Global Cumulative Jobs (", metric, ")", sep = ""),
        x = "Scenario",
        y = "Jobs"
      ) +
      custom_theme,

    "global_annual_jobs_by_technology" = ggplot(data, aes(x = Year, y = jobs, fill = main_technology)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      facet_wrap(~ main_technology, scales = "free") +
      labs(
        title = paste("Global Annual Jobs by Technology (", metric, ")", sep = ""),
        x = "Year",
        y = "Jobs"
      ) +
      custom_theme,

    "global_cumulative_jobs_by_technology" = ggplot(data, aes(x = main_technology, y = jobs, fill = scenario)) +
      geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
      labs(
        title = paste("Global Cumulative Jobs by Technology (", metric, ")", sep = ""),
        x = "Technology",
        y = "Jobs"
      ) +
      custom_theme,

    stop("Invalid result type")
  )

  # Save the plot
  save_path <- file.path(output_path, metric, paste0(result_type, ".png"))
  ggsave(filename = save_path, plot = plot, width = 10, height = 8)

  # Return the plot object (optional for testing)
  return(plot)
}
