#' Run Shiny App
#' @export
run_shiny_app <- function(output_path) {
  library(shiny)
  library(ggplot2)
  library(readr)
  library(dplyr)

  # List of result types and metrics
  result_types <- c(
    "total_annual_jobs", "cumulative_jobs",
    "annual_jobs_by_technology", "cumulative_jobs_by_technology",
    "global_total_annual_jobs", "global_cumulative_jobs",
    "global_annual_jobs_by_technology", "global_cumulative_jobs_by_technology"
  )
  metrics <- c("min", "mean", "max")

  # UI
  ui <- fluidPage(
    titlePanel("CDR Job Potential Viewer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("result_type", "Select Result Type", choices = result_types, selected = "total_annual_jobs"),
        selectInput("metric", "Select Metric", choices = metrics, selected = "mean"),
        selectInput("region", "Select Region(s)", choices = NULL, multiple = TRUE),
        selectInput("scenario", "Select Scenario(s)", choices = NULL, multiple = TRUE),
        numericInput("ncol", "Number of Columns for Facets", value = 2, min = 1),
        numericInput("nrow", "Number of Rows for Facets", value = 2, min = 1)
      ),
      mainPanel(
        plotOutput("jobPlot")
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    # Reactive: Load the selected data
    filtered_data <- reactive({
      file_path <- file.path(output_path, input$metric, paste0(input$result_type, ".csv"))
      data <- read_csv(file_path)

      # Update region and scenario dropdowns dynamically
      updateSelectInput(session, "region", choices = unique(data$region), selected = unique(data$region))
      updateSelectInput(session, "scenario", choices = unique(data$scenario), selected = unique(data$scenario))

      # Filter the data based on user selections
      data <- data %>%
        filter(region %in% input$region, scenario %in% input$scenario)

      return(data)
    })

    # Render the plot
    output$jobPlot <- renderPlot({
      data <- filtered_data()

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

      # Create the plot based on the selected result type
      plot <- switch(
        input$result_type,
        "total_annual_jobs" = ggplot(data, aes(x = Year, y = jobs, fill = region)) +
          geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
          labs(title = paste("Total Annual Jobs (", input$metric, ")", sep = ""), x = "Year", y = "Jobs") +
          custom_theme,

        "cumulative_jobs" = ggplot(data, aes(x = scenario, y = jobs, fill = region)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          labs(title = paste("Cumulative Jobs (", input$metric, ")", sep = ""), x = "Scenario", y = "Jobs") +
          custom_theme,

        "annual_jobs_by_technology" = ggplot(data, aes(x = Year, y = jobs, fill = main_technology)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          facet_wrap(~ main_technology, ncol = input$ncol, nrow = input$nrow) +
          labs(title = paste("Annual Jobs by Technology (", input$metric, ")", sep = ""), x = "Year", y = "Jobs") +
          custom_theme,

        "cumulative_jobs_by_technology" = ggplot(data, aes(x = main_technology, y = jobs, fill = region)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          labs(title = paste("Cumulative Jobs by Technology (", input$metric, ")", sep = ""), x = "Technology", y = "Jobs") +
          custom_theme,

        "global_total_annual_jobs" = ggplot(data, aes(x = Year, y = jobs, fill = scenario)) +
          geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
          labs(title = paste("Global Total Annual Jobs (", input$metric, ")", sep = ""), x = "Year", y = "Jobs") +
          custom_theme,

        "global_cumulative_jobs" = ggplot(data, aes(x = scenario, y = jobs, fill = region)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          labs(title = paste("Global Cumulative Jobs (", input$metric, ")", sep = ""), x = "Scenario", y = "Jobs") +
          custom_theme,

        "global_annual_jobs_by_technology" = ggplot(data, aes(x = Year, y = jobs, fill = main_technology)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          facet_wrap(~ main_technology, ncol = input$ncol, nrow = input$nrow) +
          labs(title = paste("Global Annual Jobs by Technology (", input$metric, ")", sep = ""), x = "Year", y = "Jobs") +
          custom_theme,

        "global_cumulative_jobs_by_technology" = ggplot(data, aes(x = main_technology, y = jobs, fill = scenario)) +
          geom_bar(stat = "identity", position = "stack", fill = "lightblue") +
          labs(title = paste("Global Cumulative Jobs by Technology (", input$metric, ")", sep = ""), x = "Technology", y = "Jobs") +
          custom_theme,

        stop("Invalid result type")
      )

      return(plot)
    })
  }

  # Run the Shiny app
  shinyApp(ui = ui, server = server)
}
