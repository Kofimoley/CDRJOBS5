#' Calculate CDR Jobs
#' @export
calculate_cdr_jobs <- function(db_path, db_name, dat_file, scenario_list = NULL,
                               region_list = NULL, output_path = "results",
                               output_type = "csv", create_plots = TRUE,
                               job_metric = "mean", ncol = 2, nrow = 2) {
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(rgcam)

  # Define the CDR query directly in the function
  CDR_query <- '<?xml version="1.0"?>
  <queries>
    <aQuery>
      <all-regions/>
      <emissionsQueryBuilder title="CO2 sequestration by tech">
        <axis1 name="subsector">subsector</axis1>
        <axis2 name="Year">emissions-sequestered</axis2>
        <xPath buildList="true" dataName="emissions" group="false" sumAll="false">
          *[@type = "sector"]/*[@type="subsector"]/*[@type="technology"]//CO2/emissions-sequestered/node()
        </xPath>
        <comments/>
      </emissionsQueryBuilder>
    </aQuery>
  </queries>'

  # Create a temporary XML file for the query
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  # Connect to GCAM database and execute the query
  conn <- localDBConn(db_path, db_name)
  data <- tryCatch(
    {
      addScenario(conn, dat_file, scenario_list, query_file)
    },
    error = function(e) {
      message("Skipping query since it already exists in the database.")
      addScenario(conn, dat_file)  # Load existing results
    }
  )
  model_output <- getQuery(data, "CO2 sequestration by tech")

  # Print column names for debugging
  message("Column names in model_output:")
  print(colnames(model_output))

  # Filter by scenario and region if specified
  if (!is.null(scenario_list)) {
    model_output <- model_output %>% filter(scenario %in% scenario_list)
  }
  if (!is.null(region_list)) {
    model_output <- model_output %>% filter(region %in% region_list)
  }

  # Convert year and value columns to numeric
  model_output <- model_output %>%
    mutate(
      year = as.numeric(year),
      value = as.numeric(value)
    )

  # Load job intensities
  data("CDR_Job_Inten", package = "CDRJOBS5")

  # Compute job potential for all metrics
  job_results <- model_output %>%
    left_join(CDR_Job_Inten, by = c("technology" = "sub_technology")) %>%
    rowwise() %>%
    mutate(
      jobs_min = value * 3.6667e6 * min_int,
      jobs_mean = value * 3.6667e6 * mean_int,
      jobs_max = value * 3.6667e6 * max_int
    ) %>%
    ungroup()

  # Helper function to aggregate results
  aggregate_jobs <- function(df, metric) {
    list(
      total_annual_jobs = df %>%
        group_by(year, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      cumulative_jobs = df %>%
        group_by(scenario, region) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      annual_jobs_by_technology = df %>%
        group_by(year, technology, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      cumulative_jobs_by_technology = df %>%
        group_by(technology, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      global_total_annual_jobs = df %>%
        group_by(year) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      global_cumulative_jobs = df %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      global_annual_jobs_by_technology = df %>%
        group_by(year, technology) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop"),
      global_cumulative_jobs_by_technology = df %>%
        group_by(technology) %>%
        summarise(jobs = sum(.data[[metric]]), .groups = "drop")
    )
  }

  # Always calculate and save all three metrics
  metrics <- c("jobs_min", "jobs_mean", "jobs_max")
  results <- lapply(metrics, function(metric) aggregate_jobs(job_results, metric))
  metric_names <- c("min", "mean", "max")

  # Save CSV results
  lapply(seq_along(results), function(i) {
    save_path <- file.path(output_path, metric_names[i])
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    lapply(names(results[[i]]), function(name) {
      write_csv(results[[i]][[name]], file.path(save_path, paste0(name, ".csv")))
    })
  })

  # Create plots for the specified job_metric (default: "mean")
  if (create_plots) {
    metric_index <- which(metric_names == job_metric)
    save_path <- file.path(output_path, metric_names[metric_index])
    lapply(names(results[[metric_index]]), function(name) {
      visualize_results(save_path, name, job_metric, ncol, nrow)
    })
  }

  message("Results and plots saved successfully to: ", output_path)
}
