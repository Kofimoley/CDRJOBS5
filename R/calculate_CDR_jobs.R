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
  data <- addScenario(conn, dat_file, scenario_list, query_file)
  model_output <- getQuery(data, "CO2 sequestration by tech")

  # Apply defaults for scenarios and regions
  scenario_list <- scenario_list %||% unique(model_output$scenario)
  region_list <- region_list %||% unique(model_output$region)

  # Filter and reshape data
  data_long <- model_output %>%
    filter(scenario %in% scenario_list, region %in% region_list) %>%
    pivot_longer(cols = starts_with("Year"), names_to = "Year", values_to = "value") %>%
    mutate(value = as.numeric(value))

  # Load job intensities
  data("CDR_Job_Inten", package = "CDRJOBS5")

  # Compute job potential for all metrics
  job_results <- data_long %>%
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
        group_by(Year, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]])),
      cumulative_jobs = df %>%
        group_by(scenario, region) %>%
        summarise(jobs = sum(.data[[metric]])),
      annual_jobs_by_technology = df %>%
        group_by(Year, main_technology, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]])),
      cumulative_jobs_by_technology = df %>%
        group_by(main_technology, scenario, region) %>%
        summarise(jobs = sum(.data[[metric]])),
      global_total_annual_jobs = df %>%
        group_by(Year) %>%
        summarise(jobs = sum(.data[[metric]])),
      global_cumulative_jobs = df %>%
        summarise(jobs = sum(.data[[metric]])),
      global_annual_jobs_by_technology = df %>%
        group_by(Year, main_technology) %>%
        summarise(jobs = sum(.data[[metric]])),
      global_cumulative_jobs_by_technology = df %>%
        group_by(main_technology) %>%
        summarise(jobs = sum(.data[[metric]]))
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
