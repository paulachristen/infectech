# Install and load necessary packages
library(remotes)
remotes::install_github("epiforecasts/scoringutils",
                        dependencies = TRUE,
                        force = TRUE)
library(scoringutils)
library(tidyverse)
library(readr)
library(dplyr)

#' Prepare forecast data for scoring
#'
#' Prepare forecast data for scoring using scoringutils::as_forecast,
#' which creates a forecast object (see ?scoringutils::as_forecast).
#' @param data A data frame containing the forecast data.
#' @param forecast_type The type of forecast ("point","quantile","sample").
#' @param observed_column The name of the column containing observed values.
#' @param predicted_column The name of the column containing predicted values.
#' @param forecast_date The name of the column containing the forecast date.
#' @param forecast_made The name of the column containing the date the forecast
#' was made.
#' @param metric The name of the metric being forecasted (e.g., cases, deaths,
#' etc.).
#' @param other_characteristic_columns (optional) Other columns to be used
#' as characteristics.
#' @return A forecast object.
prep_forecast_data <- function(data, ...) {
  UseMethod("prep_forecast_data", data)
}

# Prepare forecast data for scoring
prep_forecast_data <- function(data, forecast_type, ...) {
  if (forecast_type == "quantile") {
    result <- prep_forecast_data.quantile(data,
                                          forecast_type = forecast_type, ...)
  } else if (forecast_type == "point") {
    result <- prep_forecast_data.point(data,
                                       forecast_type = forecast_type, ...)
  } else if (forecast_type == "sample") {
    result <- prep_forecast_data.sample(data,
                                        forecast_type = forecast_type, ...)
  } else {
    stop(paste("Unsupported:", forecast_type))
  }
  return(result)
}

#' Clean Numeric Columns
#'
#' Cleans numeric columns by removing parentheses, converting to numeric,
#' and handling NaN values.
#'
#' @param data A data frame.
#' @param numeric_columns A character vector of column names to clean.
#' @return A data frame with cleaned numeric columns.
clean_numeric_columns <- function(data, numeric_columns) {
  data %>%
    dplyr::mutate(across(any_of(numeric_columns),
                         ~ as.numeric(gsub("\\(|\\)", "", .x))),
                  dplyr::mutate(across(any_of(numeric_columns), ~ ifelse(
                    grepl("^n\\.?a\\.?n\\.?$", .x,
                          ignore.case = TRUE), NA, .x
                  ))))
}

#' Prepare quantile forecast data
#'
#' @param data A data frame containing the forecast data.
#' @param forecast_type The type of forecast ("quantile").
#' @param observed_column The name of the column containing observed values.
#' @param predicted_column The name of the column containing predicted values.
#' @param quantile_columns The name(s) of the column(s) containing
#' quantiles (long format) or prefixes for quantile columns (wide format).
#' @param quantile_values Numeric values of the quantiles if the input
#' is in wide format.
#' @param forecast_date The name of the column containing the forecast date.
#' @param forecast_made The name of the column containing the date the
#' forecast was made.
#' @param metric The name of the metric being forecasted.
#' @param other_characteristic_columns (optional) Other columns to be
#' used as characteristics.
#' @return A forecast_quantile object.
prep_forecast_data.quantile <- function(data,
                                        forecast_type = "quantile",
                                        observed_column,
                                        predicted_column,
                                        quantile_columns,
                                        quantile_values,
                                        forecast_date,
                                        forecast_made,
                                        metric,
                                        other_characteristic_columns = NULL) {
  # Input validation
  stopifnot(
    all(quantile_values > 0 & quantile_values < 1),
    all(quantile_columns %in% colnames(data)), !is.null(quantile_values) ||
      length(quantile_columns) == 1,
    inherits(data[[forecast_date]], "Date"),
    inherits(data[[forecast_made]], "Date")
  )

  # Data cleaning (including quantile columns)
  numeric_cols <- c(observed_column, quantile_columns)
  data <- clean_numeric_columns(data, numeric_cols)

  data$statistical_measure <- forecast_type

  if (length(quantile_columns) > 1) {
    data <- wide_to_long_quantiles(data, quantile_columns, quantile_values)
  } else {
    data <- data %>%
      rename(quantile_level = all_of(quantile_values)) %>%
      rename(predicted_column = all_of(quantile_columns)) %>%
      dplyr::mutate(Variable = gsub("_", ".", Variable))
  }

  forecast_data <- data %>%
    rename(
      observed = !!observed_column,
      forecast_date = !!forecast_date,
      prediction_date = !!forecast_made,
      metric = !!metric
    )

  # Only rename predicted_column if it is not NULL
  if (!is.null(predicted_column)) {
    forecast_data <- forecast_data %>%
      rename(predicted = !!predicted_column)
  } else {
    forecast_data <- forecast_data %>%
      rename(predicted = "predicted_column")
  }

  # Prepare forecast unit dynamically
  forecast_unit_base <- c("prediction_date",
                          "forecast_date",
                          "metric",
                          "statistical_measure")
  if (!is.null(other_characteristic_columns)) {
    forecast_unit <- c(forecast_unit_base, other_characteristic_columns)
  } else {
    forecast_unit <- forecast_unit_base
  }

  # Conversion to scoringutils forecast object
  forecast <- scoringutils::as_forecast(
    forecast_data,
    forecast_type = "quantile",
    observed = "observed",
    predicted = "predicted",
    quantile_level = "quantile_level",
    forecast_unit = forecast_unit  # Use the dynamically created forecast_unit
  )

  return(forecast)
}


#' Prepare point forecast data
#'
#' @inheritParams prep_forecast_data
#' @param forecast_type The type of forecast ("point").
#' @param observed_column The name of the column containing observed values.
#' @param predicted_column The name of the column containing predicted values.
#' @param forecast_date The name of the column containing the forecast date.
#' @param forecast_made The name of the column containing the date the
#' forecast was made.
#' @param metric The name of the metric being forecasted.
#' @param other_characteristic_columns (optional) Other columns to
#' be used as characteristics.
#' @return A forecast_point object.
prep_forecast_data.point <- function(data,
                                     forecast_type = "point",
                                     observed_column,
                                     predicted_column,
                                     forecast_date,
                                     forecast_made,
                                     metric,
                                     other_characteristic_columns = NULL) {
  # Input validation
  stopifnot(inherits(data[[forecast_date]], "Date"),
            inherits(data[[forecast_made]], "Date"))

  # Data cleaning
  data <- clean_numeric_columns(data, c(observed_column, predicted_column))
  data$statistical_measure <- forecast_type
  data <- data %>%
    rename(
      observed = !!observed_column,
      forecast_date = !!forecast_date,
      prediction_date = !!forecast_made,
      metric = !!metric,
      predicted = !!predicted_column
    )

  # Prepare forecast unit dynamically
  forecast_unit_base <- c("prediction_date",
                          "forecast_date",
                          "metric",
                          "statistical_measure")
  if (!is.null(other_characteristic_columns)) {
    forecast_unit <- c(forecast_unit_base, other_characteristic_columns)
  } else {
    forecast_unit <- forecast_unit_base
  }

  # Conversion to scoringutils forecast object
  forecast <- scoringutils::as_forecast(
    data,
    forecast_type = "point",
    observed = "observed",
    predicted = "predicted",
    forecast_unit = forecast_unit
  )

  return(forecast)
}


#' Prepare sample forecast data
#'
#' @inheritParams prep_forecast_data
#' @param forecast_type The type of forecast ("sample").
#' @param observed_column The name of the column containing observed values.
#' @param predicted_column The name of the column containing predicted values.
#' @param forecast_date The name of the column containing the forecast date.
#' @param forecast_made The name of the column containing the date
#' the forecast was made.
#' @param metric The name of the metric being forecasted.
#' @param sample_id The name of the sample_id of each forecast.
#' @param other_characteristic_columns Optional, other columns to be
#' used as characteristics.
#' @return A forecast_point object.
prep_forecast_data.sample <- function(data,
                                      forecast_type = "sample",
                                      observed_column,
                                      predicted_column,
                                      forecast_date,
                                      forecast_made,
                                      metric,
                                      sample_id,
                                      other_characteristic_columns = NULL) {
  # Input validation
  stopifnot(inherits(data[[forecast_date]], "Date"),
            inherits(data[[forecast_made]], "Date"))

  # Data cleaning
  data <- clean_numeric_columns(data, c(observed_column, predicted_column))
  data$statistical_measure <- forecast_type
  data <- data %>%
    rename(
      observed = !!observed_column,
      forecast_date = !!forecast_date,
      prediction_date = !!forecast_made,
      metric = !!metric,
      sample_id = !!sample_id,
      predicted = !!predicted_column
    )

  # Prepare forecast unit dynamically
  forecast_unit_base <- c("prediction_date",
                          "forecast_date",
                          "metric",
                          "statistical_measure")
  if (!is.null(other_characteristic_columns)) {
    forecast_unit <- c(forecast_unit_base, other_characteristic_columns)
  } else {
    forecast_unit <- forecast_unit_base
  }
  # Conversion to scoringutils forecast object
  forecast <- scoringutils::as_forecast(
    data,
    forecast_type = "sample",
    observed = "observed",
    predicted = "predicted",
    sample_id = "sample_id",
    forecast_unit = forecast_unit
  )
  return(forecast)
}

wide_to_long_quantiles <- function(df, quantile_columns, quantile_values) {
  if (any(quantile_values < 0) || any(quantile_values > 1)) {
    stop("All quantile values must be between 0 and 1.")
  }
  if (!all(quantile_columns %in% colnames(df))) {
    stop("One or more quantile columns not found in the dataframe.")
  }

  # Identify columns to preserve (excluding quantile columns, adding a row ID)
  df <- df %>% dplyr::mutate(row_id = row_number())  # Create a temp. row ID
  preserve_columns <- setdiff(colnames(df), c(quantile_columns, "row_id"))
  # Reshape quantile data to long format
  long_quantiles <- df %>%
    select(row_id, all_of(quantile_columns)) %>%
    tidyr::pivot_longer(cols = -row_id,
                        names_to = "Variable",
                        values_to = "predicted_column") %>%
    dplyr::mutate(Variable = gsub("_", ".", Variable))
  long_quantiles$quantile_level <- rep(quantile_values,
                                       nrow(long_quantiles) /
                                         length(quantile_values))
  # Join back the preserved columns using row_id
  result_df <- long_quantiles %>%
    left_join(df %>% select(all_of(preserve_columns), row_id),
              by = "row_id") %>%
    select(-row_id)  # Remove the temporary row ID

  return(result_df)
}
