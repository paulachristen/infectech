




wide_to_long_quantiles <- function(df, quantile_columns, quantile_values) {
  # Input validation (same as before)
  if (any(quantile_values < 0) || any(quantile_values > 1)) {
    stop("All quantile values must be between 0 and 1.")
  }
  if (!all(quantile_columns %in% colnames(df))) {
    stop("One or more quantile columns not found in the dataframe.")
  }
  # Identify columns to preserve (excluding quantile columns and adding a row ID)
  df <- df %>% mutate(row_id = row_number())  # Create a temporary row ID
  preserve_columns <- setdiff(colnames(df), c(quantile_columns, "row_id"))
  # Reshape quantile data to long format
  long_quantiles <- df %>%
    select(row_id, all_of(quantile_columns)) %>%  # Include row_id
    pivot_longer(cols = -row_id,              # Pivot everything except row_id
                 names_to = "Variable",
                 values_to = "predicted_column") %>%
    mutate(
      Variable = gsub("_", ".", Variable))
  long_quantiles$quantile_level <- rep(quantile_values,nrow(long_quantiles)/length(quantile_values))
  # Join back the preserved columns using row_id
  result_df <- long_quantiles %>%
    left_join(df %>% select(all_of(preserve_columns), row_id), by = "row_id") %>%
    select(-row_id)  # Remove the temporary row ID

  return(result_df)
}

prep_forecast_data <- function(data,
                               forecast_type,
                               observed_column,
                               predicted_column,
                               quantile_columns = NULL,
                               quantile_values = NULL,
                               forecast_date,
                               forecast_made,
                               metric,
                               other_characteristic_columns = NULL) {

  if (forecast_type == "quantile") {
    if (length(quantile_columns) > 1 && is.null(quantile_values)) {
      stop("Quantile values must be provided if quantile_columns has more than one element.")
    } else if (length(quantile_columns) == 1 && !(quantile_values %in% colnames(data))) {
      stop("Quantile values column not found or not specified.")
    }
  }


  # Check date column format
  date_columns <- c(forecast_date, forecast_made)
  for (date_col in date_columns) {
    if (!inherits(data[[date_col]], "Date")) {
      warning(paste0("Date column '", date_col, "' needs to be formatted as Date."))
    }
  }

  # Step 2: Handle numeric columns (observed, predicted) (same as before)
  numeric_cols <- c("observed_column", "predicted_column", quantile_columns)
  data <- data %>%
    mutate(across(any_of(numeric_cols), ~ as.numeric(gsub("\\(|\\)", "", .x)))) %>%
    mutate(across(any_of(numeric_cols), ~ ifelse(grepl("^n\\.?a\\.?n\\.?$", .x, ignore.case = TRUE), NA, .x)))

  # Step 3: Handle quantile data (if applicable)
  if (forecast_type == "quantile") {
    if (length(quantile_columns) > 1) {
      # Wide format: Use wide_to_long_quantiles function
      if (is.null(quantile_values)) {
        stop("Quantile values must be provided if quantile_columns has more than one element.")
      }
      data <- wide_to_long_quantiles(data, quantile_columns, quantile_values)
    } else {
      # Long format: Use specified quantile_values column and rename
      if (is.null(quantile_values) || !(quantile_values %in% colnames(data))) {
        stop("Quantile values column not found or not specified.")
      }
      data <- data %>%
        rename(quantile_level = all_of(quantile_values)) %>%
        rename(predicted_column = all_of(quantile_columns)) %>%
        mutate(Variable = gsub("_", ".", Variable))
    }
  }

  # Step 4: Transform to scoringutils format (with conditional logic)
  data$statistical_measure <- forecast_type

  forecast_data <- data %>%
    rename(observed = !!observed_column,
           forecast_date = !!forecast_date,
           prediction_date = !!forecast_made,
           metric = !!metric)  # Use !! to unquote and evaluate the column names

  # Only rename predicted_column if it is not NULL
  if (!is.null(predicted_column)) {
    forecast_data <- forecast_data %>%
      rename(predicted = !!predicted_column)
  } else {
    forecast_data <- forecast_data %>%
      rename(predicted = "predicted_column")
  }

  forecast <- scoringutils::as_forecast(
    forecast_data,
    forecast_type = forecast_type,
    observed = "observed",
    predicted = "predicted",
    quantile_level = if (forecast_type == "quantile") "quantile_level" else NULL,
    forecast_unit = c("prediction_date","forecast_date", "metric", "statistical_measure", other_characteristic_columns)
  )

  names_forecast <- names(forecast)  # Get all column names
  excl <- names_forecast[names_forecast != "quantile_level"]  # Remove the column named "x"

  # Remove duplicates based on columns in alpha
  forecast <- forecast %>%
    distinct(across(all_of(excl)))

  return(forecast)
}
