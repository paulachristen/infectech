test_check("infectech")

context("prep_forecast_data")

# Test prep_forecast_data for point forecasts
test_that("prep_forecast_data.point creates forecast_point object", {

  # create sample point data
  sample_point_data <- data.frame(
    observed = c(10, 15, 22),
    predicted = c(12, 14, 20),
    forecast_date = as.Date(c("2023-12-01", "2023-12-02", "2023-12-03")),
    forecast_made = as.Date(c("2023-11-28", "2023-11-29", "2023-11-30")),
    metric = "cases"
  )

  point_forecast <- prep_forecast_data(data = sample_point_data,
                                       forecast_type = "point",
                                       observed_column = "observed",
                                       predicted_column = "predicted",
                                       forecast_date = "forecast_date",
                                       forecast_made = "forecast_made",
                                       metric = "metric")

  expect_equal(point_forecast$observed, sample_point_data$observed)
  expect_equal(point_forecast$predicted, sample_point_data$predicted)
})

# Test prep_forecast_data for quantile forecasts
test_that("prep_forecast_data.quantile creates forecast_quantile object", {
  # create sample quantile data
  sample_quantile_data <- data.frame(
    observed = c(10, 15, 22),
    quantile_0.5 = c(11, 16, 21),
    quantile_0.9 = c(13, 18, 24),
    forecast_date = as.Date(c("2023-12-01", "2023-12-02", "2023-12-03")),
    forecast_made = as.Date(c("2023-11-28", "2023-11-29", "2023-11-30")),
    metric = "cases"
  )

  quantile_forecast <- prep_forecast_data(data = sample_quantile_data,
                                          forecast_type = "quantile",
                                          observed_column = "observed",
                                          predicted_column = NULL,
                                          quantile_columns = c("quantile_0.5",
                                                               "quantile_0.9"),
                                          quantile_values = c(0.5, 0.9),
                                          forecast_date = "forecast_date",
                                          forecast_made = "forecast_made",
                                          metric = "metric")

  expect_equal(unique(quantile_forecast$quantile_level), c(0.5, 0.9))
})
