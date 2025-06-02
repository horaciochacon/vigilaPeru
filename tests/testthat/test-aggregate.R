test_that("vp_aggregate works with basic grouping", {
  # Create test data
  test_data <- data.frame(
    year = rep(2020:2022, each = 4),
    month = rep(1:2, 6),
    cases = rpois(12, 100),
    department = rep(c("Lima", "Cusco"), 6)
  )
  
  # Test basic aggregation
  result <- vp_aggregate(test_data, by = "year")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("casos" %in% names(result))
  
  # Test with multiple grouping variables
  result2 <- vp_aggregate(test_data, by = c("year", "department"))
  expect_equal(nrow(result2), 6)
  
  # Test as data.table
  result3 <- vp_aggregate(test_data, by = "year", as_tibble = FALSE)
  expect_true(data.table::is.data.table(result3))
})

test_that("vp_aggregate handles custom statistics", {
  test_data <- data.frame(
    group = rep(c("A", "B"), 50),
    value = rnorm(100, mean = 100, sd = 10)
  )
  
  # Test with custom summary functions
  result <- vp_aggregate(
    test_data,
    by = "group",
    summarize = list(
      mean_val = mean,
      sd_val = sd,
      count = length
    ),
    cases_col = "value"
  )
  
  expect_equal(nrow(result), 2)
  expect_true(all(c("mean_val", "sd_val", "count") %in% names(result)))
})

test_that("vp_aggregate_geo works correctly", {
  # Create test data with ubigeo codes
  test_data <- data.frame(
    ubigeo = c("150101", "150102", "130101", "130102", "120101"),
    cases = c(10, 20, 30, 40, 50)
  )
  
  # Test department level aggregation
  result_dept <- vp_aggregate_geo(test_data, level = "departamento")
  expect_true("departamento_codigo" %in% names(result_dept))
  expect_equal(nrow(result_dept), 3)  # 3 departments
  
  # Test province level aggregation
  result_prov <- vp_aggregate_geo(test_data, level = "provincia")
  expect_true("provincia_codigo" %in% names(result_prov))
  
  # Test with totals
  result_total <- vp_aggregate_geo(test_data, level = "departamento", include_totals = TRUE)
  expect_true(any(result_total$departamento_codigo == "00"))
})

test_that("vp_aggregate_time works with different periods", {
  # Create test data
  test_data <- data.frame(
    ano = rep(2023, 52),
    semana = 1:52,
    cases = rpois(52, 50)
  )
  
  # Test weekly aggregation (should be same as input)
  result_week <- vp_aggregate_time(test_data, period = "semana")
  expect_equal(nrow(result_week), 52)
  
  # Test monthly aggregation
  result_month <- vp_aggregate_time(test_data, period = "mes")
  expect_true("mes" %in% names(result_month))
  expect_equal(nrow(result_month), 12)
  
  # Test quarterly aggregation
  result_quarter <- vp_aggregate_time(test_data, period = "trimestre")
  expect_true("trimestre" %in% names(result_quarter))
  expect_equal(nrow(result_quarter), 4)
  
  # Test yearly aggregation
  result_year <- vp_aggregate_time(test_data, period = "ano")
  expect_equal(nrow(result_year), 1)
})

test_that("vp_aggregate handles missing values", {
  # Create test data with NAs
  test_data <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(10, NA, 20, 30)
  )
  
  # Test with na.rm = TRUE (default)
  result <- vp_aggregate(test_data, by = "group", cases_col = "value")
  expect_equal(result$casos[result$group == "A"], 10)
  expect_equal(result$casos[result$group == "B"], 50)
})