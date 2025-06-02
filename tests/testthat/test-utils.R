test_that("internet connection check works", {
  # This should work in most environments
  result <- check_internet()
  expect_true(is.logical(result))
  expect_length(result, 1)
})

test_that("tibble conversion works", {
  # Test with data.table
  dt <- data.table::data.table(x = 1:5, y = letters[1:5])
  
  # Convert to tibble
  result_tibble <- maybe_as_tibble(dt, as_tibble = TRUE)
  expect_s3_class(result_tibble, "tbl_df")
  
  # Keep as data.table
  result_dt <- maybe_as_tibble(dt, as_tibble = FALSE)
  expect_true(data.table::is.data.table(result_dt))
})

test_that("dataset ID validation works", {
  # Valid ID
  expect_silent(validate_dataset_id("malaria"))
  
  # Invalid IDs
  expect_error(validate_dataset_id(""))
  expect_error(validate_dataset_id(NULL))
  expect_error(validate_dataset_id(c("malaria", "dengue")))
})

test_that("byte formatting works", {
  expect_equal(format_bytes(100), "100 B")
  expect_equal(format_bytes(1500), "1.5 KB")
  expect_equal(format_bytes(1500000), "1.4 MB")
  expect_equal(format_bytes(1500000000), "1.4 GB")
})

test_that("logging setup works", {
  # Test verbose mode
  setup_logging(verbose = TRUE)
  threshold <- futile.logger::flog.threshold()
  expect_equal(threshold, futile.logger::INFO)
  
  # Test quiet mode
  setup_logging(verbose = FALSE)
  threshold <- futile.logger::flog.threshold()
  expect_equal(threshold, futile.logger::WARN)
})

test_that("date parsing from portal format works", {
  # Test various date formats from portal
  test_dates <- c(
    "Date changed  Mié, 12/11/2024 - 14:57",
    "Date changed  Lun, 12/16/2024 - 12:51",
    "Vie, 11/24/2023 - 08:50"
  )
  
  # Should not error
  for (date_str in test_dates) {
    result <- cache_needs_update(tempfile(), remote_modified = date_str)
    expect_true(is.logical(result))
  }
})