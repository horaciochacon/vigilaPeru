test_that("ubigeo extraction functions work correctly", {
  # Test department extraction
  expect_equal(ubigeo_get_department("150101"), "15")
  expect_equal(ubigeo_get_department(c("150101", "010203")), c("15", "01"))
  
  # Test province extraction
  expect_equal(ubigeo_get_province("150101"), "1501")
  expect_equal(ubigeo_get_province(c("150101", "010203")), c("1501", "0102"))
})

test_that("ubigeo validation works", {
  # Valid codes
  expect_true(all(ubigeo_is_valid(c("150101", "010101", "250505"))))
  
  # Invalid codes
  expect_false(ubigeo_is_valid("999999"))  # Department > 25
  expect_false(ubigeo_is_valid("15"))      # Too short
  expect_false(ubigeo_is_valid("ABC123"))  # Non-numeric
  expect_false(ubigeo_is_valid("260101"))  # Department 26 doesn't exist
})

test_that("ubigeo padding works", {
  expect_equal(ubigeo_pad("10101"), "010101")
  expect_equal(ubigeo_pad("150101"), "150101")
  expect_equal(ubigeo_pad(10101), "010101")
})

test_that("department reference table is correct", {
  deps <- ubigeo_departments()
  
  # Check structure
  expect_true(data.table::is.data.table(deps))
  expect_equal(nrow(deps), 25)
  expect_equal(names(deps), c("codigo", "departamento", "capital", "region"))
  
  # Check some specific departments
  expect_equal(deps[codigo == "15"]$departamento, "LIMA")
  expect_equal(deps[codigo == "01"]$departamento, "AMAZONAS")
  expect_equal(deps[codigo == "25"]$departamento, "UCAYALI")
})

test_that("ubigeo aggregation works", {
  # Create test data
  dt <- data.table::data.table(
    ubigeo = c("150101", "150102", "150201", "130101", "130201"),
    casos = c(10, 20, 15, 5, 8)
  )
  
  # Test department aggregation
  result_dep <- ubigeo_aggregate(dt, level = "department", include_names = TRUE)
  expect_true("geo_code" %in% names(result_dep))
  expect_true("departamento" %in% names(result_dep))
  expect_equal(result_dep$geo_code, c("15", "15", "15", "13", "13"))
  
  # Test province aggregation
  result_prov <- ubigeo_aggregate(dt, level = "province", include_names = TRUE)
  expect_true("geo_code" %in% names(result_prov))
  expect_equal(result_prov$geo_code, c("1501", "1501", "1502", "1301", "1302"))
})