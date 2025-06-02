test_that("known datasets are properly defined", {
  # Check that KNOWN_DATASETS exists and has expected structure
  expect_true(exists("KNOWN_DATASETS"))
  expect_true(is.list(KNOWN_DATASETS))
  expect_true(length(KNOWN_DATASETS) >= 3)
  
  # Check core datasets exist
  expect_true("malaria" %in% names(KNOWN_DATASETS))
  expect_true("dengue" %in% names(KNOWN_DATASETS))
  expect_true("leishmaniasis" %in% names(KNOWN_DATASETS))
  
  # Check values are character strings
  for (ds in names(KNOWN_DATASETS)) {
    expect_true(is.character(KNOWN_DATASETS[[ds]]))
    expect_true(nchar(KNOWN_DATASETS[[ds]]) > 0)
  }
})

test_that("resolve_dataset_id works correctly", {
  # Test with known names
  expect_equal(
    resolve_dataset_id("malaria"),
    "vigilancia-epidemiológica-de-malaria"
  )
  
  expect_equal(
    resolve_dataset_id("dengue"),
    "vigilancia-epidemiológica-de-dengue"
  )
  
  # Test with full ID (should return as-is)
  full_id <- "vigilancia-epidemiológica-de-malaria"
  expect_equal(resolve_dataset_id(full_id), full_id)
})

test_that("vp_dataset_info handles errors gracefully", {
  # Test with invalid dataset
  expect_error(vp_dataset_info("nonexistent_dataset"))
  
  # Test with NULL (should return all info)
  # Note: This might fail if no internet connection
  skip_if_offline()
  all_info <- vp_dataset_info()
  expect_true(is.list(all_info))
})

test_that("extract_dcat_metadata works", {
  # Create mock CKAN response
  mock_response <- list(
    result = list(
      id = "test-123",
      title = "Test Dataset",
      notes = "Test description",
      metadata_created = "2024-01-01",
      metadata_modified = "2024-01-02",
      organization = list(title = "Test Org"),
      license_title = "CC-BY",
      resources = list()
    )
  )
  
  # Extract DCAT metadata
  dcat <- extract_dcat_metadata(mock_response)
  
  expect_true(is.list(dcat))
  expect_equal(dcat$`dct:identifier`, "test-123")
  expect_equal(dcat$`dct:title`, "Test Dataset")
  expect_equal(dcat$`dct:publisher`, "Test Org")
})

test_that("CSV resource extraction works", {
  # Create mock metadata with resources
  mock_metadata <- list(
    resources = list(
      list(
        id = list("res1"),
        name = list("data.csv"),
        format = list("CSV"),
        url = list("http://example.com/data.csv"),
        size = list("1000"),
        last_modified = list("2024-01-01")
      ),
      list(
        id = list("res2"),
        name = list("data.xlsx"),
        format = list("XLSX"),
        url = list("http://example.com/data.xlsx"),
        size = list("2000"),
        last_modified = list("2024-01-02")
      )
    )
  )
  
  # Extract CSV resources
  csv_resources <- extract_csv_resources(mock_metadata)
  
  expect_equal(length(csv_resources), 1)
  expect_equal(csv_resources[[1]]$format[[1]], "CSV")
})