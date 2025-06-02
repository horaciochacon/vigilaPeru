# Helper functions for tests

# Skip if no internet connection
skip_if_offline <- function() {
  if (!check_internet()) {
    skip("No internet connection available")
  }
}

# Create mock surveillance data
create_mock_surveillance_data <- function(n = 1000) {
  data.frame(
    departamento = sample(c("LIMA", "CUSCO", "AREQUIPA", "PIURA"), n, replace = TRUE),
    provincia = paste0("PROV", sample(1:5, n, replace = TRUE)),
    distrito = paste0("DIST", sample(1:10, n, replace = TRUE)),
    enfermedad = sample(c("MALARIA", "DENGUE"), n, replace = TRUE),
    ano = sample(2020:2023, n, replace = TRUE),
    semana = sample(1:52, n, replace = TRUE),
    diagnostic = sample(c("B50", "B51", "A90", "A91"), n, replace = TRUE),
    tipo_dx = sample(c("C", "P", "S"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    diresa = sample(1:25, n, replace = TRUE),
    ubigeo = sprintf("%02d%02d%02d", 
                     sample(1:25, n, replace = TRUE),
                     sample(1:20, n, replace = TRUE),
                     sample(1:50, n, replace = TRUE)),
    edad = sample(0:80, n, replace = TRUE),
    tipo_edad = "A",
    sexo = sample(c("M", "F"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Create temporary cache directory for tests
setup_test_cache <- function() {
  temp_cache <- file.path(tempdir(), "vigilaPeru_test_cache")
  dir.create(temp_cache, showWarnings = FALSE, recursive = TRUE)
  options(vigilaPeru.cache_dir = temp_cache)
  return(temp_cache)
}

# Clean up test cache
cleanup_test_cache <- function(cache_dir) {
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
  }
}

# Mock API response for testing
mock_ckan_response <- function(success = TRUE, dataset_id = "test-dataset") {
  if (success) {
    list(
      success = TRUE,
      result = list(
        id = dataset_id,
        name = dataset_id,
        title = paste("Test Dataset", dataset_id),
        notes = "This is a test dataset for unit testing",
        metadata_created = "2024-01-01T00:00:00",
        metadata_modified = "2024-01-02T00:00:00",
        organization = list(
          id = "test-org",
          name = "test-organization",
          title = "Test Organization"
        ),
        resources = list(
          list(
            id = "resource-1",
            url = "http://example.com/data.csv",
            format = "CSV",
            name = "test_data.csv",
            size = "1048576",
            created = "2024-01-01T00:00:00",
            last_modified = "2024-01-02T00:00:00"
          )
        ),
        tags = list(
          list(name = "vigilancia"),
          list(name = "epidemiología")
        ),
        license_title = "Creative Commons Attribution 4.0"
      )
    )
  } else {
    list(
      success = FALSE,
      error = list(
        message = "Dataset not found",
        __type = "Not Found Error"
      )
    )
  }
}