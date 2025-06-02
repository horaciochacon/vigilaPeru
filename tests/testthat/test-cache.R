test_that("cache directory functions work", {
  # Test getting cache directory
  cache_dir <- vp_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(length(cache_dir) == 1)
  
  # Test setting cache directory
  temp_dir <- tempdir()
  new_cache <- file.path(temp_dir, "test_cache")
  vp_cache_dir(new_cache)
  expect_equal(vp_cache_dir(), new_cache)
  expect_true(dir.exists(new_cache))
  
  # Test cache info
  info <- vp_cache_info()
  expect_true(is.data.frame(info$resumen))
  expect_true("directorio" %in% names(info$resumen))
  expect_true("archivos_total" %in% names(info$resumen))
  
  # Clean up
  unlink(new_cache, recursive = TRUE)
})

test_that("cache clear functions work", {
  # Create temporary cache
  temp_cache <- tempfile()
  dir.create(temp_cache)
  options(vigilaPeru.cache_dir = temp_cache)
  
  # Create test files
  test_file1 <- file.path(temp_cache, "test_dataset_123.rds")
  test_file2 <- file.path(temp_cache, "another_dataset_456.rds")
  saveRDS(list(test = "data"), test_file1)
  saveRDS(list(test = "data2"), test_file2)
  
  # Test clearing specific dataset  
  # Temporarily suppress logging to avoid output during tests
  old_threshold <- futile.logger::flog.threshold()
  futile.logger::flog.threshold(futile.logger::ERROR)
  
  expect_silent(vp_cache_clear("test"))
  expect_false(file.exists(test_file1))
  expect_true(file.exists(test_file2))
  
  # Test clearing all
  saveRDS(list(test = "data"), test_file1)
  expect_silent(vp_cache_clear())
  expect_false(file.exists(test_file1))
  expect_false(file.exists(test_file2))
  
  # Restore original logging threshold
  futile.logger::flog.threshold(old_threshold)
  
  # Clean up
  unlink(temp_cache, recursive = TRUE)
})

test_that("cache path generation works", {
  # Test dataset path
  path1 <- get_cache_path("malaria", extension = "rds")
  expect_true(grepl("malaria_metadata.json", path1))
  
  # Test resource path
  path2 <- get_cache_path("dengue", "resource123", "csv")
  expect_true(grepl("dengue_resource123.csv", path2))
})

test_that("cache needs update logic works", {
  # Create temporary file
  temp_file <- tempfile()
  writeLines("test", temp_file)
  
  # Test file doesn't exist
  expect_true(cache_needs_update("nonexistent_file.txt"))
  
  # Test max age
  expect_false(cache_needs_update(temp_file, max_age_hours = 24))
  
  # Test with old file (simulate old file)
  Sys.setFileTime(temp_file, Sys.time() - 25 * 3600)
  expect_true(cache_needs_update(temp_file, max_age_hours = 24))
  
  # Clean up
  unlink(temp_file)
})