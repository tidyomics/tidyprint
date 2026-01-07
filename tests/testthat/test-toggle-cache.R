library(testthat)
library(tidyprint)

# Helper to temporarily override cache path for testing
with_temp_cache <- function(code) {
  # Save original cache path getter
  ns <- asNamespace("tidyprint")
  original_getter <- ns$.get_cache_path
  
  # Create temporary directory
  temp_dir <- tempfile("tidyprint_test_cache")
  dir.create(temp_dir, recursive = TRUE)
  
  # Create a new function that uses temp directory
  new_getter <- function() {
    file.path(temp_dir, "tidy_print_enabled.rds")
  }
  
  # Unlock and override .get_cache_path to use temp directory
  if (bindingIsLocked(".get_cache_path", ns)) {
    unlockBinding(".get_cache_path", ns)
  }
  assign(".get_cache_path", new_getter, envir = ns)
  
  on.exit({
    # Restore original
    if (bindingIsLocked(".get_cache_path", ns)) {
      unlockBinding(".get_cache_path", ns)
    }
    assign(".get_cache_path", original_getter, envir = ns)
    # Lock it back if it was locked
    lockBinding(".get_cache_path", ns)
    # Clean up temp directory
    unlink(temp_dir, recursive = TRUE)
  })
  
  # Also clear the option at start
  old_option <- getOption("tidyprint.use_tidy_print")
  on.exit({
    if (is.null(old_option)) {
      options(tidyprint.use_tidy_print = NULL)
    } else {
      options(tidyprint.use_tidy_print = old_option)
    }
  }, add = TRUE)
  
  options(tidyprint.use_tidy_print = NULL)
  
  force(code)
}

test_that("tidy_print_enabled returns FALSE by default", {
  with_temp_cache({
    expect_false(tidy_print_enabled())
  })
})

test_that("tidy_print_on enables tidy print (session only)", {
  with_temp_cache({
    # Start with disabled
    tidy_print_off()
    expect_false(tidy_print_enabled())
    
    # Enable (non-remember)
    result <- tidy_print_on(remember = FALSE)
    expect_true(result)
    expect_true(tidy_print_enabled())
    expect_true(getOption("tidyprint.use_tidy_print"))
    
    # Cache should not exist
    cache_path <- tidyprint:::.get_cache_path()
    expect_false(file.exists(cache_path))
  })
})

test_that("tidy_print_off disables tidy print (session only)", {
  with_temp_cache({
    # Start with enabled
    tidy_print_on()
    expect_true(tidy_print_enabled())
    
    # Disable (non-remember)
    result <- tidy_print_off(remember = FALSE)
    expect_false(result)
    expect_false(tidy_print_enabled())
    expect_false(getOption("tidyprint.use_tidy_print"))
    
    # Cache should not exist
    cache_path <- tidyprint:::.get_cache_path()
    expect_false(file.exists(cache_path))
  })
})

test_that("tidy_print_on with remember creates cache file", {
  with_temp_cache({
    # Enable rememberly
    tidy_print_on(remember = TRUE)
    
    # Check cache file exists
    cache_path <- tidyprint:::.get_cache_path()
    expect_true(file.exists(cache_path))
    
    # Check cache contains TRUE
    cached_value <- readRDS(cache_path)
    expect_true(cached_value)
    
    # Check that enabled returns TRUE
    expect_true(tidy_print_enabled())
  })
})

test_that("tidy_print_off with remember updates cache file", {
  with_temp_cache({
    # First enable rememberly
    tidy_print_on(remember = TRUE)
    cache_path <- tidyprint:::.get_cache_path()
    expect_true(file.exists(cache_path))
    expect_true(readRDS(cache_path))
    
    # Now disable rememberly
    tidy_print_off(remember = TRUE)
    
    # Check cache contains FALSE
    expect_true(file.exists(cache_path))
    cached_value <- readRDS(cache_path)
    expect_false(cached_value)
    
    # Check that enabled returns FALSE
    expect_false(tidy_print_enabled())
  })
})

test_that("option takes precedence over cache", {
  with_temp_cache({
    # Create cache with FALSE
    tidy_print_off(remember = TRUE)
    expect_false(tidy_print_enabled())
    
    # Set option to TRUE - should override cache
    options(tidyprint.use_tidy_print = TRUE)
    expect_message(
      result <- tidy_print_enabled(),
      "R option.*overrides cache value"
    )
    expect_true(result)
    
    # Set option to FALSE - should override cache
    options(tidyprint.use_tidy_print = FALSE)
    expect_false(tidy_print_enabled())
    
    # Clear option - should use cache
    options(tidyprint.use_tidy_print = NULL)
    expect_false(tidy_print_enabled())
    
    # Update cache to TRUE
    tidy_print_on(remember = TRUE)
    # Clear option first
    options(tidyprint.use_tidy_print = NULL)
    expect_true(tidy_print_enabled())
  })
})

test_that("option is used when cache doesn't exist", {
  with_temp_cache({
    # No cache file should exist
    cache_path <- tidyprint:::.get_cache_path()
    expect_false(file.exists(cache_path))
    
    # Set option to TRUE
    options(tidyprint.use_tidy_print = TRUE)
    expect_true(tidy_print_enabled())
    
    # Set option to FALSE
    options(tidyprint.use_tidy_print = FALSE)
    expect_false(tidy_print_enabled())
  })
})

test_that("toggling works multiple times", {
  with_temp_cache({
    # Toggle on
    tidy_print_on()
    expect_true(tidy_print_enabled())
    
    # Toggle off
    tidy_print_off()
    expect_false(tidy_print_enabled())
    
    # Toggle on again
    tidy_print_on()
    expect_true(tidy_print_enabled())
    
    # Toggle off again
    tidy_print_off()
    expect_false(tidy_print_enabled())
  })
})

test_that("option overrides remember cache setting", {
  with_temp_cache({
    # Enable rememberly (sets both option and cache to TRUE)
    tidy_print_on(remember = TRUE)
    # Clear option to use cache
    options(tidyprint.use_tidy_print = NULL)
    expect_true(tidy_print_enabled())
    
    # Now set option to FALSE (cache is still TRUE) - should override and message
    options(tidyprint.use_tidy_print = FALSE)
    expect_message(
      result <- tidy_print_enabled(),
      "R option.*overrides cache value"
    )
    expect_false(result)
    
    # Disable rememberly (sets cache to FALSE)
    tidy_print_off(remember = TRUE)
    # Now cache is FALSE, option is still FALSE - no message needed
    expect_false(tidy_print_enabled())
    
    # Change option to TRUE (cache is FALSE) - should override and message
    options(tidyprint.use_tidy_print = TRUE)
    expect_message(
      result <- tidy_print_enabled(),
      "R option.*overrides cache value"
    )
    expect_true(result)
    
    # Clear option - should use cache (FALSE)
    options(tidyprint.use_tidy_print = NULL)
    expect_false(tidy_print_enabled())
  })
})

test_that("cache file is created in correct location", {
  with_temp_cache({
    tidy_print_on(remember = TRUE)
    
    cache_path <- tidyprint:::.get_cache_path()
    cache_dir <- dirname(cache_path)
    
    # Directory should exist
    expect_true(dir.exists(cache_dir))
    
    # File should exist
    expect_true(file.exists(cache_path))
    
    # Should be an RDS file
    expect_equal(basename(cache_path), "tidy_print_enabled.rds")
  })
})

test_that("functions return correct invisible values", {
  with_temp_cache({
    # tidy_print_on should return TRUE invisibly
    result <- tidy_print_on()
    expect_true(result)
    expect_invisible(tidy_print_on())
    
    # tidy_print_off should return FALSE invisibly
    result <- tidy_print_off()
    expect_false(result)
    expect_invisible(tidy_print_off())
  })
})
