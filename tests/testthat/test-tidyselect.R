library(dplyr)

# Set up test data
pedestrian_small <- pedestrian %>%
  group_by(Sensor) %>%
  slice(1:10) %>%
  ungroup()

tourism_small <- tourism %>%
  group_by_key() %>%
  slice(1:10) %>%
  ungroup()

test_that("key_vars() works as tidyselect helper in select()", {
  # Select only key variables (index will be automatically re-added)
  result <- tourism_small %>% select(key_vars())
  # tsibble automatically adds index back
  expect_true(index_var(tourism_small) %in% names(result))
  expect_true(all(key_vars(tourism_small) %in% names(result)))
  expect_true(is_tsibble(result))
  
  # Combine with other selections
  result <- tourism_small %>% select(key_vars(), Quarter)
  expect_true(all(c(key_vars(tourism_small), "Quarter") %in% names(result)))
  
  # Select key_vars with index_var
  result <- tourism_small %>% select(index_var(), key_vars())
  expected_names <- c(index_var(tourism_small), key_vars(tourism_small))
  expect_equal(names(result), expected_names)
})

test_that("key_vars() works as tidyselect helper in relocate()", {
  # Move key variables to the end
  result <- tourism_small %>% relocate(key_vars(), .after = last_col())
  expect_equal(
    tail(names(result), length(key_vars(tourism_small))),
    key_vars(tourism_small)
  )
  
  # Move key variables to the beginning
  result <- tourism_small %>% relocate(key_vars(), .before = everything())
  expect_equal(
    head(names(result), length(key_vars(tourism_small))),
    key_vars(tourism_small)
  )
})

test_that("key_vars() errors appropriately outside tsibble context", {
  regular_df <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(
    regular_df %>% select(key_vars()),
    "Cannot use `key_vars\\(\\)` as a tidyselect helper"
  )
})

test_that("index_var() works as tidyselect helper in select()", {
  # Select only index variable (key will be automatically re-added)
  result <- tourism_small %>% select(index_var())
  expect_true(index_var(tourism_small) %in% names(result))
  # tsibble automatically adds key variables back
  expect_true(all(key_vars(tourism_small) %in% names(result)))
  expect_true(is_tsibble(result))
  
  # Combine with key_vars
  result <- tourism_small %>% select(index_var(), key_vars())
  expected_names <- c(index_var(tourism_small), key_vars(tourism_small))
  expect_equal(names(result), expected_names)
  
  # Select index with measured variables
  result <- tourism_small %>% select(index_var(), measured_vars())
  expected_names <- c(index_var(tourism_small), measured_vars(tourism_small))
  # Keys are automatically added
  expect_true(all(c(expected_names, key_vars(tourism_small)) %in% names(result)))
})

test_that("index_var() works as tidyselect helper in relocate()", {
  # Move index to end
  result <- tourism_small %>% relocate(index_var(), .after = last_col())
  expect_equal(tail(names(result), 1), index_var(tourism_small))
  
  # Move index after key_vars
  result <- tourism_small %>% relocate(index_var(), .after = key_vars())
  key_idx <- which(names(result) %in% key_vars(tourism_small))
  index_idx <- which(names(result) == index_var(tourism_small))
  expect_true(index_idx == max(key_idx) + 1)
})

test_that("index_var() errors appropriately outside tsibble context", {
  regular_df <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(
    regular_df %>% select(index_var()),
    "Cannot use `index_var\\(\\)` as a tidyselect helper"
  )
})

test_that("measured_vars() works as tidyselect helper in select()", {
  # Select only measured variables (index and key will be automatically re-added)
  result <- tourism_small %>% select(measured_vars())
  expect_true(all(measured_vars(tourism_small) %in% names(result)))
  # tsibble automatically adds index and key back
  expect_true(index_var(tourism_small) %in% names(result))
  expect_true(all(key_vars(tourism_small) %in% names(result)))
  expect_true(is_tsibble(result))
  
  # Combine with key and index
  result <- tourism_small %>% select(measured_vars(), key_vars(), index_var())
  expected_names <- c(
    measured_vars(tourism_small),
    key_vars(tourism_small),
    index_var(tourism_small)
  )
  expect_equal(names(result), expected_names)
  
  # Select measured variables with where()
  result <- pedestrian_small %>% select(measured_vars(), where(is.numeric))
  # Should include measured vars
  expect_true(all(measured_vars(pedestrian_small) %in% names(result)))
})

test_that("measured_vars() works as tidyselect helper in relocate()", {
  # Move measured variables to beginning
  result <- tourism_small %>% relocate(measured_vars())
  expect_equal(
    head(names(result), length(measured_vars(tourism_small))),
    measured_vars(tourism_small)
  )
  
  # Move measured variables to end
  result <- tourism_small %>% relocate(measured_vars(), .after = last_col())
  expect_equal(
    tail(names(result), length(measured_vars(tourism_small))),
    measured_vars(tourism_small)
  )
})

test_that("measured_vars() errors appropriately outside tsibble context", {
  regular_df <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(
    regular_df %>% select(measured_vars()),
    "Cannot use `measured_vars\\(\\)` as a tidyselect helper"
  )
})

test_that("index2_var() works as tidyselect helper in select()", {
  # Create tsibble with index2
  indexed <- tourism_small %>% index_by(Year = lubridate::year(Quarter))
  
  # Select index2 variable (index and key will be automatically re-added)
  result <- indexed %>% select(index2_var())
  expect_true(index2_var(indexed) %in% names(result))
  expect_true(index_var(indexed) %in% names(result))
  expect_true(all(key_vars(indexed) %in% names(result)))
  expect_true(is_tsibble(result))
  
  # Combine with other selectors
  result <- indexed %>% select(index_var(), index2_var(), key_vars())
  expected_names <- c(
    index_var(indexed),
    index2_var(indexed),
    key_vars(indexed)
  )
  expect_equal(names(result), expected_names)
})

test_that("index2_var() works as tidyselect helper in relocate()", {
  indexed <- tourism_small %>% index_by(Year = lubridate::year(Quarter))
  
  # Move index2 to end
  result <- indexed %>% relocate(index2_var(), .after = last_col())
  expect_equal(tail(names(result), 1), index2_var(indexed))
  
  # Move index2 after index
  result <- indexed %>% relocate(index2_var(), .after = index_var())
  index_idx <- which(names(result) == index_var(indexed))
  index2_idx <- which(names(result) == index2_var(indexed))
  expect_equal(index2_idx, index_idx + 1)
})

test_that("index2_var() errors appropriately outside tsibble context", {
  regular_df <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(
    regular_df %>% select(index2_var()),
    "Cannot use `index2_var\\(\\)` as a tidyselect helper"
  )
})

test_that("tidyselect helpers work together in complex selections", {
  # Select all metadata columns
  result <- tourism_small %>%
    select(index_var(), key_vars(), measured_vars())
  expect_equal(names(result), names(tourism_small))
  
  # Reorder: measured, then key, then index
  result <- tourism_small %>%
    select(measured_vars(), key_vars(), index_var())
  expected_order <- c(
    measured_vars(tourism_small),
    key_vars(tourism_small),
    index_var(tourism_small)
  )
  expect_equal(names(result), expected_order)
  
  # Complex relocate
  result <- tourism_small %>%
    relocate(measured_vars(), .after = index_var())
  index_idx <- which(names(result) == index_var(tourism_small))
  measured_idx <- which(names(result) %in% measured_vars(tourism_small))
  expect_true(all(measured_idx > index_idx))
})

test_that("tidyselect helpers work with negation", {
  # Select everything except key variables (but keys will be re-added by tsibble)
  result <- tourism_small %>% select(!key_vars())
  # Keys are automatically added back to maintain tsibble structure
  expect_true(all(key_vars(tourism_small) %in% names(result)))
  # But measured vars should be there
  expect_true(all(measured_vars(tourism_small) %in% names(result)))
  
  # Select everything except measured variables
  result <- tourism_small %>% select(!measured_vars())
  expect_false(any(measured_vars(tourism_small) %in% names(result)))
  expect_true(all(key_vars(tourism_small) %in% names(result)))
  expect_true(index_var(tourism_small) %in% names(result))
  
  # Select everything except index (but index will be re-added by tsibble)
  result <- tourism_small %>% select(!index_var())
  # Index is automatically added back to maintain tsibble structure
  expect_true(index_var(tourism_small) %in% names(result))
})

test_that("tidyselect helpers preserve tsibble attributes", {
  # After selecting with key_vars
  result <- tourism_small %>% select(key_vars(), index_var(), measured_vars())
  expect_true(is_tsibble(result))
  expect_equal(key_vars(result), key_vars(tourism_small))
  expect_equal(index_var(result), index_var(tourism_small))
  
  # After relocating
  result <- tourism_small %>% relocate(measured_vars(), .before = everything())
  expect_true(is_tsibble(result))
  expect_equal(key_vars(result), key_vars(tourism_small))
  expect_equal(index_var(result), index_var(tourism_small))
})

test_that("tidyselect helpers work with renamed columns", {
  # Select and rename
  result <- tourism_small %>% select(idx = index_var(), measured_vars())
  expect_true("idx" %in% names(result))
  expect_true(all(measured_vars(tourism_small) %in% names(result)))
  
  # Select key_vars with rename
  result <- tourism_small %>%
    select(index_var(), key1 = Region, key2 = State, key3 = Purpose, measured_vars())
  expect_equal(names(result)[2:4], c("key1", "key2", "key3"))
})

test_that("tidyselect helpers work with multiple tsibbles", {
  # Create another tsibble
  ped_result <- pedestrian_small %>% select(key_vars(), index_var())
  expect_true(is_tsibble(ped_result))
  expect_equal(key_vars(ped_result), key_vars(pedestrian_small))
  
  tour_result <- tourism_small %>% select(key_vars(), index_var())
  expect_true(is_tsibble(tour_result))
  expect_equal(key_vars(tour_result), key_vars(tourism_small))
  
  # They should have different key structures
  expect_false(identical(key_vars(ped_result), key_vars(tour_result)))
})
