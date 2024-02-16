test_that("split_col_value() returns tibble with specific col names", {
  timestamps <- tibble::tibble(
    timestamp = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
            "2022-04-18 09:07:51", "2022-04-19 02:46:48")
  )

  split_data <- finanzR::split_col_value(timestamps, timestamp, colnames = c("date", "time"))

  testthat::expect_type(split_data, "list")
  testthat::expect_named(split_data, c("timestamp", "date", "time"))
})
