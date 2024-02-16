test_that("multiplication works", {
  timestamps <- tibble::tibble(
    time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
            "2022-04-18 09:07:51", "2022-04-19 02:46:48")
    )

  split_data <- finanzR::split_col_value(timestamps, time, colnames = c("date", "time"))

  testthat::expect_type(split_data, "list")
  testthat::expect_named(split_data, c("date", "time"))
})
