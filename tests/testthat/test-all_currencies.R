test_that("all_currencies() returns tibble with specific col names", {
  currencies <- finanzR::all_currencies()

  testthat::expect_type(currencies, "list")
  testthat::expect_named(currencies, c("symbol", "name", "currency_id"))
})

