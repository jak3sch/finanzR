testthat::test_that("all_coins() returns tibble with specific col names", {
  coins <- finanzR::all_coins()

  testthat::expect_type(coins, "list")
  testthat::expect_named(coins, c("name", "symbol", "coin_id"))
})

testthat::test_that("all_coins() returns no missing symbols", {
  coins <- finanzR::all_coins() %>%
    dplyr::filter(is.na(coin_id))

  testthat::expect_length(coins$coin_id, 0)
})

testthat::test_that("all_coins() returns no duplicates", {
  max_count <- finanzR::all_coins() %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::pull(count)

  testthat::expect_equal(max_count, 1)
})
