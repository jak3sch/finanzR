testthat::test_that("kraken_ledgers_prepare() returns tibble with specific col names", {
  ledgers <- tibble::tibble(
    refid = 1:5,
    time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
                  "2022-04-18 09:07:51", "2022-04-19 02:46:48"),
    type = "deposit",
    asset = "ZEUR",
    amount = c("1000", "200", "350", "4000", "100")
  )

  prepared_data <- finanzR::kraken_ledgers_prepare(ledgers)

  testthat::expect_type(prepared_data, "list")
  testthat::expect_named(prepared_data, c("refid", "timestamp", "type", "asset", "amount", "currency", "deposit", "dividend", "receive", "spend", "staking", "trade", "transfer", "withdrawal", "note", "symbol", "coin_id", "date", "time"))
})
