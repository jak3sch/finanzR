#' Load all supported currencies
#'
#' @description Load all supported currencies. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.
#'
#' @return A character string with all currency IDs
#'
#' @export
#'
#' @examples
#' r <- all_currencies()
#' r

kraken_ledgers_to_pp <- function(input, base_currency = "eur", lang = "de", filter = FALSE) {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  # TODO: if filter is set, filter for transaction types

  prepare <- finanzR::kraken_ledgers_prepare(input = ledgers)

  # load all coins
  all_coins <- crypto2::crypto_list(only_active = FALSE)

  # only needed for staking
  coin_history <- finanzR::add_coin_price(input = prepare, coins = all_coins, base_currency = {{base_currency}})

  staking <- finanzR::kraken_staking(coin_history, prepared = TRUE, base_currency = {{base_currency}})

  cli::cli_inform(c(i = "create import file"))

  output <- coin_history %>%
    dplyr::filter(staking == FALSE & staking_start == FALSE & staking_end == FALSE) %>% # remove all staking transactions
    finanzR::kraken_rename_values() %>%
    rbind(staking) %>%
    dplyr::arrange(timestamp) %>%
    finanzR::pp_rename_columns()

  write.table(output, "pp_import_kraken_main_account.csv", sep = ";", row.names = FALSE, na = "")
  cli::cli_inform(c(i = 'you can now import the "pp_import_kraken" csv files in your working directory'))
}
