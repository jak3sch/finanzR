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
kraken_ledgers_to_pp <- function(input, base_currency = "eur", input_type = "file") {
  if (input_type == "file") {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  input <- finanzR::kraken_ledgers_prepare(ledgers)

  deposits <- finanzR::kraken_deposit(input, input_type = "data")

  output <- rbind(deposits) %>%
    finanzR::pp_rename_columns()

  finanzR::write_csv(output, output = "pp_kraken_test.csv")
}
