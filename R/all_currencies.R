#' Load all supported currencies
#'
#' @description Load all supported currencies. It uses the [crypto2 fiat_list()](https://sstoeckl.github.io/crypto2/reference/crypto_list.html) as source
#'
#' @param filer A `string` to filter currencies for a specific provider (currently only `"kraken"`).
#'
#' @return Returns a tibble with the following columns:
#' * `name` (character): currency name
#' * `sign` (character): currency special character
#' * `symbol` (character): international currency symbol (e.g. USD, EUR...)
#' * `kraken_asset` (character): sync helper for kraken ledger transaction
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' r <- all_currencies()
#' r

all_currencies <- function(filter = "") {
  currencies <- crypto2::fiat_list(include_metals = FALSE) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(kraken_asset = paste0("Z", symbol))

  if(filter == "kraken") {
    kraken_cash_to_crypto_coin_currencies <-c("USD", "EUR", "CAD", "JPY", "GBP", "CHF", "AUD")

    currencies <- currencies %>%
      dplyr::filter(symbol %in% kraken_cash_to_crypto_coin_currencies)
  }

  return(currencies)
}
