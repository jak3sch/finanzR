#' Load all supported currencies
#'
#' @description Load all supported currencies. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.
#'
#' @return Returns a tibble with the following columns:
#' * `symbol` (character): currency symbol (e.g. USD, EUR...);
#' * `name` (character): currency name;
#' *`currency_id` (character): currency id to sync with kraken assets (e.g. ZEUR)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' r <- all_currencies()
#' r

all_currencies <- function() {
  kraken_cash_to_crypto_coin_currencies <-c("USD", "EUR", "CAD", "JPY", "GBP", "CHF", "AUD")
  currency_names <- c("US Dollar", "Euro", "Canadian Dollar", "Yen", "Pound Sterling", "Swiss franc", "Australian Dollar")

  currencies <- tibble::tibble(kraken_cash_to_crypto_coin_currencies, currency_names, .name_repair = ~ c("symbol", "name")) %>%
    dplyr::mutate(currency_id = paste0("Z", symbol))

  return(currencies)
}
