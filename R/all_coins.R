#' Load all supported coins
#'
#' @description Load all supported crypto coins. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.
#'
#' @return A tibble with three columns:
#' * `coin_id` (character): coin IDs
#' * `symbol` (character): coin symbols
#' * `name` (character): common names of the coins
#'
#' @export
#'
#' @examples
#' r <- all_coins()
#' head(r, 5)

all_coins <- function() {
    coins <- crypto2::crypto_list(only_active = FALSE) %>%
        dplyr::select(name, symbol, slug)

    return(coins)
}
