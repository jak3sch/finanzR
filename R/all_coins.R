#' Load all supported Coins
#'
#' @description Load all supported crypto coins from the CoinGecko API via the [geckor](https://github.com/next-game-solutions/geckor) package.
#'
#' @return A tibble with three columns:
#' * `coin_id` (character): coin IDs, ordered alphabetically;
#' * `symbol` (character): coin symbols;
#' * `name` (character): common names of the coins;
#'
#' @export
#'
#' @examples
#' r <- all_coins()
#' head(r, 10)
all_coins <- function() {
    if (exists("coins")) {
        return(coins)
    } else {
        coins <- geckor::supported_coins()
        return(coins)
    }
}
