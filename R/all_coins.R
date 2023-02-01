#' Load all supported Coins
#'
#' Load all supported crypto coins from the [geckor](https://github.com/next-game-solutions/geckor) package.
#'
#' @return A dataframe with the following columns:
#' * `timestamp` (POSIXct);
#' * `coin_id` (character): same as the argument `coin_id`;
#' * `vs_currency` (character): same as the argument `vs_currency`;
#' * `price` (double): coin price, as of `timestamp`;
#' * `total_volume` (double): a 24 hours rolling-window trading volume, as
#' of `timestamp`;
#' * `market_cap` (double): market capitalisation, as of `timestamp`.
#'
#' @export
all_coins <- function() {
    if (exists("coins")) {
        return(coins)
    } else {
        coins <- geckor::supported_coins()
        return(coins)
    }
}