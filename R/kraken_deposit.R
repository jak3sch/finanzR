#' fdgfdgf
#'
#' @description Adds coin values from CoinGecko API to Kraken staking transactions
#'
#' @param input A `string` with the path to the exported Kraken ledgers data or a `data.frame` with the same strucutre as the kraken ledgers export.
#' @param base_currency A `string` with the prefered currency in which the coin value will be returned. One of `c("eur", "usd")`. If the result will be imported in Portfolio Performance, this should be the same as the base currency set there.
#' @param input_type A `string` which defines what kind of input you are passing. If you are not passing a exported `ledgers.csv` from Kraken the data needs the columns `time`, `type`, `asset` and `amount.`
#'
#' @return A tibble with the following columns:
#' * `date` (date): date of the staking transaction;
#' * `time` (character) time of the staking transaction;
#' * `type` (character): staking;
#' * `asset` (character): Kraken coin abbreviation;
#' * `amount` (double): amount of staked coins;
#' * `fee` (double): transaction fee;
#' * `price` (double): coin price in `base_currency`, as of `date`;
#' * `currency` (character): currency of coin value;
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' kraken_data <- data.frame(
#'   time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
#'          "2022-04-18 09:07:51", "2022-04-19 02:46:48", "2022-04-25 09:11:56",
#'          "2022-04-26 02:48:12"),
#'   type = "staking",
#'   asset = c("TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S", "ADA.S", "TRX.S"),
#'   amount = c(0.4, 0.55, 10.76, 0.55, 10.77, 0.55, 10.79)
#' )
#'
#' r <- kraken_deposit(kraken_data, input_type = "data.frame")
#' head(r, 10)

kraken_deposit <- function(input, base_currency = "eur", input_type = "file", lang = "de") {
  if (input_type == "file") {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  input <- input_data %>%
    dplyr::filter(deposit == TRUE) %>%

    # check, if group elemnts have the same asset
    dplyr::group_by(refid) %>%
    dplyr::mutate(
      same_asset = asset == dplyr::lag(asset) # add col for currency transactions, which have two different deposits
    ) %>%
    dplyr::ungroup() %>%

    # filter necessary rows
    dplyr::filter(
      !is.na(currency) & same_asset == TRUE # filter one row from currency transactions
    ) %>%

    # cleanup
    dplyr::mutate(
      asset = ifelse(is.na(currency), asset, NA),
      price = ifelse(!is.na(currency), amount, NA)
    ) %>%
    dplyr::select(date, time, type, asset, amount, price, fee, currency)

  if(lang == "de") {
    output <- input %>%
      dplyr::mutate(
        type = dplyr::case_when(
          type == "deposit" & !is.na(currency) ~ "Einlage",
          TRUE ~ type
        )
      )
  }

  return(output)
}
