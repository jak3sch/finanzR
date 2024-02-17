#' Load coin values for Kraken staking
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
#' r <- kraken_staking(kraken_data, input_type = "data.frame")
#' head(r, 10)

kraken_staking <- function(input, base_currency = "eur", input_type = "file") {
  coins <- finanzR::all_coins()

  if (input_type == "file") {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  input <- input_data %>%
    dplyr::filter(.data$type == "staking") %>%
    dplyr::mutate(
      asset = tolower(stringr::str_replace(.data$asset, ".S", ""))
    ) %>%
    dplyr::left_join(
      coins,
      dplyr::select(.data$coin_id, .data$symbol),
      by = c("asset" = "symbol")
    ) %>%
    dplyr::group_by(.data$time, .data$asset) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  #used_coin_hinstory <- geckor::coin_history(
  #  coin_id = unique(input$coin_id),
  #  days = "max",
  #  vs_currency = base_currency,
  #  interval = "daily"
  #  ) %>%
  #  dplyr::mutate(date = base::as.Date(timestamp))

  #staking <- input %>%
  #  dplyr::mutate(time = stringr::str_split(.data$time, " ")) %>%
  #  tidyr::unnest_wider(.data$time, "") %>%
  #  dplyr::rename("time" = "time2") %>%
  #  dplyr::mutate(date = as.Date(.data$time1)) %>%
  #  dplyr::left_join(
  #    used_coin_hinstory %>%
  #      dplyr::select(.data$date, .data$coin_id, .data$price),
  #    by = c("date", "coin_id")
  #  ) %>%
  #  dplyr::mutate("currency" = base_currency) %>%
  #  dplyr::select(dplyr::any_of(c(
  #    "date",
  #    "time",
  #    "type",
  #    "asset",
  #    "amount",
  #    "fee",
  #    "price",
  #    "currency"
  #  )))

  return(input)
}
