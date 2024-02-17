#' fdgfdgf
#'
#' @description Adds coin values from CoinGecko API to Kraken staking transactions
#'
#' @param input A `string` with the path to the exported Kraken ledgers data or a `data.frame` with the same strucutre as the kraken ledgers export.
#' @param base_currency A `string` with the prefered currency in which the coin value will be returned. One of `c("eur", "usd")`. If the result will be imported in Portfolio Performance, this should be the same as the base currency set there.
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
#'
#' @export
#'
#' @examples
#' ledgers <- tibble::tibble(
#'   refid = 1:5,
#'   time = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49", "2022-04-18 09:07:51", "2022-04-19 02:46:48"),
#'   type = "deposit",
#'   asset = "ZEUR"
#' )
#'
#' r <- kraken_deposit(ledgers, input_type = "data")
#' head(r, 5)

kraken_deposit <- function(input, base_currency = "eur", lang = "de") {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  deposits <- input_data %>%
    dplyr::filter(deposit == TRUE & type == "deposit") %>%

    # check, if group elements have the same asset
    dplyr::group_by(refid) %>%
    dplyr::mutate(
      same_asset = asset == dplyr::lag(asset) # add col for currency transactions, which have two different deposits
    ) %>%
    dplyr::arrange(same_asset) %>%
    dplyr::filter(
      dplyr::row_number() == 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(timestamp) %>%

    # handle staking deposits
    dplyr::mutate(
      note = ifelse(stringr::str_detect(asset, ".S"), paste("Staking:", note), note)
    ) %>%

    # cleanup
    dplyr::mutate(
      price = ifelse(is.na(price), amount, price)
    ) %>%
    dplyr::select(date, time, type, symbol, amount, price, fee, note)

  if(lang == "de") {
    output <- deposits %>%
      dplyr::mutate(
        type = dplyr::case_when(
          type == "deposit" ~ "Einlage",
          TRUE ~ type
        )
      )
  }

  return(output)
}
