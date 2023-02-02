#' Create import file for staking from Kraken
#'
#' @param file (string): path to the exported ledgers.csv file from Kraken
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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export

kraken_staking <- function(input, currency = "EUR", input_type = "file") {
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

  used_coin_hinstory <- geckor::coin_history(
    coin_id = unique(input$coin_id),
    days = "max",
    vs_currency = "eur",
    interval = "daily"
    ) %>%
    dplyr::mutate(date = base::as.Date(timestamp))

  staking <- input %>%
    dplyr::mutate(time = stringr::str_split(.data$time, " ")) %>%
    tidyr::unnest_wider(.data$time, "") %>%
    dplyr::rename("time" = "time2") %>%
    dplyr::mutate(date = as.Date(.data$time1)) %>%
    dplyr::left_join(
      used_coin_hinstory %>%
        dplyr::select(.data$date, .data$coin_id, .data$price),
      by = c("date", "coin_id")
    ) %>%
    dplyr::mutate(
      "symbol" = paste(toupper(.data$asset), currency, sep = "/")
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "date",
      "time",
      "type",
      "symbol",
      "amount",
      "fee",
      "price"
    )))

  dividend <- staking %>%
    dplyr::mutate(type = "Dividende")

  buy <- staking %>%
    dplyr::mutate(type = "Kauf")

  return(rbind(dividend, buy))
}