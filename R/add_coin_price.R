#' Load historic coin data
#'
#' @description Load all supported currencies. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.Zeitraum von coin zu coin unterschiedlich. fehler erscheinen beim import in pp.
#'
#' @return Returns a tibble with the following columns:
#' * `symbol` (character): currency symbol (e.g. USD, EUR...)
#' * `name` (character): currency name
#' * `currency_id` (character): currency id to sync with kraken assets (e.g. ZEUR)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' r <- coin_history()
#' r

add_coin_price <- function(input, coins = "", base_currency = "eur") {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  if (is.data.frame(coins)) {
    all_coins <- coins
  } else {
    all_coins <- crypto2::crypto_list(only_active = FALSE)
  }

  # filter all not supported but multiple used symbols
  all_coins_filtered <- all_coins %>%
    dplyr::filter(
      !(symbol == "ADA" & slug != "cardano"),
      !(symbol == "DOGE" & slug != "dogecoin"),
      !(symbol == "ETC" & slug != "ethereum-classic"),
      !(symbol == "SHIB" & slug != "shiba-inu"),
      !(symbol == "XRP" & slug != "zcash")
    )

  # get all used coins in input_data
  used_coins <- input_data %>%
    dplyr::filter(is.na(price)) %>% # filter all entries that already have prices
    dplyr::select(symbol) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      all_coins_filtered,
      by = "symbol"
    ) %>%
    dplyr::filter(!is.na(slug))

  # get date of first transaction
  start_date <- input_data %>%
    dplyr::arrange(date) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(date = stringr::str_remove_all(date, "-")) %>%
    dplyr::pull(date)

  # get date of last transaction
  end_date <- input_data %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(date = stringr::str_remove_all(date, "-")) %>%
    dplyr::pull(date)

  # get coin history for all used coins between start end end date
  used_coins_history <- crypto2::crypto_history(
    coin_list = used_coins,
    convert = {{base_currency}},
    start_date = as.character(as.numeric(start_date) - 1),
    end_date = end_date,
    single_id = TRUE # slower, but FALSE returns error
  )

  # split date for high and low
  used_coins_output <- used_coins_history %>%
    dplyr::rename(timestamp_high = time_high, timestamp_low = time_low) %>%
    finanzR::split_col_value(target = timestamp_high, colnames = c("date_high", "time_high")) %>%
    finanzR::split_col_value(target = timestamp_low, colnames = c("date_low", "time_low")) %>%
    dplyr::mutate(
      timestamp = as.character(timestamp)
    )

  output <- input_data %>%
    # join input_data with coin history by date and symbol
    dplyr::left_join(
      used_coins_output %>%
        dplyr::select(timestamp, symbol, open, high, low, close, time_high, time_low),
      by = c("symbol", "date" = "timestamp")
    ) %>%

    # calculate price
    dplyr::mutate(
      diff_open = as.numeric(stringr::str_remove_all(time, ":")),
      diff_close = abs(as.numeric(stringr::str_remove_all(time, ":")) - 235900),
      diff_high = abs(as.numeric(stringr::str_remove_all(time, ":")) - as.numeric(stringr::str_remove_all(time_high, ":"))),
      diff_low = abs(as.numeric(stringr::str_remove_all(time, ":")) - as.numeric(stringr::str_remove_all(time_low, ":"))),
      open_high = (as.numeric(open) + as.numeric(high)) / 2,
      open_low = (as.numeric(open) + as.numeric(low)) / 2,
      close_high = (as.numeric(close) + as.numeric(high)) / 2,
      close_low = (as.numeric(close) + as.numeric(low)) / 2,
      price_day_avg = (as.numeric(open) + as.numeric(close)) / 2,
      price_per_unit = dplyr::case_when(
        (diff_open <= diff_close & diff_high <= diff_low) ~ open_high, # transaction closer to open and high
        (diff_open <= diff_close & diff_high > diff_low) ~ open_low, # transaction closer to open and low
        (diff_open > diff_close & diff_high <= diff_low) ~ close_high, # transaction closer to close and high
        (diff_open > diff_close & diff_high > diff_low) ~ close_low # transaction closer to close and low
      ),
      price = dplyr::case_when(
        staking == TRUE & is.na(price) & !is.na(price_per_unit) ~ price_day_avg, # multiple entries for staking transactions can have very different times. so they all get the daily average
        is.na(price) & !is.na(price_per_unit) ~ price_per_unit * amount, # if no staking entry has no price but a price per unit calculate the new price
        #is.na(price) & !is.na(price_per_unit) & staking == TRUE ~ price_per_unit, # if staking entry has no price but a price per unit calculate the new price
        is.na(price) & currency == TRUE ~ amount, # if entry has no price but is currency (e.g. deposit) use amount as price
        TRUE ~ price
      ),
    ) %>%
    dplyr::select(-dplyr::ends_with("open"), -dplyr::ends_with("close"), -dplyr::ends_with("high"), -dplyr::ends_with("low"), -price_day_avg, -price_per_unit)

  return(output)
}
