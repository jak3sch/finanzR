#' Load all supported currencies
#'
#' @description Load all supported currencies. It uses the [kraken](https://support.kraken.com/hc/en-us/articles/201893658-Currency-pairs-available-for-trading-on-Kraken) cash-to-crypto pairs.
#'
#' @return A character string with all currency IDs
#'
#' @export
#'
#' @examples
#' r <- all_currencies()
#' r

kraken_ledgers_to_pp <- function(input, base_currency = "eur", lang = "de", filter = FALSE) {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  # TODO: if filter is set, filter for transaction types

  message("prepare data")
  prepare <- finanzR::kraken_ledgers_prepare(input = input_data)

  # load all coins
  all_coins <- crypto2::crypto_list(only_active = FALSE)

  # only needed for staking
  coin_history <- finanzR::add_coin_price(input = prepare, coins = all_coins, base_currency = base_currency)

  message("create import file")

  if(lang == "de") {
    lang_deposit <- "Einlage"
    lang_buy <- "Kauf"
    lang_sell <- "Verkauf"
    lang_withdrawal <- "Entnahme"
    lang_fee <- "Gebühren"
  }

  output <- coin_history %>%
    # tmp filter TODO: remove
    dplyr::filter(staking == FALSE, !(type %in% c("deposit", "dividend")), subtype != "spotfromfutures") %>%

    # translations for import
    dplyr::mutate(
      type = dplyr::case_when(
        type == "deposit" & currency == TRUE ~ lang_deposit, # deposit of currency
        type == "receive" & currency == FALSE ~ lang_buy, # receive coin
        type == "spend" & currency == FALSE ~ lang_sell,
        #type == "transfer" ~ "Umbuchung",
        type == "withdrawal" & currency == TRUE ~ lang_withdrawal, # withdrawal currency
        type == "withdrawal" & currency == FALSE ~ lang_sell, # withdrawal coin
        type == "trade" & amount < 0 ~ lang_sell, # trade coin
        type == "trade" & amount > 0 ~ lang_buy, # trade coin
        type == "fee" ~ lang_fee,
        TRUE ~ type
      )
    ) %>%

    # final fixes
    dplyr::mutate(
      fee = ifelse(is.na(fee), 0, fee),
      symbol =  ifelse(currency == FALSE, paste(symbol, toupper(base_currency) , sep = "/"), NA),
      currency = toupper(base_currency)
    ) %>%
    dplyr::select(date, time, type, symbol, amount, price, fee, currency, note) %>%
    finanzR::pp_rename_columns()

  write.table(output, "pp_kraken_test.csv", sep = ";", row.names = FALSE, na = "")
  message("you can now import the csv file in your working directory")
}
