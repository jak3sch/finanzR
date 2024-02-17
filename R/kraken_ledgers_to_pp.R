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
kraken_ledgers_to_pp <- function(input, base_currency = "eur", input_type = "file") {
  if (input_type == "file") {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  all_coins <- crypto2::crypto_list(only_active = FALSE)

  prepare <- finanzR::kraken_ledgers_prepare(input = ledgers)

  transactions <- prepare %>%
    #dplyr::filter(type %in% c("trade") & staking == FALSE) %>%

    # handle multiple entries per transaction
    dplyr::group_by(refid) %>%
    dplyr::arrange(type, currency) %>% # create same oder in each group (currency necessary for trades)
    dplyr::mutate(
      price = dplyr::case_when(
        # combine separate spend/receive entries
        type == "receive" & currency == FALSE ~ dplyr::lead(amount), # combine entries from coin buys
        type == "spend" & currency == FALSE ~ dplyr::lag(amount), # combine entries from coin sells

        # withdrawal
        type == "withdrawal" & currency == TRUE ~ amount,

        # trades
        type == "trade" & staking == FALSE & currency == FALSE ~ dplyr::lead(amount)
      ),
      fee = dplyr::case_when(
        type == "spend" & currency == FALSE ~ dplyr::lag(fee) # get fee from coin selling  transaction
      )
    ) %>%
    dplyr::filter(
      currency == FALSE | # all coin transactions
        !(currency == TRUE & type == "deposit" & is.na(balance)), # remove duplicates from currency deposits
      !(type == "spend" & currency == TRUE & is.na(price)), # remove spend transactions from coin buys
      !(type == "receive" & currency == TRUE & is.na(price)), # remove receive transaction from coin sells
      !(type == "withdrawal" & staking == FALSE & is.na(balance)), # remove duplicates from withdrawals
      !(type == "trade" & staking == FALSE & currency == TRUE), # remove duplicates from coin trades
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      amount = ifelse(amount < 0, amount * -1, amount),
      price = ifelse(price < 0, price * -1, price),
      price = ifelse(is.na(price), amount, price), # add a price to transactions where non is existent (e.g. currency transactions)
      price = ifelse(currency == FALSE, (price / amount) * 1000, price)
    )
    dplyr::filter(refid == "TSDIUJS-3JTKJ-Y4ZB34") %>%
    dplyr::select(-(deposit:currency))

  # only needed for staking
  #coin_history <- finanzR::add_coin_price(input = prepare, coins = all_coins, base_currency = base_currency)

  if(lang == "de") {
    translations <- transactions %>%
      dplyr::mutate(
        type = dplyr::case_when(
          type == "deposit" & currency == TRUE ~ "Einlage", # deposit of currency
          type == "receive" & currency == FALSE ~ "Kauf", # receive coin
          type == "spend" & currency == FALSE ~ "Verkauf",
          #type == "transfer" ~ "Umbuchung",
          type == "withdrawal" & currency == TRUE ~ "Entnahme", # withdrawal currency
          type == "withdrawal" & currency == FALSE ~ "Verkauf", # withdrawal coin
          type == "trade" & amount < 0 ~ "Verkauf", # trade coin
          type == "trade" & amount > 0 ~ "Kauf", # trade coin
          TRUE ~ type
        )
      )
  }

  output <- translations %>%
    dplyr::filter(staking == FALSE, !(type %in% c("deposit", "dividend")), subtype != "spotfromfutures") %>%

    dplyr::mutate(
      fee = ifelse(is.na(fee), 0, fee),
      symbol =  ifelse(currency == FALSE, paste(symbol, toupper(base_currency) , sep = "/"), NA),
      currency = toupper(base_currency)
    ) %>%
    dplyr::select(date, time, type, symbol, amount, price, fee, currency, note) %>%
    finanzR::pp_rename_columns()

  finanzR::write_csv(output, output = "pp_kraken_test.csv")
}

all_coins
