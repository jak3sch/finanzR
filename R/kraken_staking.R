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

kraken_staking <- function(input, prepared = FALSE, base_currency = "eur") {
  cli::cli_inform(c(i = "create staking data"))

  if (prepared == TRUE) {
    input_data <- input
  } else {
    message("prepare data")
    prepare <- finanzR::kraken_ledgers_prepare(input = input)
    all_coins <- crypto2::crypto_list(only_active = FALSE)
    input_data <- finanzR::add_coin_price(input = prepare, coins = all_coins, base_currency = {{base_currency}})
  }

  staking <- input_data %>%
    dplyr::filter(staking == TRUE | staking_start == TRUE | staking_end == TRUE)

  # 1. coin to staking
  ## 1. withdrawal coin -> sell coin main account
  ## 2. transfer "spottostaking" -> transfer from main to staking account
  ## 3. deposit coin.S -> buy coin staking account
  ## 4. transfer "stakingfromspot" -> transfer from main to staking account

  # 2. staking

  # 3. staking to coin
  ## 1. withdrawal coin.S -> sell coin staking account
  ## 2. transfer "stakingtospot" -> transfer from staking to main account
  ## 3. deposit coin -> buy coin main account
  ## 4. transfer "spotfromstaking" -> transfer from staking to main account

  main_account_transactions <- staking %>%
    dplyr::filter(
      staking_start == TRUE & staking == FALSE |
        staking_end == TRUE & staking == FALSE,
      !(type == "transfer" & staking_end == TRUE) # remove transfer coin (#3. ## 4.) because pp has the option to select an offset account on import
    ) %>%
    finanzR::kraken_rename_values() %>%
    dplyr::mutate(
      symbol = ifelse(
        subtype == "spottostaking" & staking_start == TRUE & staking == FALSE,
        toupper({{base_currency}}),
        symbol
      )
    )

  staking_account_transactions <- staking %>%
    dplyr::filter(
      !(staking_start == TRUE & staking == FALSE),
      !(type == "transfer" & staking_start == TRUE) # remove deposit coin.S (## 3.) because pp has the option to select an offset account on import
    ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "staking" & transfer == FALSE ~ "dividend",
        TRUE ~ type
      ),
    ) %>%
    finanzR::kraken_rename_values() %>%
    finanzR::pp_rename_columns()

  write.table(staking_account_transactions, "pp_import_kraken_staking_account.csv", sep = ";", row.names = FALSE, na = "")

  # return df if prepared == TRUE, else write csv
  if (prepared == TRUE) {
    return(main_account_transactions)
  } else {
    output <- main_account_transactions %>%
      finanzR::pp_rename_columns()

    write.table(output, "pp_import_kraken_main_account.csv", sep = ";", row.names = FALSE, na = "")
  }

    # type == "deposit" & staking == TRUE & transfer == TRUE # sell coins fro staking
    # type == "transfer" & staking == TRUE & transfer == TRUE # umbuchung zu anderem konto

    # handle multiple entries per transaction
    #dplyr::mutate(

    #)
    #dplyr::filter(
    #  !(type == "deposit" & staking == TRUE & transfer == FALSE & is.na(balance)), # remove all staking deposits that are no transfer with no balance
    #) %>%

    #dplyr::mutate(
    #  type = dplyr::case_when(
    #    type == "staking" & transfer == FALSE ~ "dividend",
    #    type == "" & transfer == TRUE ~ "sell",
    #    TRUE ~ type
    #  ),
    #  price_new = dplyr::case_when(
    #    type == "transfer" & staking == TRUE ~ price / amount,
    #    TRUE ~ price
    #  ),
    #  note = ifelse(staking == TRUE, paste("Staking:", note), note) subtype
    #) %>%

    #dplyr::filter(!(type %in% c("dividend", "withdrawal")))


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
}
