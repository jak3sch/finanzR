#' Basic data manipulation for ledgers.csv from kraken
#'
#' @description Runs basic data manipulation and normalisation to an exported ledgers.csv from kraken.com.
#'
#' @param input A `string` with the path to the exported Kraken ledgers data or a `data.frame`. If you are not passing an exported `ledgers.csv` from Kraken, the data needs the columns:
#' * `refid`: a unique id for each transaction
#' * `time`: date and time in the format "Date Time". [`split_col_value`]() is performed on this column.
#' * `type`: the type of the transaction (e.g. deposit, withdrawal, trade...)
#' * `asset`: the asset of the transaction
#' * `amount`: amount of asset in transaction
#'
#' @return Returns the input data with additional columns:
#' * `currency` (character): currency transactions getting a new column with the used currency
#' * `deposit` (true/false): transaction indicator
#' * `dividend` (true/false): transaction indicator
#' * `receive` (true/false): transaction indicator
#' * `spend` (true/false): transaction indicator
#' * `staking` (true/false): transaction indicator
#' * `trade` (true/false): transaction indicator
#' * `transfer` (true/false): transaction indicator
#' * `withdrawal` (true/false): transaction indicator
#' * `date` (character): date of the staking transaction
#' * `time` (character): time of the staking transaction
#' * `note` (character): a note with a summary of the original transaction (amount, asset, type, timestamp, refid)
#' * `symbol` (character): a cleaned up version of the asset column
#' * `coin_id` (character): the coin_id joined from [`all_coins()`]()
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
#'   asset = "ZEUR",
#'   amount = c("1000", "200", "350", "4000", "100")
#' )
#'
#' r <- kraken_ledgers_prepare(ledgers)
#' r

kraken_ledgers_prepare <- function(input) {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  currencies <- finanzR::all_currencies(filter = "kraken") %>%
    dplyr::select(symbol, kraken_asset)

  output <- input_data %>%
    dplyr::group_by(refid) %>%

    # join currency symbols
    dplyr::left_join(
      currencies,
      by = c("asset" = "kraken_asset")
    ) %>%

    #fix kraken naming conventions (leading X for old crypto currencies)
    dplyr::mutate(
      symbol = dplyr::case_when(
        !asset %in% c("XCN", "XRT", "XTZ") ~ stringr::str_replace(asset, "^X", ""), # remove leading X from coins like XXBTC
        TRUE ~ asset
      ),

      # remove .s from staking assets
      symbol = stringr::str_replace(symbol, "\\.S", ""),

      # kraken uses a different symbols in ledgers
      symbol = dplyr::case_when(
        symbol == "XBT" ~ "BTC",
        symbol == "XDG" ~ "DOGE",
        symbol == "LUNA" ~ "LUNC",
        symbol == "LUNA2" ~ "LUNA",
        TRUE ~ symbol
      )
    ) %>%

    dplyr::rename(timestamp = time) %>%
    dplyr::mutate(
      # create cols for each type to categorize groups with multiple entries per transaction
      deposit = any(type == "deposit"),
      dividend = any(type == "dividend"),
      receive = any(type == "receive"),
      spend = any(type == "spend"),
      staking = any(type == "staking") | any(stringr::str_detect(asset, ".S")) | any(subtype %in% c("stakingtospot", "stakingfromspot", "spottostaking", "spotfromstaking")),
      trade = any(type == "trade"),
      transfer = any(type == "transfer"),
      withdrawal = any(type == "withdrawal"),
      currency = asset %in% currencies$kraken_asset,
      note = paste(amount, asset, type, "from", timestamp, "refid:", refid),
    ) %>%
    dplyr::ungroup() %>%

    # add geckor asset id by symbol
    #dplyr::left_join(
    #  finanzR::all_coins() %>%
    #    dplyr::select(-name),
    #  by = "symbol"
    #) %>%

    # create separate columns for date and time
    finanzR::split_col_value(target = timestamp, colnames = c("date", "time"))

  return(output)
}
