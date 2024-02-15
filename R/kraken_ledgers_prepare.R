#' Basic data manipulation for ledgers.csv from kraken
#'
#' @description Runs basic data manipulation and normalisation to an exported ledgers.csv from kraken.com.
#'
#' @param input A `string` with the path to the exported Kraken ledgers data or a `data.frame`. If you are not passing an exported `ledgers.csv` from Kraken, the data needs the columns:
#' * `refid`: a unique id for each transaction
#' * `type`: the type of the transaction (e.g. deposit, withdrawal, trade...)
#' * `time`: date and time in the format YYYY-MM-DD HH:MM:SS
#'
#' @return Returns the input data with additional columns:
#' * `date` (date): date of the staking transaction;
#' * `time` (character): time of the staking transaction;
#' * `symbol` (character): the symbol of the used asset
#' * `currency` (character): currency transactions getting a new column with the used currency
#' * `deposit` (true/false): transaction indicator
#' * `dividend` (true/false): transaction indicator
#' * `receive` (true/false): transaction indicator
#' * `spend` (true/false): transaction indicator
#' * `staking` (true/false): transaction indicator
#' * `trade` (true/false): transaction indicator
#' * `transfer` (true/false): transaction indicator
#' * `withdrawal` (true/false): transaction indicator
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' r <- kraken_ledgers_prepare()
#' head(r, 10)

kraken_ledgers_prepare <- function(input) {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  currencies <- finanzR::all_currencies() %>%
    dplyr::pull(currency_id)

  output <- input_data %>%
    dplyr::group_by(refid) %>%
    dplyr::left_join(
      finanzR::all_currencies() %>%
        dplyr::select(currency_id, symbol),
      by = c("asset" = "currency_id")
    ) %>%
    dplyr::rename(currency = symbol) %>%
    dplyr::mutate(
      # create cols for each type to categorize groups with multiple entries per transaction
      deposit = any(type == "deposit"),
      dividend = any(type == "dividend"),
      receive = any(type == "receive"),
      spend = any(type == "spend"),
      staking = any(type == "staking"),
      trade = any(type == "trade"),
      transfer = any(type == "transfer"),
      wihthdrawal = any(type == "withdrawal")
    ) %>%
    dplyr::ungroup() %>%

    # create separate columns for date and time
    finanzR::split_datetime(colname = time)

  return(output)
}
