#' Splits a column value in to multiple columns
#'
#' @description Splits a column value on a passed character and stores each element in separate columns.
#'
#' @param input A `data.frame`
#' @param target A colname `` where the value should be split.
#' @param sep A `string` on which the value should be split (default = `" "`).
#' @param colnames A `list` with the new colnames. The `target` colname should not be included in this parameter.
#'
#' @return Returns the input data with new columns named by the `colnames` parameter
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   timestamp = c("2022-04-05 03:24:26", "2022-04-11 17:51:44", "2022-04-12 02:43:49",
#'          "2022-04-18 09:07:51", "2022-04-19 02:46:48")
#' )
#'
#' r <- split_col_value(input = data, target = timestamp, colnames = c("date", "time"))
#' r

kraken_rename_values <- function(input, provider = "kraken", lang = "de", base_currency = "eur") {
  if (is.character(input)) {
    input_data <- utils::read.csv(input)
  } else {
    input_data <- input
  }

  if(lang == "de") {
    lang_deposit <- "Einlage"
    lang_buy <- "Kauf"
    lang_sell <- "Verkauf"
    lang_withdrawal <- "Entnahme"
    lang_fee <- "Gebühren"
    lang_transfer_out <- "Umbuchung (Ausgang)"
    lang_dividend <- "Dividende"
  }

  output <- input_data %>%
    # translations for import
    dplyr::mutate(
      type = dplyr::case_when(
        type == "deposit" & currency == TRUE ~ lang_deposit, # deposit of currency
        type == "receive" & currency == FALSE ~ lang_buy, # receive coin
        type == "spend" & currency == FALSE ~ lang_sell,
        type == "withdrawal" & currency == TRUE ~ lang_withdrawal, # withdrawal currency
        type == "withdrawal" & currency == FALSE ~ lang_sell, # withdrawal coin
        type == "trade" & amount < 0 ~ lang_sell, # trade coin
        type == "trade" & amount > 0 ~ lang_buy, # trade coin
        type == "fee" ~ lang_fee,
        type == "transfer" & staking_start == TRUE ~ lang_transfer_out,
        type == "transfer" & staking_end == TRUE ~ lang_transfer_out,
        type == "deposit" & staking_start == TRUE ~ lang_buy,
        type == "deposit" & staking_end == TRUE ~ lang_buy,
        type == "deposit" & transfer == FALSE ~ lang_buy,
        type == "dividend" ~ lang_dividend,
        TRUE ~ type
      ),
      # final fixes
      fee = ifelse(is.na(fee), 0, fee),
      symbol =  ifelse(currency == FALSE, paste(symbol, toupper({{base_currency}}) , sep = "/"), NA),
      currency = toupper({{base_currency}})
    )

  return(output)
}
