#' Rename columns for Portfolio Performance import
#'
#' @description Translates tibble column names for Portfolio Performance import. Columns that will be renamed: `date`, `time`, `amount`, `price`, `symbol`, `type`, `fee`
#'
#' @param df A `tibble.` Currently optimized for Kraken ledgers export.
#' @param lang Currently only `"de"`.
#'
#' @importFrom magrittr %>%
#'
#' @export

pp_rename_columns <- function(df, lang = "de") {
    if (lang == "de") {
        col_names <- c(
            "Datum" = "date",
            "Zeit" = "time",
            "Stueck" = "amount",
            "Wert" = "price",
            "Ticker-Symbol" = "symbol",
            "Typ" = "type",
            "Gebuehr" = "fee",
            "Buchungswaehrung" = "currency"
        )
    }

    df <- df %>%
        dplyr::rename(dplyr::any_of(col_names))

    return(df)
}
