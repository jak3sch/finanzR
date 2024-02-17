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
            "Typ" = "type",
            "Ticker-Symbol" = "symbol",
            "Stueck" = "amount",
            "Wert" = "price",
            "Gebuehr" = "fee",
            "Buchungswaehrung" = "currency",
            "Notiz" = "note"
        )
    }

    df <- df %>%
        dplyr::rename(dplyr::any_of(col_names))

    return(df)
}
