#' Create import data for Portfolio Performance
#'
#' @param df
#' @param type
#' @param file
#'
#' @export
pp_create_import <- function(type, file, lang = "de") {
    if (type == "kraken_staking") {
        df <- finanzR::kraken_staking(file)
        df <- finanzR::pp_rename_columns(df)

        finanzR::write_csv(df, output = "pp_kraken_staking.csv") # nolint: line_length_linter.
    }
}