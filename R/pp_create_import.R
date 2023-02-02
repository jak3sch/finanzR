#' Create import file for Portfolio Performance
#'
#' @description This function creates a file `pp_kraken_staking.csv` in the current working directory. This file can be imported in Kraken. *IMPORTANT* in the import dialog you have to define the correct formatting for `Wert`, `Stueck` and `Gebuehr`
#'
#' @param type Currently only `"kraken_staking"`. Performs [`kraken_staking`], [`pp_rename_columns`] and [`write_csv`]
#' @param input `String` to file path. Currently it needs to be a Kraken ledgers.csv export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
pp_create_import <- function(type, input) {
    if (type == "kraken_staking") {
        df <- finanzR::kraken_staking(input) %>%
          dplyr::mutate("symbol" = paste(toupper(.data$asset), toupper(.data$currency), sep = "/")) %>%
          dplyr::select(-type, -asset, -currency)

        df <- finanzR::pp_rename_columns(df)

        dividend <- df %>%
          dplyr::mutate(Typ = "Dividende")

        buy <- df %>%
          dplyr::mutate(Typ = "Kauf")

        finanzR::write_csv(rbind(dividend, buy), output = "pp_kraken_staking.csv")
    }
}
