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

split_col_value <- function(input, target, sep = " ", colnames) {

  original_input <- input

  # only select the target col and split it
  split_input <- original_input %>%
    dplyr::select({{target}}) %>%
    dplyr::mutate(split = stringr::str_split({{target}}, {{sep}})) %>%
    tidyr::unnest_wider(split, "") %>%
    dplyr::select(-{{target}})

  colnames(split_input) <- colnames # change colnames

  output <- dplyr::bind_cols(original_input, split_input)

  return(output)
}
