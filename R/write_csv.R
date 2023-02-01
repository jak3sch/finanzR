#' Write to local csv file
#'
#' @param df
#' @param output
#'
#' @export
write_csv <- function(df, output) {
    write.csv(df, output, row.names = FALSE)
}