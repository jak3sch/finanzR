#' Write to local csv file
#'
#' @description Write data to a local csv file
#'
#' @param df `data` which is stored in csv file
#' @param output A `string` with the path and filename
#'
#' @export
write_csv <- function(df, output) {
    write.csv(df, output, row.names = FALSE)
}
