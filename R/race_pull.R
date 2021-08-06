#' Pull race values as column from tidycensus DF
#'
#' @param df A tidycensus dataframe with race values stored in a string in a column
#' @param race_col The name of the column to store race values in
#' @param col A column from which to extract race values, usually concept
#'
#' @return A tidycensus dataframe with race values stored in a column
#' @export
#'
#' @examples
race_pull <- function(df, race_col = "race", col = "concept") {
  return(df %>%
           dplyr::rename_all(tolower) %>%
           dplyr::mutate(!!sym(race_col) := tolower(gsub(".*\\((.*)\\).*", "\\1", !!sym(col)))))
}
