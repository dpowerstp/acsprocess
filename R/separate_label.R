#' Separate tidycensus label column
#'
#' tidycensus datasets come with a label function containing information on the row of interest in one string. This function separates the string into multiple columns defined by the names_vector
#'
#' @param df a tidycensus loaded dataset
#' @param names_vector a vector of new column names corresponding to the number of variables created by the separate function. Can enter NA to omit
#' @param label_col the name of the label column; usually label
#'
#' @return a tidycensus dataset with new variables corresponding to the split string
#' @export
#'
#' @examples
separate_label <- function(df, names_vector, label_col = "label"){
  return(df %>%
           dplyr::rename_all(tolower) %>%
           tidyr::separate(!!dplyr::sym(label_col), into = names_vector, sep = "\\!\\!",remove = FALSE))
}
