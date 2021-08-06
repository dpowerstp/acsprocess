#' Function to remove extraneous characters from strings
#'
#' @param df a tidycensus dataframe
#'
#' @return a tidycensus dataframe with extraneous characters removed
#' @export
#'
#' @examples
col_formatter <- function(df){

  df %>%
    # filter(!!sym(col_name) != "White alone") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub(":", "", .x)),
                  dplyr::across(dplyr::where(is.character), ~ gsub(" alone", "", .x)),
                  dplyr::across(dplyr::where(is.character), ~ gsub(" or ", "/", .x)))
}

