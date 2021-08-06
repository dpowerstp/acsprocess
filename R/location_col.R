#' Create location column based on ACS dataframe name values
#'
#' @param df Tidycensus acs dataframe with a name column containing place names for Takoma Park, Montgomery County, PG County, and/or Maryland
#'
#' @return A df with a location column with shortened version of location names
#' @export
#'
#' @examples
location_col <- function(df){
  df <- df %>%
    dplyr::mutate(location = dplyr::case_when(grepl("Takoma", name) ~ "Takoma Park",
                                       grepl("Montgomery", name) ~ "Montgomery County",
                                       grepl("Prince George", name) ~ "PG County",
                                       TRUE ~ "Maryland"))

  df
}
