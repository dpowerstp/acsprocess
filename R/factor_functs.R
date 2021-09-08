#' Race factor
#' Orders race column in given data-frame for consistency in plot_ly coloring, based on what races present in dataframe
#'
#' @param df Dataframe with race column
#' @param race_col Name of race column
#'
#' @return Dataframe with race column as factor
#' @export
#'
#' @examples
race_factor <- function(df, race_col = race){

  race_vec <- c("White",
                "Black",
                "Hispanic",
                "Asian",
                "Other",
                "Multiracial",
                "NHPI",
                "AIAN",
                "Overall")

  pres_race <- df %>%
    dplyr::pull({{race_col}}) %>%
    unique()

  race_vec <- race_vec[race_vec %in% pres_race]

  if (!all(pres_race %in% race_vec)){
    stop("Error; some race-values in dataframe not present in ordered race vector")
  }

  df %>%
    dplyr::mutate({{race_col}} := factor({{race_col}}, race_vec, race_vec))
}
