#' Recode ACS race  values
#'
#' Function to recode a tidycensus ACS dataframe with a race column to shorter labels. This is best used after the race_pull function, which separates the tidycensus dataframe "concept" column into a meaningful race value.
#'
#'
#' @param df A dataframe with tidycensus ACS race values
#' @param race_col The column in the dataframe containing tidycensus ACS race values
#' @param new_race_col The name of the new column with renamed race values as an ordered factor based on what race values are present in the dataframe
#'
#' @return A dataframe with a renamed ACS race column
#' @export
#'
#' @examples

race_recode <- function(df, race_col, new_race_col = "race"){

  # browser()
  race_lookup <- function(string){

    # browser()
    val <- dplyr::case_when(grepl("black or", string, ignore.case = TRUE) ~ "Black",
                     grepl("asian alone", string, ignore.case = TRUE) ~ "Asian",
                     grepl("two or more", string, ignore.case = TRUE) ~ "Multiracial",
                     grepl("some other", string, ignore.case = TRUE) ~ "Other",
                     grepl("white alone", string, ignore.case = TRUE) ~ "White",
                     grepl("hispanic or latin", string, ignore.case = TRUE) ~ "Hispanic",
                     grepl("American Indian and", string, ignore.case = TRUE) ~ "AIAN",
                     grepl("Native Hawaiian", string, ignore.case = TRUE)~ "NHPI",
                     grepl("Overall", string, ignore.case = T) ~ "Overall")

    return(val)

  }

  race_vector_order <- c("White", "Black", "Hispanic", "Asian", "Other", "Multiracial", "AIAN", "NHPI", "Overall")

  return_df <- df %>%
    dplyr::mutate(!!sym(new_race_col) := race_lookup({{race_col}}))

  race_present <- return_df %>%
    dplyr::pull(!!sym(new_race_col))

  race_vector_order <- race_vector_order[race_vector_order %in% race_present]

  return_df <- return_df %>%
    dplyr::mutate(!!sym(new_race_col) := factor(!!sym(new_race_col), race_vector_order, race_vector_order))

  if (any(is.na(return_df[[new_race_col]]))){
    stop("Error; missing race values")
  }

  return_df

}
