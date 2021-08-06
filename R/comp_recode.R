#' Recode ACS computer/internet access variables
#'
#' Function to recode tidycensus ACS dataset with a column for computer access and a column for internet access to one column with shortened descriptors.
#'
#' @param df A tidycensus ACS dataset with a column for computer access and one for internet access.
#' @param comp_col The name of the computer access column.
#' @param int_col The name of the internet accesss column.
#' @param new_comp The name of the new column
#'
#' @return A tidycensus ACS dataset with a factor-ordered computer/internet access column
#' @export
#'
#' @examples
comp_recode <- function(df, comp_col, int_col, new_comp = "comp_int"){
  comp_order <- c("Broadband internet",
                  "Dial-up internet",
                  "No internet",
                  "No computer")

  comp_change <- function(string_comp, string_int){

    comp_int <- ifelse(!grepl("No", string_comp), paste0(string_comp, " ", string_int), comp_order[4])

    comp_int <- dplyr::case_when(grepl("Without an Internet", comp_int) ~ comp_order[3],
                          grepl("dial-up", comp_int) ~ comp_order[2],
                          grepl("broadband", comp_int) ~ comp_order[1],
                          TRUE ~ comp_int)

    return(comp_int)
  }

  return <- df %>%
    dplyr::mutate(!!sym(new_comp) := comp_change({{ comp_col }}, {{ int_col }}),
           !!sym(new_comp) := factor(!!sym(new_comp), comp_order, comp_order))

  if (any(is.na(return[[new_comp]]))){
    stop("Error; missing values in return dataset")
  }

  return(return)

}
