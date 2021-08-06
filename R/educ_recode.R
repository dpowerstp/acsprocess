#' Recode education values
#'
#' A function to recode tidycensus education columns to a shorter string for graphing purposes
#'
#' @param df A tidycensus dataframe with an education column
#' @param educ_col The column name of  to the education column in the tidycensus dataframe
#' @param new_educ The name of the new column containing the recoded education values; by edfault, educ_recode
#'
#' @return A dataframe with a recoded education column with shorter names
#' @export
#'
#' @examples
educ_recode <- function(df, educ_col, new_educ = "educ_recode"){

  vec_order <- c("Less than HS",
                 "HS graduate",
                 "Associate's/some college")

  if (any(grepl("Professional", dplyr::pull(df, {{ educ_col }}), ignore.case = T))) {
    vec_order <- c(vec_order, "Bachelor's degree", "Graduate degree")
  }

  else {
    vec_order <- c(vec_order, "Bachelor's or higher")
  }

  educ_change <- function(string){
    case_when(grepl("Less than", string, ignore.case = T) ~ vec_order[1],
              grepl("equivalency", string, ignore.case = T) ~ vec_order[2],
              grepl("Some college", string, ignore.case = T) ~ vec_order[3],
              grepl("Bachelor's", string, ignore.case = T) ~ vec_order[4],
              grepl("Graduate", string, ignore.case = T) ~ vec_order[5],
              TRUE ~ "Missing")
  }

  return <- df %>%
    dplyr::mutate(!!sym(new_educ) := factor(educ_change({{educ_col}}), vec_order, vec_order))

  if (any(return[[new_educ]] == "Missing")){
    stop("Error; returned missing education values")
  }

  return(return)
}

