#' Pull disability values from concept column
#'
#' @param df ACS dataset on disability-types, contained in one column
#' @param new_col Name of column with disability values pulled
#' @param concept_col Name of column with disability types
#'
#' @return
#' @export
#'
#' @examples
disab_pull <- function(df, new_col = disability, concept_col = concept){
  df %>%
    mutate({{new_col}} := {{concept_col}} %>%
             tolower %>%
             gsub("sex by age by ", replacement = "", x = .) %>%
             gsub(" difficulty", "", .) %>%
             stringr::str_to_title(.))
}


