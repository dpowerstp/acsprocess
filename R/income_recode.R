#' Recode income values
#'
#' Creates shortened names for incomes in tidycensus downloaded ACS data
#'
#' @param df A tidycensus downloaded dataset with an income column
#' @param income_col The name of the column in the tidycensus dataset containing original income values
#' @param new_income The name of the new, factorized column with shortened income values
#' @param rent_own Whether to revalue the 100k income range to be inclusive of different income ranges for owners and renters (i.e., if you want to compare renter and owner incomes).
#'
#' @return
#' @export
#'
#' @examples
income_recode <- function(df, income_col, new_income = "income_recode", rent_own = F){

  change_income <- function(string){
    gsub("(,000)|(,999)", "k", string) %>%
      gsub(" to ", "-", x =  .) %>%
      gsub("Less than ", "< ", x =  .) %>%
      ifelse(grepl(" or more", ., ignore.case = T), paste0("> ", . ), .) %>%
      gsub(" or more", "", x = .) %>%
      gsub(":", "", .)
  }

  df <- df %>%
    mutate(!!sym(new_income) := change_income({{income_col}}))

  factor_vector <- pull(df, !!sym(new_income)) %>% unique

  factor_vector_big <- grep(">", factor_vector, value = T)
  factor_vector_small <- grep("<", factor_vector, value = T)

  factor_vector <- c(factor_vector_small,
                     factor_vector[!factor_vector %in% c(factor_vector_small, factor_vector_big)],
                     factor_vector_big)

  df <- df %>%
    mutate(!!sym(new_income) := factor(!!sym(new_income), factor_vector, factor_vector))

  if (rent_own){

    rent_own_replacer <- function(string){
      recode(string, "100k-149k" = "Owner: 100k-$150k \n Renter: > 100k",
             "> 100k" = "Owner: 100k-$150k \n Renter: > 100k")
    }

    df <- df %>%
      mutate(!!sym(new_income) := rent_own_replacer(!!sym(new_income)))
  }

  df

}
