#' Group ACS data and calculate derived-MOEs
#'
#' Groups tidy-census ACS dataframe by set of group columns, calculates aggregated estimates, and derives margin of error for aggregated estimates, per ACS instructions.
#'
#' @param df A tidy-census ACS dataframe.
#' @param group_cols Names of columns to group the dataframe by, formatted as a character vector.
#' @param est_col Name of estimate column, typically "estimate", formatted as a character string.
#' @param moe_col Name of margin of error column, typically "moe", formatted as a character string.
#' @param name_col Name of new, aggregated moe column, formatted as character string.
#'
#' @return A dataframe with estimates aggregated across group columns and a derived margin of error.
#' @export
#'
#' @examples
est_moe_derive <- function(df, group_cols, est_col = "estimate", moe_col = "moe", name_col = paste(group_cols, collapse = "_")){

  name_col_est <- paste0(name_col, "_est")
  name_col_moe <- paste0(name_col, "_moe")

  if (any(grepl("est", c(moe_col, name_col_moe)) | any(grepl("moe", c(est_col, name_col_est))))){
    warning("'est' picked up in name of margin of error column or 'moe' picked up in 'name of estimate column; check to make sure names of columns are correct'")
  }

  df %>%
    dplyr::group_by(dplyr::across(group_cols)) %>%
    dplyr::mutate(!!sym(name_col_est) := sum(!!sym(est_col)),
           !!sym(name_col_moe) := sum((!!sym(moe_col))^2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!sym(name_col_moe) := sqrt(!!sym(name_col_moe)))

}
