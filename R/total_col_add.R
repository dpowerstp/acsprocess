#' Create tidycensus total column
#'
#' A function to pull one or multiple total values in a tidycensus dataset for a group or groups from a row and make it into a column. This is best used after separate_label; the group total will be NA after the label is separated, so you enter the name of the group column as the total_col value e.g., ("pop_total" = race_ethnicity) to create a column for the population total in a dataset where race_ethnicity has the racial/ethnic group for each row.
#'
#' @param df a tidycensus dataframe with a total represented as one row
#' @param total_cols a list, formatted as list(name of new column = name column to pull total for). If you've separated the label column, the name of column to pull for will be NA.
#' @param join_col a column or multiple columns to join the totals to. Often name, unless a race-disaggregated dataset without an aggregate total across races (e.g., race by age). If multiple totals are being calculated (i.e., length of total_cols > 1), then the columns are joined in sequence (e.g., pulling totals for race, then race by age, then race by age by gender). In these cases, the join_col will usually be a vector consisting of the unique identifier column (usually "name") followed by the values (not names) of the total_cols list except for the last value. If the there is one total column and multiple join columns, the columns are joined all at once (e.g., to generate unique totals for a place and race). In situations involving a sequence where a unique observation represents a combination of two variables, it's best to merge the two variables in advance to one column.
#' @param est_col the name of the column containing values for each group; unless renamed or transformed, estimate
#'
#' @return a tidycensus dataset with totals for different groups as columns
#' @export
#'
#' @examples
total_col_add <- function(df,
                          total_cols,
                          join_col = "name",
                          est_col = "estimate") {
  return_df <- df %>%
    dplyr::rename_all(tolower)

  total_cols_vec <- total_cols

  total_cols_name <- names(total_cols)

  # loop through list of total columns to pull
  purrr::walk(seq_along(total_cols_vec), ~ {

    select_tot_col <- total_cols_vec[[.x]]
    select_cols_name <- total_cols_name[[.x]]

    if (length(join_col) > 1 & length(total_cols) > 1){
      # subset join columns
      join_col <-join_col[1:.x]
    }

    # create name of moe for total
    group_moe <- paste0(select_cols_name, "_moe")

    # filter to total
    total_df <- return_df %>%
      dplyr::filter(is.na(!!dplyr::sym(select_tot_col)))

    # update moe to 0 if missing - per this discussion - https://github.com/walkerke/tidycensus/issues/29

    # pull total value and moe for that (na value of subgroup column = total for that group
    total_df <- total_df %>%
      dplyr::mutate(!!dplyr::sym(select_cols_name) := !!dplyr::sym(est_col),
                    !!dplyr::sym(group_moe) := ifelse(is.na(moe), 0, moe))

    # filter to total column and its moe as well as column to rejoin data by
    total_df <- total_df %>%
      dplyr::select(c(join_col, select_cols_name, group_moe))

    # join total data to df and filter out total-row
    return_df <<- return_df %>%
      dplyr::left_join(total_df) %>%
      dplyr::filter(!is.na(!!dplyr::sym(select_tot_col)))

  })

  group_vec <- c(join_col, unname(unlist(total_cols, use.names = F)), "variable", est_col)

  df_check <- return_df %>%
    dplyr::group_by(dplyr::across(group_vec)) %>%
    dplyr::mutate(freq = dplyr::n())

  if (any(df_check %>% dplyr::pull(freq) > 1) | nrow(df_check) > nrow(df)){
    browser()

    stop("Observations that are supposed to be unique repeat; check whether join columns and total columns align")
  }

  return(return_df)
}
