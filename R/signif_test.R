#' Significance test for ACS estimates
#'
#' @param est1 First estimate to compare
#' @param est2 Second estimate to compare
#' @param moe1 MOE of first estimate
#' @param moe2 MOE of second estimate
#' @param just_val Whether to return just the level of confidence or a list with the critical value and level of confidence
#'
#' @return A single value of whether/at what level of confidence the estimates differ statistically, or a list with the critical value and first value
#' @export
#'
#' @examples
signif_test <- function(est1, est2, moe1, moe2, just_val = F){

  se1 <- moe1 / 1.645
  se2 <- moe2 / 1.645

  se1_sqr <- se1 ^ 2
  se2_sqr <- se2 ^ 2

  z_score <- abs((est1 - est2) / sqrt(se1_sqr + se2_sqr))

  crit_val <- dplyr::case_when(z_score > 2.576 ~ 0.99,
                        z_score > 1.960 ~ 0.95,
                        z_score > 1.645 ~ 0.9)

  if (just_val){
    return(crit_val)
  }

  vals <- list("z_score" = z_score,
               "crit_val" = crit_val)

  return(vals)

}

#' Compare significance
#' Identifies statistical significance of differences between a group in a dataframe and other groups.
#'
#' @param df A dataframe with estimate and margin of error columns for different groups.
#' @param filter_col The column containing different groups, one of which you want to compare.
#' @param filter_val A value that filters the filter column to one unique group. E.g., if the filter column is age-groups, the filter value should pull one age group.
#' @param join_col Columns to join the filtered dataframe to the original dataframe by. Columns should result in one-to-one matches between filtered dataframe and original. E.g., if you want to compare differences for 20-30 year olds in a city against all other age groups in the same city, your join columns should be the city column.
#' @param est_col The column containing the value you're comparing.
#' @param moe_col The column containing the moe you're comparing.
#'
#' @return A dataframe containing four new columns for the estimate/moe of the compare-group, the critical value of the difference between the compare group and other groups, and the statistical-significance level of the difference.
#' @export
#'
#' @examples
signif_compare <- function(df,
                           filter_col,
                           filter_val,
                           join_col,
                           est_col,
                           moe_col){

  # remove spaces/regular expression values
  filter_name <- filter_val %>%
    tolower %>%
    gsub(" ", "_", .) %>%
    gsub("(^)|($)", "", .)

  # generate column names
  est_filter <- paste0(filter_name, "_est")

  moe_filter <- paste0(filter_name, "_moe")

  crit_filter <- paste0("crit_", filter_name)

  signif_filter <- paste0("signif_", filter_name)

  # create dataframe of values to compare original df values against
  filter_df <- df %>%
    # ungroup data in case grouped
    dplyr::ungroup() %>%
    dplyr::filter(grepl(filter_val, {{ filter_col }}, ignore.case = T))

  if ((filter_df %>%
       dplyr::pull({{ filter_col }}) %>%
       unique %>%
       length) > 1){
    stop("Error; multiple unique values returned in filtered column; check precision of filter_val")
  }

  filter_df <- filter_df %>%
    dplyr::select({{join_col}}, {{ est_col }}, {{ moe_col }}) %>%
    dplyr::distinct() %>%
    dplyr::rename(!!sym(est_filter) := {{ est_col }},
           !!sym(moe_filter) := {{ moe_col }})

  if (nrow(filter_df) == 0){
    stop("Error; empty filtered dataframe; check filter column")
  }

  # browser()
  # calculate statistical significance
  return_df <- df %>%
    dplyr::left_join(filter_df) %>%
    dplyr::mutate(!!sym(crit_filter) := acsprocess::signif_test({{ est_col }},
                                             !!sym(est_filter),
                                             {{ moe_col }},
                                             !!sym(moe_filter))[[1]],
           !!sym(signif_filter) := acsprocess::signif_test({{ est_col }},
                                               !!sym(est_filter),
                                               {{ moe_col }},
                                               !!sym(moe_filter))[[2]])

  if (nrow(return_df) > nrow(df)){
    browser()
    stop("Error; rows in return df exceed original dataframe. Check join columns")
  }

  return_df

}

#' Significance overall
#'
#' Function to calculate overall results for a root dataframe, and compare significance of group-differences for a grouped dataframe.
#'
#' @param processed_df Processed dataframe where each row represents a group to compare against the overall.
#' @param root_df Lightly-processed ACS dataframe with estimate and moe column. Estimates should be components of a whole (i.e., not median income, but something like population). E.g., if the root_df describes computer access by sex and age, and the processed_df describes computer access by age group.
#' @param group_cols The set of columns to calculate overall results for. E.g., if you want to see how age groups differ from overall rates of access, you would enter the computer access column as the group col.
#' @param est_col The name of the value column in the processed dataset.
#' @param moe_col The name of the moe column in the processed dataset.
#' @param bind_overall Whether to add overall results as a row to the processed dataframe. Should enter name of column with groups comparing against (e.g., if comparing ages against overall, should enter age).
#'
#' @return The processed dataframe with additional columns for the overall estimate and MOE, the critical value of groups differences from the overall difference, and the overall statistical significance of results. If bind_overall is not null, also has an additional row for overall results.
#' @export
#'
#' @examples
signif_overall <- function(processed_df,
                           root_df,
                           group_cols,
                           est_col,
                           moe_col,
                           bind_overall = NULL){

  overall_df <- root_df %>%
    dplyr::ungroup() %>%
    # calculate overall results for all groups but last group
    acsprocess::est_moe_derive(group_cols = group_cols[1:(length(group_cols)-1)],
                   name_col = "pop_total") %>%
    acsprocess::est_moe_derive(group_cols = group_cols,
                   name_col = "overall") %>%
    dplyr::select(group_cols, pop_total_est, pop_total_moe, overall_est, overall_moe) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe("pct_overall",
                       "pop_total_est",
                       "pop_total_moe",
                       "overall_est",
                       "overall_moe")

  # prepare dataset to join
  overall_df_join <- overall_df %>%
    dplyr::select(group_cols, pct_overall, pct_moe) %>%
    dplyr::rename(pct_moe_overall = pct_moe)

  # join and calculate significance
  return_df <- processed_df %>%
    dplyr::ungroup() %>%
    dplyr::left_join(overall_df_join) %>%
    dplyr::mutate(crit_overall = acsprocess::signif_test(est1 = {{ est_col }},
                                      est2 = pct_overall,
                                      moe1 = {{ moe_col }},
                                      moe2 = pct_moe_overall)[[1]],
           signif_overall = acsprocess::signif_test(est1 = {{ est_col }},
                                        est2 = pct_overall,
                                        moe1 = {{ moe_col }},
                                        moe2 = pct_moe_overall)[[2]])

  if (nrow(return_df) > nrow(processed_df)){
    stop("Error; number of rows in return df exceeds processed df")
  }

  # add overall results as category for data
  if (!is.null(bind_overall)){

    # browser()
    overall_df_bind <- overall_df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!sym(bind_overall) := "Overall") %>%
      dplyr::select(group_cols, bind_overall, pct_overall, pct_moe) %>%
      dplyr::rename({{ est_col }} := pct_overall,
             {{ moe_col }} := pct_moe)

    return_df <- return_df %>%
      dplyr::bind_rows(overall_df_bind)
  }

  return(return_df)
}



#' Significance check
#'
#' Loops through named list of columns containing statistical-significance results, and creates new column describing any statistically significant results.
#'
#' @param data Dataframe with columns containing descriptions of significantly significant results against other comparison groups (e.g., another location, overall vs. a group).
#' @param signif_cols Named list with value representing name of the column with significance values, and names representing how significance should be described in the new column (e.g., "Montgomery County" = "signif_mont" would be for a signif_mont column describing the significance of differences with Montgomery County, and Montgomery County is how it would be represented in the new column),
#'
#' @return A dataframe with a column describing all statistically significant differences with comparison groups (e.g., Montgomery County, Maryland, and Overall).
#' @export
#'
#' @examples
signif_checker <- function(data,
                           signif_cols = list("Overall" = "signif_overall",
                                              "MC" = "signif_mont",
                                              "MD" = "signif_maryland")){
  # create blank column
  data <- data %>%
    dplyr::mutate(signif_check = "")

  # rotate through
  purrr::walk2(signif_cols, names(signif_cols), ~ {
    data <<- data %>%
      dplyr::mutate(signif_check = paste0(signif_check,
                                   dplyr::case_when(is.na(!!sym(.x)) ~ "",
                                             TRUE ~ paste0(", ", .y))))
  })

  data %>%
    dplyr::mutate(signif_check = ifelse(signif_check == "",
                                 "None",
                                 gsub("^, ", "", signif_check)))

}


#' Process ACS data
#'
#' Groups lightly-processed tidycensus/ACS dataframe, and identifies statistical-significance of values comparing Takoma Park to Montgomery County and Maryland.
#'
#' @param df Lightly-processed ACS dataframe with estimate and MOE column.
#' @param group_cols Columns to group ACS dataframe by.
#' @param overall_cols The set of columns to calculate overall results for. E.g., if you want to see how age groups differ from overall rates of access, you would enter the computer access column as the group col.
#' @param name_col Base-name of column to store values in for grouped dataframe.
#' @param bind_overall Whether to add overall results as a row to the processed dataframe. Should enter name of column with groups comparing against (e.g., if comparing ages against overall, should enter age).
#' @param root_df If the df has already been processed more, the base-dataframe from which the df was processed.
#'
#' @return
#' @export
#'
#' @examples
process_df <- function(df,
                       group_cols,
                       overall_cols,
                       name_col,
                       bind_overall = NULL,
                       root_df = NULL) {

  # create column names
  name_est <- paste0(name_col, "_est")
  name_moe <- paste0(name_col, "_moe")
  name_pct <- paste0("pct_", name_col)

  # calculate group totals
  processed_df <- df %>%
    acsprocess::est_moe_derive(group_cols = group_cols,
                               name_col = name_col)

  # estimate total of subgroup
  processed_df <- processed_df %>%
    acsprocess::est_moe_derive(group_cols = group_cols[1:(length(group_cols) -1)],
                               name_col = "tot")

  if (is.null(root_df)){
    root_df <- df
  }

  # calculate percent moe, run significance tests
  processed_df <- processed_df %>%
    dplyr::select(group_cols, tot_est, tot_moe, name_est, name_moe) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(name_pct,
                                   "tot_est",
                                   "tot_moe",
                                   name_est,
                                   name_moe) %>%
    acsprocess::signif_overall(root_df = root_df,
                               group_cols = overall_cols,
                               est_col = !!sym(name_pct),
                               moe_col = pct_moe,
                               bind_overall = bind_overall)

  return(processed_df)

}





