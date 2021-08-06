#' Derive percent/ratio margin of error for groups
#'
#' A function to calculate the margin of error for proportions or ratios of tidycensus ACS dataframes, which are not included by default. Dataset should be structured with an aggregate column and margin of error uniform across rows, and a column for component values/margins of error. Formula construction follows ACS guidance in "Understanding and Using ACS Data," available here: https://www.census.gov/programs-surveys/acs/guidance/handbooks/general.html.
#'
#' @param df A tidycensus ACS dataframe with rows representing component values and a column with a common group total.
#' @param proportion_col The name of the proportion column to create
#' @param aggregate_est A column representing an aggregate total for the group; the denominator in the proportion
#' @param aggregate_moe A column representing the margin of error for the aggregate estimate
#' @param component_est A column representing the value of the component value for which a proportion is being calculated; the numerator in the proportion
#' @param component_moe A column representing the marging of error of the component value
#' @param type_moe "proportion" or "ratio" for whether a proportion or ratio is being constructed
#'
#' @return A tidycensus dataframe with a calculated proportion or ratio and its margin of error.
#' @export
#'
#' @examples
derive_pct_est_moe <- function (df,
                                proportion_col,
                                aggregate_est,
                                aggregate_moe,
                                component_est = "estimate",
                                component_moe = "moe",
                                type_moe = "proportion") {

  if (any(df[[aggregate_est]] < df[[component_est]]) & type_moe == "proportion"){
    stop("Error; aggregate column should be greater than all component values")
  }

  df <- df %>%
    dplyr::mutate(
      # calculate proportion expressed in terms 100, and set to 0 if na
      !!sym(proportion_col) := !!sym(component_est) * 100 / !!sym(aggregate_est),
      !!sym(proportion_col) := ifelse(is.na(!!sym(proportion_col)), 0, !!sym(proportion_col)),
      # squared proportion of component moe
      component_sqr = (!!sym(component_moe)) ^ 2,
      # squared proportion value returned to non-100 values
      component_pr_ssqr = ((!!sym(proportion_col)) / 100) ^ 2,
      # squared moe of aggregate value
      aggregate_moe_sqr = (!!sym(aggregate_moe)) ^ 2)

  if (type_moe == "proportion"){

    df <- df %>%
      dplyr::mutate(
        # the portion of the formula before it's square-rooted
        sqrt_interior = component_sqr - (component_pr_ssqr * aggregate_moe_sqr),
        # per acs guidance - if difference less than 0, add the two
        sqrt_interior = ifelse(sqrt_interior < 0, component_sqr + (component_pr_ssqr * aggregate_moe_sqr), sqrt_interior),
        # calculate square root of interior part
        sqrt_part = sqrt(sqrt_interior),
        # multiply by 1 / aggregate estimate and express in terms of 100
        pct_moe = 100 * (1 / (!!sym(aggregate_est))) * (sqrt_part),
        # calculate upper bound
        pct_upper = (!!sym(proportion_col)) + pct_moe,
        # calculate lower bound
        pct_lower = (!!sym(proportion_col)) - pct_moe,
        # adjust both if too high/low
        pct_lower = ifelse(pct_lower < 0, 0, pct_lower),
        pct_upper = ifelse(pct_upper > 100, 100, pct_upper))

    # remove intermediate variables
    df %>%
      dplyr::select(-c(component_sqr, component_pr_ssqr, aggregate_moe_sqr, sqrt_interior, sqrt_part))

  }

  else if (type_moe == "ratio"){
    df <- df %>%
      # difference for ratio moes from proportion = add component_sqr and product of ratio squared and aggregate moe squared
      dplyr::mutate(sqrt_part = sqrt(component_sqr + (component_pr_ssqr * aggregate_moe_sqr)),
                    ratio_moe = 100 * (1 / (!!sym(aggregate_est))) * (sqrt_part),
                    ratio_upper = (!!sym(proportion_col)) + ratio_moe,
                    ratio_lower = (!!sym(proportion_col)) - ratio_moe,
                    ratio_lower = ifelse(ratio_lower < 0, 0, ratio_lower))

    # remove intermediate vars
    df %>%
      dplyr::select(-c(component_sqr, component_pr_ssqr, aggregate_moe_sqr, sqrt_part))

  }

  else{
    stop("error; type_moe parameter must be ratio or proportion")
  }

}
