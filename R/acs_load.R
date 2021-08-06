#' acs_load
#'
#' Function to load acs 5-year data for set of variables easily at given geography
#'
#' @param geog a census geography; e.g., "place"
#' @param state a state; can enter NULL to get for nation
#' @param vars_load variables to load based on tidycensus variable list.
#' @param year_val a year to load data from
#'
#' @return ACS dataset
#' @export
#'
#' @examples
acs_load <- function(geog, state, vars_load, year_val = 2019){

  if (is.null(state)){
    tp_data <- tidycensus::get_acs(geography = geog,
                       year = year_val,
                       cache_table = TRUE,
                       survey = "acs5",
                       variables = vars_load,
                       show_call = TRUE)


  }

  else {
    tp_data <- tidycensus::get_acs(geography = geog,
                       year = year_val,
                       cache_table = TRUE,
                       state = state,
                       survey = "acs5",
                       variables = vars_load,
                       show_call = TRUE)
  }

  return(tp_data)

}
