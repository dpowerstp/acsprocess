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


#' Pull ACS Data
#'
#' Function for downloading acs data at given geography and year
#'
#' @param varlist Vector of variables to download
#' @param filename String to save file as
#' @param year ACS5 year to download data from; default 2020
#' @param geog Geography of data to download; default block group
#' @param varscensus tidycensus downloaded variable names using tidycensus::load_variables; default NULL, which loads in ACS5 variables to merge
#' @param state tidycensus state; default "Maryland"
#'
#' @return Creates data directories to save loaded ACS5 data in
#' @export
#'
#' @examples
acspull <- function(varlist, filename, year = 2020, geog = "block group", varscensus = NULL, state = "Maryland", basedir = "./data"){

  if (is.null(varscensus)){

    varscensus <- tidycensus::load_variables(year = year, "acs5", cache = T)

  }

  acsdata <- tidycensus::get_acs(
    year= 2020,
    variables = varlist,
    geography = geog,
    survey= "acs5",
    state = state,
    geometry = F,
    cache_table = T
  ) %>%
    dplyr::rename_all(tolower)

  # browser()
  # join
  acsdata <- acsdata %>%
    dplyr::left_join(varscensus, by = c("variable" = "name"))

  stateabbr <- state.abb[grep(state, state.name)]

  # create directory to store data if not exist
  acsprocess::quickdircreate(basedir)
  acsprocess::quickdircreate(paste0(basedir, "/", year))
  acsprocess::quickdircreate(paste0(basedir, "/", year, "/", stateabbr, "/"))
  acsprocess::quickdircreate(paste0(basedir, "/", year, "/", stateabbr, "/", geog))

  # save data
  saveRDS(object = acsdata,
          file = paste0(basedir,
                        "/",
                        year,
                        "/",
                        stateabbr,
                        "/",
                        geog,
                        "/",
                        filename,
                        ".rds"))

}


#' Check if directory exists and create it if not
#'
#' Checks if directory exists and create it if not
#'
#' @param dirpath Path to directory checking exists and creating if not
#'
#' @return Created directory
#' @export
#'
#' @examples
quickdircreate <- function(dirpath){

  if (!dir.exists(dirpath)){
    dir.create(dirpath)
  }

}


