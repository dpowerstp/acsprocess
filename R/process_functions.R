
# Base processing ----

#' Prepare ACS data
#'
#' Function to standardize tidycensus downloaded ACS data, with option to filter to a specific place
#'
#' @importFrom magrittr %>%
#'
#' @param df Downloaded tidycensus ACS dataframe
#' @param place_string String name of tidycensus place to filter data to
#' @param no_pull True/false whether to filter dataframe to tidycensus place
#' @param variables tidycensus dataframe of variables; default 2019
#'
#' @return tidycensus dataframe with column names in lower case and variable labels associated with tidycensus data
#' @export
#'
#' @examples
puller_funct <- function(df, place_string = "", no_pull = FALSE, variables = acsprocess::variables_2019) {

  if (!no_pull){
    df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(grepl(place_string, name)) %>%
      dplyr::left_join(variables, by = c("variable" = "name"))
  }

  else if (no_pull){
    df %>%
      dplyr::rename_all(tolower) %>%
      dplyr::left_join(variables, by = c("variable" = "name"))
  }

}

#' Data puller
#'
#' Function to read in rds data with a common name from a data directory
#'
#' @param root_string Root of file name (e.g., municipalities or counties)
#' @param data_string Name of data directory (e.g., age by sex)
#'
#' @return Dataframe
#' @export
#'
#' @examples
data_puller <- function(root_string, data_string){
  file_name <- paste0("data/", root_string, data_string, ".rds")
  print(file_name)

  read_rds(file_name)

}

#' Save dataframe
#'
#' Saves dataframe to output directories
#'
#' @param object Dataframe to save as rds file
#' @param place_string String name of type of place in dataframe (e.g., municipalities)
#' @param data_string String name of
#' @param desc_year Year of data
#' @param place_dir Name of place directory
#'
#' @return Saved dataframe in created output directories associated with year and place data describes
#' @export
#'
#' @examples
data_saver <- function(object, place_string, data_string, desc_year = "acs5_2019", place_dir = "all"){
  if (!dir.exists(paste0("data/output_data/",
                         desc_year))){

    dir.create(paste0("./data/output_data/",
                      desc_year))
  }

  if (!dir.exists(paste0("data/output_data/",
                         desc_year, "/", place_dir))){

    dir.create(paste0("./data/output_data/",
                      desc_year, "/", place_dir))
  }


  saveRDS(object, file = paste0("data/output_data/",
                                desc_year,
                                "/",
                                place_dir,
                                "/",
                                data_string,
                                "_",
                                place_string,
                                ".rds"))

}

# race ethnicity/ancestry/english proficiency ----


#' Process race and ethnicity data
#'
#' Processes tidycensus downloaded race/ethnicity data
#'
#' @param df tidycensus downloaded race/ethnicity data file
#'
#' @return processed race/ethnicity data
#' @export
#'
#' @examples
process_race_ethn <- function(df){

  race_ethn_processed <- df %>%
    acsprocess::separate_label(c(NA, NA, "race_ethnicity", "two"))

  race_ethn_processed <- race_ethn_processed %>%
    dplyr::mutate(race_ethnicity = ifelse(two == "White alone" & !is.na(two), "NH White alone", race_ethnicity),
           two = ifelse(two == "White alone", NA, two)) %>%
    dplyr::filter(is.na(two) & (!is.na(race_ethnicity) | !grepl("HISPANIC", concept, ignore.case = T)))

  # browser()

  race_ethn_processed <- race_ethn_processed %>%
    acsprocess::total_col_add(total_cols = list("pop_total" = "race_ethnicity")) %>%
    dplyr::select(-c(concept, variable)) %>%
    dplyr::distinct() %>%
    dplyr::filter(!(grepl("Two or more", race_ethnicity) & !is.na(two))) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "pop_total",
                       aggregate_moe = "pop_total_moe") %>%
    dplyr::select(-c(label, two))

}

#' Population totals from race/ethnicity
#'
#' Grab population totals from processed race/ethnciity data
#'
#' @param race_ethn_processed_df Processed race/ethnicity data
#'
#' @return Dataframe with population totals for given place
#' @export
#'
#' @examples
process_pop_tot <- function(race_ethn_processed_df){
  race_ethn_processed_df %>%
    dplyr::select(geoid, name, pop_total, pop_total_moe) %>%
    dplyr::distinct()
}

#' Process race, age, gender
#'
#' Processes tidycensus downloaded race, age, and gender data
#'
#' @param df tidycensus downloaded race, age, and gender data
#'
#' @return Processed race, age, and gender data
#' @export
#'
#' @examples
process_race_age_gender <- function(df){

  df %>%
    acsprocess::separate_label(c(NA, NA, "gender", "age")) %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(race_name = paste0(name, "_", race)) %>%
    acsprocess::total_col_add(c("race_tot"= "gender", "gender_tot" = "age"), c("race_name", "gender")) %>%
    acsprocess::derive_pct_est_moe("pct_age",
                                   "gender_tot",
                                   "gender_tot_moe")

}


#' Process ancestry data
#'
#' Processes tidycensus downloaded ancestry data
#'
#' @param df tidycensus downloaded ancestry data
#' @param sub Whether to calculate percentages of country for a given continent
#'
#' @return Processed tidycensus ancestry data
#' @export
#'
#' @examples
process_ancestry <- function(df, sub = F){

  ancestry <- df %>%
    acsprocess::separate_label(c(NA, NA, "ancestry", "country"))

  if (sub){
    ancestry_combos <- ancestry %>%
      dplyr::filter(!is.na(country)) %>%
      dplyr::pull(ancestry) %>%
      unique()

    ancestry <- ancestry %>%
      dplyr::filter(ancestry %in% ancestry_combos) %>%
      acsprocess::total_col_add(list("tot_ancestry" = "country"), join_col = c("name", "ancestry")) %>%
      acsprocess::derive_pct_est_moe("pct_country",
                                     "tot_ancestry",
                                     "tot_ancestry_moe")

  }

  else{
    ancestry <- df %>%
      acsprocess::separate_label(c(NA, NA, "ancestry", "country")) %>%
      dplyr::filter(is.na(country)) %>%
      dplyr::select(-country) %>%
      acsprocess::total_col_add(total_cols = list("tot_pop" = "ancestry")) %>%
      acsprocess::derive_pct_est_moe("pct_ancestry",
                                     "tot_pop",
                                     "tot_pop_moe")
  }

  return(ancestry)

}

#' Process Asian ethnicity
#'
#' Processes tidycensus downloaded data on Asian ethnicity
#'
#' @param df tidycensus downloaded data on Asian by ethnicity
#'
#' @return processed data on Asian ethnicity
#' @export
#'
#' @examples
process_asian_disaggregated <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "ethnicity")) %>%
    acsprocess::total_col_add(list("tot_asian" = "ethnicity")) %>%
    acsprocess::derive_pct_est_moe("pct_asian",
                                   "tot_asian",
                                   "tot_asian_moe")
}

#' Process age/english language proficiency
#'
#' Processes data on age and English language proficiency
#'
#' @param df tidycensus downloaded ACS5 data on age and English-language proficiency
#'
#' @return processed data on age/English language proficiency
#' @export
#'
#' @examples
process_age_lang <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "age", "homelang")) %>%
    acsprocess::total_col_add(c("pop_tot" = "age", "age_tot" = "homelang"), c("name", "age")) %>%
    acsprocess::derive_pct_est_moe("pct_age",
                                   "age_tot",
                                   "age_tot_moe")
}


#' Process foreign born birth
#'
#' Processes tidycensus downloaded data on birth by country of origin
#'
#' @param df tidycensus downloaded data on country of origin
#'
#' @return processed tidycensus dataset with country of origin results by continent
#' @export
#'
#' @examples
process_foreign_born_birth <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "continent", "americas")) %>%
    dplyr::mutate(continent = ifelse(!is.na(americas), americas, continent)) %>%
    dplyr::select(-americas) %>%
    acsprocess::total_col_add(join_col = "name", total_cols = list("continent_overall" = "continent")) %>%
    dplyr::select(-c(variable, label, concept)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "continent_pct",
                                   aggregate_est = "continent_overall",
                                   aggregate_moe = "continent_overall_moe",
                                   component_est = "estimate",
                                   component_moe = "moe")
}

#' Process foreign born
#'
#' Processes tidycensus downloaded data on birthplace
#'
#' @param df tidycensus downloaded data on birthplace
#'
#' @return processed dataframe on birth by location
#' @export
#'
#' @examples
process_foreign_born <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "birthplace")) %>%
    acsprocess::total_col_add(join_col = "name", total_cols = list("birth_overall" = "birthplace")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "birth_pct",
                                   aggregate_est = "birth_overall",
                                   aggregate_moe = "birth_overall_moe",
                                   component_est = "estimate",
                                   component_moe = "moe")
}


#' Process foreign born by race
#'
#' @param df tidycensus downloaded dataset of birthplace by race
#'
#' @return Processed tidycensus dataframe of birthplace by race
#' @export
#'
#' @examples
process_race_foreign_born <-function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "birthplace")) %>%
    acsprocess::race_pull() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "birthplace")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                                   aggregate_est = "race_total",
                                   aggregate_moe = "race_total_moe")
}

#' Process language spoken at home
#'
#' Processes ACS5 tidycensus downloaded data on language spoken at home
#'
#' @param df ACS5 tidycensus downloaded data on language spoken at home
#'
#' @return Processed data on language spoken at home
#' @export
#'
#' @examples
process_home_lang <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "language")) %>%
    acsprocess::total_col_add(total_cols = list("pop_overall" = "language")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_language",
                                   aggregate_est = "pop_overall",
                                   aggregate_moe = "pop_overall_moe")
}


# internet access ----

#' Process Computer/Internet Data
#'
#' Processes tidycensus downloaded data on computer/internet usage
#'
#' @param df tidycensus downloaded data on computer/internet usage
#'
#' @return processed data on computer/internet usage
#' @export
#'
#' @examples
process_computer_overall <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "comp_access", "int_type")) %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(place_race = paste0(name, "_", race)) %>%
    dplyr::mutate(int_type = ifelse(grepl("No computer", comp_access, ignore.case = T), "No computer", int_type)) %>%
    acsprocess::total_col_add(c("tot_people" = "comp_access", "tot_comp" = "int_type"), join_col = c("place_race")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "percent_race",
                       aggregate_est = "tot_people",
                       aggregate_moe = "tot_people_moe")
}

#' Process Computer/Internet by Age
#'
#' Processes ACS5 tidycensus downloaded data on computer/internet usage by age
#'
#' @param df ACS5 tidycensus downloaded data on computer/internet usage by age
#'
#' @return Processed data on computer/internet usage by age
#' @export
#'
#' @examples
process_computer_age <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "age", "comp", "int")) %>%
    dplyr::mutate(int = ifelse(grepl("No", comp), comp, int)) %>%
    acsprocess::total_col_add(c("pop_tot" = "age", "age_tot" = "comp", "comp_tot" = "int"),
                              c("name", "age", "comp")) %>%
    acsprocess::comp_recode(comp_col = comp, int) %>%
    acsprocess::derive_pct_est_moe("pct_int",
                                   "age_tot",
                                   "age_tot_moe")
}


#' Process internet/race data
#'
#' Processes ACS5 tidycensus downloaded data on internet/race, in preparation for other processing
#'
#' @param df ACS5 tidycensus downloaded data on internet usage by race
#'
#' @return Base-level processed data on internet usage by race
#' @export
#'
#' @examples
process_race_internet_base <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "computer_access", "internet_access")) %>%
    acsprocess::race_pull()
}

#' Process Computer Usage by Race
#'
#' Processes ACS5 tidycensus downloaded data on computer/internet usage by race
#'
#' @param df ACS5 tidycensus downloaded data on computer/internet usage by race
#'
#' @return processed data on computer/internet usage by race
#' @export
#'
#' @examples
process_race_computer <- function(df){
  df %>%
    acsprocess::process_race_internet_base() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    dplyr::filter(!is.na(computer_access) & is.na(internet_access)) %>%
    dplyr::select(-c(variable, label, internet_access, concept)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race_computer",
                                   aggregate_est = "race_total",
                                   aggregate_moe = "race_total_moe")
}

#' Process race/internet data
#'
#' Processes ACS5 tidycensus downloaded data on race/internet usage
#'
#' @param df ACS5 tidycensus downloaded data on race/internet usage
#'
#' @return Processed data on race/internet usage
#' @export
#'
#' @examples
process_race_internet_complete <- function(df){
  df %>%
    acsprocess::process_race_internet_base() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    dplyr::mutate(internet_access = ifelse(computer_access == "No Computer", computer_access, internet_access)) %>%
    dplyr::filter(!is.na(internet_access)) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_internet",
                                   aggregate_est = "race_total",
                                   aggregate_moe = "race_total_moe")
}


# education ----

#' Process education data
#'
#' Processes ACS5 tidycensus downloaded data on educational attainment
#'
#' @param df ACS5 tidycensus downloaded data on educational attainment
#'
#' @return Processed data on educational attainment
#' @export
#'
#' @examples
process_educ_overall <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "education")) %>%
    acsprocess::total_col_add(total_cols = list("population" = "education")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "percent_pop",
                       aggregate_est = "population",
                       "population_moe")
}

#' Process educational attainment by race
#'
#' Processes ACS5 tidycensus downloaded data on educational attainment by race
#'
#' @param df ACS5 tidycensus downloaded data on educational attainment by race
#'
#' @return Processed data on educational attainment by race
#' @export
#'
#' @examples
educ_race_process <- function(df){

  # education/race overall
  educ_race_df_overall <- df %>%
    acsprocess::process_educ_race() %>%
    dplyr::filter(!is.na(education) | is.na(education) & is.na(gender)) %>%
    acsprocess::total_col_add(list("race_total" = "gender"), join_col = c("name", "race")) %>%
    acsprocess::est_moe_derive(c("name", "race", "education")) %>%
    dplyr::select(-c(variable, gender, label, concept, estimate, moe)) %>%
    dplyr::distinct()

  educ_race_df_overall <- educ_race_df_overall %>%
    acsprocess::derive_pct_est_moe(proportion_col = "race_education_pct",
                                   aggregate_est = "race_total",
                                   aggregate_moe = "race_total_moe",
                                   component_est = "name_race_education_est",
                                   component_moe = "name_race_education_moe")

  educ_race_df_overall

}

#' Process educational attainment by race
#'
#' ACS5 tidycensus downloaded data on educational attainment by race
#'
#' @param df ACS5 tidycensus downloaded data on educational attainment by race
#'
#' @return Processed data on educational attainment by race
#' @export
#'
#' @examples
process_educ_race <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "gender", "education")) %>%
    acsprocess::race_pull() %>%
    dplyr::filter(race != "white alone")
}

#' Process education/race/sex data
#'
#' Processes tidycensus downloaded data on educational attainment by race and sex
#'
#' @param df ACS5 tidycensus downloaded data on educational attainment by race and sex
#'
#' @return Processed data on educational attainment by race and sex
#' @export
#'
#' @examples
process_educ_race_gender <- function(df){
  df %>%
    acsprocess::process_educ_race() %>%
    dplyr::filter(!is.na(gender)) %>%
    dplyr::mutate(race_place = paste0(name, "_", race)) %>%
    acsprocess::total_col_add(list("race_gender_total" = "education"), join_col = c("race_place", "gender")) %>%
    dplyr::filter(!is.na(education)) %>%
    est_moe_derive(c("name", "race", "education", "gender")) %>%
    dplyr::select(-c(variable, label, concept, estimate, moe)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "race_education_gender_pct",
                                   aggregate_est = "race_gender_total",
                                   aggregate_moe = "race_gender_total_moe",
                                   component_est = "name_race_education_gender_est",
                                   component_moe = "name_race_education_gender_moe")
}



# income ----

#' Process race/household income
#'
#' Processes ACS5 tidycensus downloaded data on race and household income
#'
#' @param df ACS5 tidycensus downloaded data on race and household income
#'
#' @return Processed data on race/household income
#' @export
#'
#' @examples
process_income_race <- function(df){

  df %>%
    acsprocess::race_pull() %>%
    acsprocess::separate_label(c(NA, NA, "income_range")) %>%
    dplyr::filter(race != "white alone") %>%
    acsprocess::total_col_add(total_cols = list("race_households" = "income_range"), join_col = c("name", "race")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "percent_race_households",
                                   aggregate_est = "race_households",
                                   aggregate_moe = "race_households_moe")
}

#' Process median income by race
#'
#' Processes ACS5 tidycensus downloaded data on household median income by race
#'
#' @param df ACS5 tidycensus downloaded data on household median income by race
#'
#' @return Processed data on median income by race
#' @export
#'
#' @examples
process_income_race_median <- function(df) {
  df %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(race = ifelse(grepl("in 2019 inflation", race), NA, race)) %>%
    acsprocess::separate_label(c(NA, "Median household income")) %>%
    acsprocess::total_col_add(total_cols = list("overall_median_income" = "race")) %>%
    dplyr::filter(race != "white alone") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "ratio_median_income",
                                   aggregate_est = "overall_median_income",
                                   aggregate_moe = "overall_median_income_moe",
                                   type_moe = "ratio")
}

#' Process income data
#'
#' Processes ACS5 tidycensus downloaded data on income in last 12 months
#'
#' @param df ACS5 tidycensus downloaded data on income in last 12 months
#'
#' @return Processed data on income in last 12 months
#' @export
#'
#' @examples
process_income_last12 <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "income_range")) %>%
    acsprocess::total_col_add(total_cols = list("pop_overall" = "income_range")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_income_range",
                                   aggregate_est = "pop_overall",
                                   aggregate_moe = "pop_overall_moe") %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(pct_cumul = cumsum(pct_income_range))
}


#' Process age and income data
#'
#' Processes ACS5 tidycensus downloaded data on age and income in last 12 months
#'
#' @param df ACS5 tidycensus downloaded data on age and income in last 12 months
#'
#' @return Lightly processed data on age and income in last 12 months
#' @export
#'
#' @examples
process_age_income <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "age", "income")) %>%
    acsprocess::total_col_add(list("households" = "age", "agehous"= "income"), join_col = c("name", "age")) %>%
    dplyr::mutate(age_new = gsub("Householder ", "", age)) %>%
    dplyr::mutate(pct_agehous = pct_round(estimate, agehous)) %>%
    acsprocess::derive_pct_est_moe("pct_agehous", "agehous", "agehous_moe") %>%
    dplyr::group_by(name, age) %>%
    acsprocess::income_recode(income_col = income) %>%
    dplyr::arrange(income_recode) %>%
    dplyr::mutate(cuml_agehous = cumsum(estimate),
           cuml_agehous_pct = round(cuml_agehous * 100 / agehous, 2)) %>%
    dplyr::ungroup() %>%
    acsprocess::age_recode(age_new)
}


# employment ----

#' Process Employment/Age/Gender Data
#'
#' Processes ACS5 tidycensus downloaded data on employment by age and gender
#'
#' @param df ACS5 tidycensus downloaded data on employment by age and gender
#'
#' @return Processed data on employment by age and gender
#' @export
#'
#' @examples
process_employment_age_gender <- function(df){
  df %>%
    acsprocess::process_employment_age_gender_base() %>%
    dplyr::filter(grepl("In labor force", labor_force_status)) %>%
    dplyr::mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force)) %>%
    acsprocess::total_col_add(list("total_workers" = "type_labor_force"), join_col = c("name", "gender", "age_range")) %>%
    dplyr::filter(!(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "gender", "age_range", "type_labor_force", "employment_status"),
                   name_col = "workers") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_gender_age",
                       aggregate_est = "total_workers",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    dplyr::select(-c(variable, estimate, moe, label, concept, race)) %>%
    dplyr::distinct()
}

#' Process Race/Employment/Gender under 65
#'
#' Processes ACS5 tidycensus downloaded data on employment by race and gender for people under 65
#'
#' @param df ACS5 tidycensus downloaded data on employment by race and gender for people under 65
#'
#' @return Processed data on employment by race and gender for people under 65
#' @export
#'
#' @examples
process_race_employ_gender_u65 <- function(df) {
  df %>%
    acsprocess::process_race_employment_age_gender_base() %>%
    dplyr::filter(grepl("C23002", variable)) %>%
    dplyr::filter(!grepl("sex by age", race) &
                    grepl("In labor force", labor_force_status) &
                    grepl("16 to 64", age_range) &
                    !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    dplyr::mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status)) %>%
    acsprocess::total_col_add(list("total_labor_force" = "type_labor_force"), c("name", "gender", "race")) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                                   aggregate_est = "total_labor_force",
                                   aggregate_moe = "total_labor_force_moe",
                                   component_est = "workers_est",
                                   component_moe = "workers_moe") %>%
    dplyr::select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    dplyr::distinct()
}


#' Base-processing of race/employment under 65 data
#'
#' Performs base-processing of race/employment under 65 data
#'
#' @param df ACS5 tidycensus downloaded data on employment by race and gender for people under 65
#' @param tot_df Totals dataset
#'
#' @return Base-processed data on employment by race and gender under 65
#' @export
#'
#' @examples
process_race_employment_u65_base <- function(df, tot_df) {

  df %>%
    dplyr::left_join(tot_df) %>%
    dplyr::filter(!is.na(type_labor_force)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "race", "type_labor_force", "employment_status"),
                               name_col = "workers") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                                   aggregate_est = "total_workers_est",
                                   aggregate_moe = "total_workers_moe",
                                   component_est = "workers_est",
                                   component_moe = "workers_moe") %>%
    dplyr::select(-c(variable, gender, age_range, estimate, moe, label, concept)) %>%
    dplyr::distinct()
}

#' Process race/employment data under 65
#'
#' Processes ACS5 tidycensus downloaded data on race/employment for people under 65
#'
#' @param df ACS5 tidycensus downloaded data on emloyment by race for people under 65
#'
#' @return Processed data on employment by race for people under 65
#' @export
#'
#' @examples
process_race_employment_u65 <- function(df){
  race_employment_u65_preprocess <- df %>%
    acsprocess::process_race_employment_age_gender_base() %>%
    dplyr::filter(grepl("C23002", variable)) %>%
    dplyr::filter(!grepl("sex by age", race) &
                    grepl("In labor force", labor_force_status) &
                    grepl("16 to 64", age_range) &
                    !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    dplyr::mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status))

  total_df <- race_employment_u65_preprocess %>%
    dplyr::filter(is.na(type_labor_force)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "race"), name_col = "total_workers") %>%
    dplyr::select(name, race, total_workers_est, total_workers_moe) %>%
    dplyr::distinct()

  race_employment_u65 <- race_employment_u65_preprocess %>%
    acsprocess::process_race_employment_u65_base(tot_df = total_df)

  race_employment_u65
}

#' Base-processing employment by race, age, and gender
#'
#' Base processing of ACS5 tidycensus downloaded data on employment by race, age, and gender
#'
#' @param df ACS5 tidycensus downloaded data on employment by race, age, and gender
#'
#' @return Processed data on employment by race, age, and gender
#' @export
#'
#' @examples
process_race_employment_age_gender_base <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "gender", "age_range", "labor_force_status", "type_labor_force", "employment_status")) %>%
    acsprocess::race_pull()
}

#' Process employment by race and gender
#'
#' Process ACS5 tidycensus downloaded data on employment by race and gender
#'
#' @param df ACS5 tidycensus downloaded data on employment by race and gender
#'
#' @return Processed data on employment by race and gender
#' @export
#'
#' @examples
process_race_gender_employment_general <- function(df){
  process <- df %>%
    acsprocess::process_race_employment_age_gender_base() %>%
    dplyr::filter(grepl("C23002", variable)) %>%
    dplyr::filter(!grepl("sex by age", race) & grepl("In labor force", labor_force_status) & !is.na(type_labor_force)) %>%
    dplyr::mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
                  # employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
                  type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force))

  total_df <- process %>%
    dplyr::filter(is.na(employment_status) | grepl("65 years", age_range)) %>%
    acsprocess::est_moe_derive(group_cols = c("name","race", "gender"), name_col = "race_gender_laborforce") %>%
    dplyr::select(name, race, gender, race_gender_laborforce_est, race_gender_laborforce_moe) %>%
    dplyr::distinct()

  process %>%
    dplyr::left_join(total_df) %>%
    # calculate total across age groups in labor force
    dplyr::filter(!is.na(employment_status) | grepl("Armed Forces", type_labor_force)) %>%
    dplyr::mutate(employment_status= ifelse(is.na(employment_status), type_labor_force, employment_status)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                                   aggregate_est = "race_gender_laborforce_est",
                                   aggregate_moe = "race_gender_laborforce_moe",
                                   component_est = "workers_est",
                                   component_moe = "workers_moe") %>%
    dplyr::select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    dplyr::distinct()
}

#' Base-process employment/age/gender
#'
#' Base-processing of ACS5 tidycensus downloaded data on employment/age/gender
#'
#' @param df ACS5 tidycensus downloaded data on employment/age/gender
#'
#' @return Base-processed ACS5 tidycensus downloaded data on employment/age/gender
#' @export
#'
#' @examples
process_employment_age_gender_base <- function(df){
  df %>%
    acsprocess::process_race_employment_age_gender_base() %>%
    dplyr::filter(!grepl("C23002", variable))
}


#' Process employment/age data
#'
#' Processes ACS5 tidycensus downloaded data on employment by age
#'
#' @param df ACS5 tidycensus downloaded data on employment by age
#'
#' @return Processed data on employment by age
#' @export
#'
#' @examples
process_employment_age <- function(df){
  employment_age_preprocess <- df %>%
    acsprocess::process_employment_age_gender_base() %>%
    dplyr::filter(grepl("In labor force", labor_force_status)) %>%
    dplyr::mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
                  employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
                  type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force))

  total_df <- employment_age_preprocess %>%
    dplyr::filter(is.na(type_labor_force)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "age_range"), name_col = "total_workers") %>%
    dplyr::select(name, age_range, total_workers_est, total_workers_moe) %>%
    dplyr::distinct()

  employment_age <- employment_age_preprocess %>%
    dplyr::filter(!is.na(type_labor_force) & !is.na(employment_status)) %>%
    dplyr::left_join(total_df) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "age_range", "type_labor_force", "employment_status"),
                               name_col = "workers") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_age",
                                   aggregate_est = "total_workers_est",
                                   aggregate_moe = "total_workers_moe",
                                   component_est = "workers_est",
                                   component_moe = "workers_moe") %>%
    dplyr::select(-c(variable, estimate, moe, label, concept, race, gender)) %>%
    dplyr::distinct()

  employment_age
}

#' Process labor force by race and gender for people under 65
#'
#' @param df ACS5 tidycensus downloaded data on labor force participation by race and gender for people under 65
#'
#' @return Processed data on employment by race and gender for people under 65
#' @export
#'
#' @examples
process_race_laborforce_gender_u65 <- function(df){
  race_laborforce_gender_u65 <- df %>%
    acsprocess::process_race_employment_age_gender_base() %>%
    dplyr::filter(grepl("C23002", variable)) %>%
    dplyr::filter(grepl("16 to 64", age_range) & is.na(type_labor_force)) %>%
    acsprocess::total_col_add(list("total_persons" = "labor_force_status"), c("name", "race", "gender")) %>%
    acsprocess::est_moe_derive(c("name", "race", "gender", "labor_force_status"), name_col = "persons") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race_gender",
                                   aggregate_est = "total_persons",
                                   aggregate_moe = "total_persons_moe",
                                   component_est = "persons_est",
                                   component_moe = "persons_moe") %>%
    dplyr::select(-c(variable, estimate, moe, label, concept, age_range, type_labor_force, employment_status)) %>%
    dplyr::distinct()

}


# tenure/housing ----

#' Process tenure by race
#'
#' Processes ACS5 tidycensus downloaded data on tenure by race
#'
#' @param df ACS5 tidycensus downloaded data on tenure by race
#'
#' @return Processed data on tenure by race
#' @export
#'
#' @examples
process_tenure_race <- function(df){

  df %>%
    acsprocess::process_race_home_own() %>%
    dplyr::filter(!is.na(tenure)) %>%
    acsprocess::total_col_add(join_col = c("name", "tenure"), total_cols = list("tenure_overall" = "race")) %>%
    dplyr::select(-c(variable, concept)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "race_pct_tenure",
                       aggregate_est = "tenure_overall",
                       aggregate_moe = "tenure_overall_moe") %>%
    dplyr::arrange(tenure, race)
}


#' Process cost-burden for homeowners
#'
#' Processes ACS5 tidycensus downloaded data on cost burden by owners
#'
#' @param df ACS5 tidycensus downloaded data on cost burden by owners
#'
#' @return Processed data on cost burden for owners
#' @export
#'
#' @examples
process_burden_owners <- function(df){

  burden_owners_overall <- df %>%
    acsprocess::separate_label(c(NA, NA, "mortgage_status", "percent_household_income")) %>%
    dplyr::filter(!(!is.na(mortgage_status) & is.na(percent_household_income))) %>%
    acsprocess::total_col_add(total_cols = list("total_homeowners" = "mortgage_status"), join_col = c("name")) %>%
    dplyr::filter(!is.na(percent_household_income)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "percent_household_income")) %>%
    dplyr::select(-c(concept, mortgage_status, variable, label, estimate, moe)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_homeowners",
                       aggregate_est = "total_homeowners",
                       aggregate_moe = "total_homeowners_moe",
                       component_est = "name_percent_household_income_est",
                       component_moe = "name_percent_household_income_moe") %>%
    dplyr::mutate(cumulpct = cumsum(pct_homeowners))

  return(burden_owners_overall)
}

#' Process cost-burden for owners by mortgage status
#'
#' Processes ACS5 tidycensus downloaded data on cost-burden for owners by mortgage status
#'
#' @param df ACS5 tidycensus downloaded data on cost-burden for owners by mortgage status
#'
#' @return Processed data on cost-burden for owners by mortgage status
#' @export
#'
#' @examples
process_burden_owners_mortgage <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "mortgage_status", "percent_household_income")) %>%
    dplyr::filter(!is.na(mortgage_status)) %>%
    acsprocess::total_col_add(total_cols = list("total_homeowners_mortgage" = "percent_household_income"), join_col = c("name", "mortgage_status")) %>%
    dplyr::select(-c(concept, variable, label)) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_homeowners_mortgage",
                       aggregate_est = "total_homeowners_mortgage",
                       aggregate_moe = "total_homeowners_mortgage_moe")
}

#' Process cost burden by household income for owners
#'
#' Processes ACS5 tidycensus downloaded data on cost-burden for owners by household income
#'
#' @param df ACS5 tidycensus downloaded data on cost-burden for owners by household income
#'
#' @return Processed cost-burden by household income data for owners
#' @export
#'
#' @examples
process_household_income_ownercosts <- function(df) {
  df %>%
    acsprocess::total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    dplyr::filter(housing_costs != "Not computed") %>%
    acsprocess::est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")

}

#' Process cost-burden by household income for renters
#'
#' Processes ACS5 tidycensus downloaded data on cost-burden by household income for renters
#'
#' @param df ACS5 tidycensus downloaded data on cost-burden by household income for renters
#'
#' @return Processed data on cost burden by household income for renters
#' @export
#'
#' @examples
process_household_income_rentercosts <- function(df){
  df %>%
    acsprocess::total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    dplyr::filter(housing_costs != "Not computed") %>%
    acsprocess::est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")
}

#' Process tenure by age
#'
#' Processes ACS5 tidycensus downloaded data on tenure by age
#'
#' @param df ACS5 tidycensus downloaded data on tenure by age
#'
#' @return Processed data on tenure by age
#' @export
#'
#' @examples
process_tenure_age <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "tenure", "age")) %>%
    acsprocess::total_col_add(c("pop_tot" = "tenure", "tenure_tot" = "age"),
                              c("name", "tenure")) %>%
    acsprocess::derive_pct_est_moe("pct_age",
                                   "tenure_tot",
                                   "tenure_tot_moe") %>%
    dplyr::rename(pct_age_moe = pct_moe) %>%
    # calculate total for location/age group
    acsprocess::est_moe_derive(c("name", "age"), name_col = "locage") %>%
    dplyr::mutate(pct_ageten = pct_round(estimate, locage_est)) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_ageten", aggregate_est = "locage_est", "locage_moe")
}

#' Base-process home-ownership by race
#'
#' ACS5 tidycensus downloaded data on home-ownership by race
#'
#' @param df ACS5 tidycensus downloaded data on home-ownership by race
#'
#' @return Processed data on home-ownership by race
#' @export
#'
#' @examples
process_race_home_own <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "tenure")) %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(race = ifelse(race == "tenure", NA, race))
}

#' Process home-ownership by race and tenure
#'
#' Processes ACS5 tidycensus downloaded data on tenure and race
#'
#' @param df ACS5 tidycensus downloaded data on tenure by race
#'
#' @return Processed data on tenure by race
#' @export
#'
#' @examples
process_race_home_own_race_tenure <- function(df){
  df %>%
    acsprocess::process_race_home_own() %>%
    dplyr::filter(!is.na(race)) %>%
    acsprocess::total_col_add(join_col = c("name", "race"),
                              total_cols = list("race_overall" = "tenure")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "race_pct",
                                   aggregate_est = "race_overall",
                                   aggregate_moe = "race_overall_moe",
                                   component_est = "estimate", component_moe = "moe")
}

#' Process tenure
#'
#' Processes ACS5 tidycensus downloaded data on tenure
#'
#' @param df ACS5 tidycensus downloaded data on tenure
#'
#' @return Processed data on tenure
#' @export
#'
#' @examples
process_tenure_df <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "tenure")) %>%
    acsprocess::total_col_add(total_cols = list("tenure_overall" = "tenure")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_tenure",
                                   aggregate_est = "tenure_overall",
                                   aggregate_moe = "tenure_overall_moe")
}

#' Process median income by tenure
#'
#' Processes ACS5 tidycensus downloaded data on median income by tenure
#'
#' @param df ACS5 tidycensus downloaded data on median income by tenure
#'
#' @return Processed data on median income by tenure
#' @export
#'
#' @examples
process_tenure_income_median <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, NA, "tenure")) %>%
    acsprocess::total_col_add(total_cols = list("median_income_overall" = "tenure")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "ratio_income",
                                   aggregate_est = "median_income_overall",
                                   aggregate_moe = "median_income_overall_moe",
                                   type_moe = "ratio") %>%
    dplyr::select(-c(label, concept, variable))
}


#' Process cost-burden for renters
#'
#' Processes ACS5 tidycensus downloaded data on cost-burden for renters
#'
#' @param df ACS5 tidycensus downloaded data on cost-burden for renters
#'
#' @return Processed data on cost-burden for renters
#' @export
#'
#' @examples
process_burden_renters <- function(df) {
  df %>%
    acsprocess::separate_label(c(NA, NA, "percent_income")) %>%
    acsprocess::total_col_add(list("total_renters"="percent_income"))  %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_renters",
                                   aggregate_est = "total_renters",
                                   aggregate_moe = "total_renters_moe") %>%
    dplyr::mutate(cumul_pct = cumsum(pct_renters))
}

#' Process income for owners
#'
#' Processes ACS5 tidycensus downloaded data on income for owners
#'
#' @param df ACS5 tidycensus downloaded data on income for owners
#'
#' @return Processed data on income for owners
#' @export
#'
#' @examples
process_owner_income_num <- function(df){
  df %>%
    dplyr::filter(is.na(housing_costs)) %>%
    acsprocess::total_col_add(list("num_homeowners" = "household_income")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_homeowners",
                                   "num_homeowners",
                                   "num_homeowners_moe")

}

#' Process income for renters
#'
#' Processes ACS5 tidycensus downloaded data on income for renters
#'
#' @param df ACS5 tidycensus downloaded data on income for renters
#'
#' @return Processed data on income for renters
#' @export
#'
#' @examples
process_renter_income_num <- function(df){
  df %>%
    dplyr::filter(is.na(housing_costs)) %>%
    acsprocess::total_col_add(list("num_renters" = "household_income")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_renters",
                                   "num_renters",
                                   "num_renters_moe")

}

#' Process owner cost-burden by age
#'
#' Processes ACS5 tidycensus downloaded data on owner cost-burden by age
#'
#' @param df ACS5 tidycensus downloaded data on owner cost-burden by age
#'
#' @return Processed data on owner cost-burden by age
#' @export
#'
#' @examples
process_own_costburden_age <- function(df){
  df %>%
    separate_label(names_vector = c(NA, NA, "age", "cost_burden")) %>%
    acsprocess::total_col_add(total_cols = c("households" = "age", "hous_age" = "cost_burden"), join_col = c("name", "age")) %>%
    dplyr::mutate(tenure = "Owner")
}


#' Process renter cost-burden by age
#'
#' Processes ACS5 tidycensus downloaded data on renter cost-burden by age
#'
#' @param df ACS5 tidycensus downloaded data on renter cost-burden by age
#'
#' @return Processed data on renter cost-burden by age
#' @export
#'
#' @examples
process_rent_costburden_age <- function(df){

  df %>%
    acsprocess::separate_label(names_vector = c(NA, NA, "age", "cost_burden")) %>%
    acsprocess::total_col_add(total_cols = c("households" = "age", "hous_age" = "cost_burden"), join_col = c("name", "age")) %>%
    dplyr::mutate(tenure = "Renter")

}

#' Process owner and renter cost-burden by age
#'
#' Processes ACS5 tidycensus downloaded datasets on renter cost-burden by age, and owner cost-burden by age, and binds them together
#'
#' @param rent_df ACS5 tidycensus downloaded data on renter cost-burden by age
#' @param own_df ACS5 tidycensus downloaded data on owner cost-burden by age
#'
#' @return Lightly processed data on renter/owner cost burden by age
#' @export
#'
#' @examples
process_rent_own_costburden_age <- function(rent_df, own_df){

  rent <- rent_df %>%
    acsprocess::process_rent_costburden_age()

  own <- own_df %>%
    acsprocess::process_own_costburden_age()

  join <- rbind(rent, own) %>%
    acsprocess::est_moe_derive(group_cols = c("name"), name_col = "tot_households")

}


# health insurance ----

#' Process health insurance by race
#'
#' Processes ACS5 tidycensus downloaded data on health insurance by race
#'
#' @param df ACS5 tidycensus downloaded data on health insurance by race
#'
#' @return Processed data on health insurance by race
#' @export
#'
#' @examples
process_health_race <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "age", "health")) %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(name_race = paste0(name, "_", race)) %>%
    acsprocess::total_col_add(total_cols = list("race_total" = "age", "race_age_total" = "health"), join_col = c("name_race", "age")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_age",
                       aggregate_est = "race_age_total",
                       aggregate_moe = "race_age_total_moe")


}

#' Process health-insurance by age
#'
#' Processes ACS5 tidycensus downloaded data on health insurance by age
#'
#' @param df ACS5 tidycensus downloaded data on health insurance by age
#'
#' @return Processed health-insurance by age data
#' @export
#'
#' @examples
process_health_age <- function(df) {
  # browser()
  df <- df %>%
    acsprocess::separate_label(c(NA, NA, "age", "num_plans", "type_coverage"))

  # correct for no insurance
  df_none <- df %>%
    dplyr::filter(grepl("No health insurance", num_plans)) %>%
    dplyr::distinct() %>%
    # correct for msissing
    dplyr::mutate(type_coverage = ifelse(grepl("No health insurance", num_plans), "No insurance", type_coverage))


  df %>%
    rbind(df_none) %>%
    acsprocess::total_col_add(total_cols = c("total" = "age", "age_total" = "num_plans", "age_num_plans_total" = "type_coverage"), join_col = c("name", "age", "num_plans")) %>%
    acsprocess::derive_pct_est_moe("pct_hc",
                                   "age_num_plans_total",
                                   "age_num_plans_total_moe")
}


# disability ----

#' Pull type of disability
#'
#' Creates column on disability by type from ACS5 tidycensus downloaded data on disability by type
#'
#' @param df ACS5 tidycensus downloaded data on disability by type
#' @param new_col Name of new column that will store disability-type, data-masked; default disability
#' @param concept_col Data-masked name of concept column; default concept
#'
#' @return ACS5 tidycensus downloaded data on disability by type with disability column
#' @export
#'
#' @examples
disab_pull <- function(df, new_col = disability, concept_col = concept){
  df %>%
    dplyr::mutate({{new_col}} := {{concept_col}} %>%
                    tolower %>%
                    gsub("sex by age by ", replacement = "", x = .) %>%
                    gsub(" difficulty", "", .) %>%
                    stringr::str_to_title(.))
}


#' Process disability overall
#'
#' Processes ACS5 tidycensus downloaded data on disability overall
#'
#' @param df ACS5 tidycensus downloaded data on disability overall with disability column
#'
#' @return Processed data on disability overall with disability column
#' @export
#'
#' @examples
process_disability_overall <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "sex", "age", "disability")) %>%
    acsprocess::total_col_add(c("tot_pop" = "sex",
                    "tot_sex" = "age",
                    "tot_age" = "disability"),
                  c("name",
                    "sex",
                    "age")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_disability",
                       aggregate_est = "tot_age",
                       aggregate_moe = "tot_age_moe")
}

#' Processes disability by type
#'
#' Processes ACS5 tidycensus downloaded data on disability by type
#'
#' @param df ACS5 tidycensus downloaded data on disability by type
#' @param pop_tot_df Dataframe with population totals
#'
#' @return Processed data on disability by type
#' @export
#'
#' @examples
process_read_disab_type <- function(df, pop_tot_df){
  df %>%
    acsprocess::disab_pull(disability, concept) %>%
    dplyr::mutate(name_disab = paste0(name, "_", disability)) %>%
    acsprocess::separate_label(c(NA, NA, "sex", "age", "disab_type")) %>%
    acsprocess::total_col_add(total_cols =
                                c("pop_tot" = "sex",
                                  "sex_tot" = "age",
                                  "age_tot" = "disab_type"),
                              join_col = c("name_disab", "sex", "age")) %>%
    dplyr::mutate(disab_pop = ifelse(grepl("With a",
                                           disab_type),
                                     "Has a disability",
                                     "No disability")) %>%
    # drop population total and join population total data because some disability categories dont include all pop
    dplyr::select(-c(pop_tot, pop_tot_moe)) %>%
    dplyr::left_join(pop_tot_df)
}

#' Process disability as percent of population
#'
#' Processes ACS5 tidycensus downloaded data on disability, calculating percentages of place's population
#'
#' @param df_disability ACS5 tidycensus downloaded data on percentages of people with a disability
#'
#' @return
#' @export
#'
#' @examples
process_disability_pop <- function (df_disability){
  df_disability %>%
    acsprocess::est_moe_derive(c("name", "disability")) %>%
    dplyr::filter(grepl("With a disability", disability)) %>%
    dplyr::select(geoid, name, name_disability_est, name_disability_moe) %>%
    dplyr::distinct()
}


#' Process disability by age
#'
#' Processes ACS5 tidycensus downloaded data on rates of disability by age
#'
#' @param df_disability ACS5 tidycensus downloaded data on disability by age
#'
#' @return Processed data on disability by age
#' @export
#'
#' @examples
process_disability_age <- function (df_disability){
  df_disability %>%
    acsprocess::est_moe_derive(c("name", "age", "disability")) %>%
    dplyr::filter(grepl("With a disability", disability)) %>%
    dplyr::select(geoid, name, age, name_age_disability_est, name_age_disability_moe) %>%
    dplyr::distinct()
}


#' Process disability by type
#'
#' Processes ACS5 tidycensus downloaded data on disability by type
#'
#' @param df ACS5 tidycensus downloaded data on disability by type
#' @param df_disab_pop Data on disability as percent of population
#'
#' @return
#' @export
#'
#' @examples
process_disab_type <- function(df, df_disab_pop){
  df %>%
    dplyr::select(-c(sex, age, variable, label)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "disab_type"),
                               est_col = "estimate",
                               moe_col = "moe") %>%
    dplyr::select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    distinct() %>%
    dplyr::left_join(df_disab_pop) %>%
    dplyr::filter(disab_pop == "Has a disability") %>%
    acsprocess::derive_pct_est_moe("name_disab_type_pct_pop",
                                   "pop_total",
                                   "pop_total_moe",
                                   "name_disab_type_est",
                                   "name_disab_type_moe") %>%
    dplyr::rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("name_disab_type_pct_disab",
                       "name_disability_est",
                       "name_disability_moe",
                       "name_disab_type_est",
                       "name_disab_type_moe") %>%
    dplyr::rename(pct_moe_disab = pct_moe)
}


#' Process disability by sex
#'
#' Processes ACS5 tidycensus downloaded data on rates of disability by sex
#'
#' @param df ACS5 tidycensus downloaded data on disability by sex
#'
#' @return Processed disability by sex data
#' @export
#'
#' @examples
process_sex_disab_overall <- function(df){
  df %>%
    acsprocess::est_moe_derive(c("name", "sex", "disability")) %>%
    dplyr::filter(grepl("With a disability", disability)) %>%
    dplyr::select(geoid, name, sex, tot_sex, tot_sex_moe, name_sex_disability_est, name_sex_disability_moe) %>%
    dplyr::distinct()
}


#' Process disability by sex
#'
#' Processes ACS5 tidycensus downloaded data on disability by sex
#'
#' @param df ACS5 tidycensus downloaded data on disability
#' @param df_disab_sex  ACS5 tidycensus downloaded data on disability by sex
#'
#' @return Processed disability by sex data
#' @export
#'
#' @examples
process_disab_sex <- function(df, df_disab_sex){
  # browser()

  df %>%
    acsprocess::est_moe_derive(c("name", "sex", "disability", "disab_type"),
                               name_col = "sex_disability_type") %>%
    dplyr::filter(grepl("With a", disab_type)) %>%
    dplyr::select(geoid, name, sex, disability, pop_total, pop_total_moe, sex_disability_type_est, sex_disability_type_moe) %>%
    dplyr::distinct() %>%
    dplyr::left_join(df_disab_sex) %>%
    acsprocess::derive_pct_est_moe("sex_disability_type_pct_disability",
                                   "name_sex_disability_est",
                                   "name_sex_disability_est",
                                   "sex_disability_type_est",
                                   "sex_disability_type_moe") %>%
    dplyr::rename(pct_moe_disab = pct_moe) %>%
    acsprocess::derive_pct_est_moe("sex_disability_type_pct_pop",
                                   "pop_total",
                                   "pop_total_moe",
                                   "sex_disability_type_est",
                                   "sex_disability_type_moe") %>%
    dplyr::rename(pct_moe_pop = pct_moe) %>%
    acsprocess::derive_pct_est_moe("sex_disability_type_pct_sex",
                                   "tot_sex",
                                   "tot_sex_moe",
                                   "sex_disability_type_est",
                                   "sex_disability_type_moe") %>%
    dplyr::rename(pct_moe_sex = pct_moe)
}


#' Process foodstamp and disability status data
#'
#' Processes tidycensus-downloaded data from table B22010 on household food-stamp receipt by disability status. Option to calculate overall-results for household disability status, since other Census tables on disability are not available at the block-group level
#'
#' @param df tidycensus-downloaded data on household food-stamp receipt by disability status, census table B22010
#' @param overall Whether to aggregate dataframe to households by presence of person with a disability; default F
#'
#' @return Processed dataframe on household food-stamp receipt by disability status, or households by presence of person with a disability
#' @export
#'
#' @examples
process_disab_foodstamp <- function(df, overall = F){

  disabdf <- df %>%
    acsprocess::separate_label(names_vector = c(NA, NA, "foodstamp", "disab")) %>%
    acsprocess::total_col_add(total_cols = c("tothous" = "foodstamp", "totfood" = "disab"), join_col = c("name", "foodstamp"))

  if (overall){
    disabdf <- disabdf %>%
      acsprocess::est_moe_derive(group_cols = c("name", "disab")) %>%
      dplyr::select(geoid, name, tothous, tothous_moe, disab, name_disab_est, name_disab_moe) %>%
      dplyr::rename(estimate = name_disab_est,
                    moe = name_disab_moe) %>%
      acsprocess::derive_pct_est_moe(
        proportion_col = "pct_disab",
        aggregate_est = "tothous",
        aggregate_moe = "tothous_moe"
      )
  }

  else {
    disabdf <- disabdf %>%
      acsprocess::derive_pct_est_moe(proportion_col = "pct_disabfood", aggregate_est = "totfood", aggregate_moe = "totfood_moe")

  }

  disabdf

}

#' Process disability by age
#'
#' Processes ACS5 tidycensus downloaded data on disability by age
#'
#' @param df ACS5 tidycensus downloaded data on disability by age
#' @param disab_age_df Disability by age data
#'
#' @return Processed disability by age data
#' @export
#'
#' @examples
process_disab_age <- function(df, disab_age_df) {
  df %>%
    dplyr::select(-c(sex, variable, label)) %>%
    dplyr::left_join(disab_age_df) %>%
    acsprocess::est_moe_derive(group_cols = c("name_disab", "age"),
                               est_col = "estimate",
                               moe_col = "moe",
                               name_col = "age_pop") %>%
    # est_moe_derive(group_cols = c("name", "age", "disab_pop"),
    #                est_col = "estimate",
    #                moe_col = "moe",
    #                name_col = "age_disab_pop") %>%
    acsprocess::est_moe_derive(group_cols = c("name", "age", "disab_type"),
                               est_col = "estimate",
                               moe_col = "moe",
                               "age_disab_type") %>%
    dplyr::select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    dplyr::distinct() %>%
    dplyr::filter(disab_pop == "Has a disability") %>%
    acsprocess::derive_pct_est_moe("age_disab_type_pct_age",
                                   "age_pop_est",
                                   "age_pop_moe",
                                   "age_disab_type_est",
                                   "age_disab_type_moe") %>%
    dplyr::rename(pct_moe_age = pct_moe) %>%
    acsprocess::derive_pct_est_moe("age_disab_type_pct_pop",
                                   "pop_total",
                                   "pop_total_moe",
                                   "age_disab_type_est",
                                   "age_disab_type_moe") %>%
    dplyr::rename(pct_moe_pop = pct_moe) %>%
    acsprocess::derive_pct_est_moe("age_disab_type_pct_disab",
                                   "name_age_disability_est",
                                   "name_age_disability_est",
                                   "age_disab_type_est",
                                   "age_disab_type_moe") %>%
    dplyr::rename(pct_moe_disab = pct_moe)
}

#' Pull race/disability/age
#'
#' Pulls race/disability/age from tidycensus downloaded ACS5 data
#'
#' @param df tidycensus downloaded ACS5 data on race/disability/age
#'
#' @return Dataframe with variables for age, disability, race
#' @export
#'
#' @examples
process_race_disability_age <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "age", "disability")) %>%
    acsprocess::race_pull()
}

#' Process race/disability data
#'
#' Processes tidycensus downloaded ACS5 data on disability by race
#'
#' @param df tidycensus downloaded ACS5 data on disability and race and age
#'
#' @return tidycensus downloaded ACS5 data on race/disability
#' @export
#'
#' @examples
process_race_disability <- function(df){
  df %>%
    acsprocess::process_race_disability_age() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "age")) %>%
    dplyr::filter(!is.na(disability)) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "race", "disability")) %>%
    dplyr::select(-c(variable, estimate, moe, label, age, concept)) %>%
    dplyr::distinct() %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race_disability",
                                   aggregate_est = "race_total",
                                   aggregate_moe = "race_total_moe",
                                   component_est = "name_race_disability_est",
                                   component_moe = "name_race_disability_moe")
}


# veteran status ----

#' Base-process veteran-status by sex and age
#'
#' Conducts base-processing of veteran status by sex and age. This dataset is formatted oddly (separated-columns are somewhat inconsistent), so just separates label into sex, age, and veteran columns
#'
#' @param df Tidycensus ACS5 dataframe on veteran status by age and sex
#'
#' @return Label-separated ACS5 dataframe on veteran status by age and sex
#' @export
#'
#' @examples
base_process_vetagesex <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "sex", "age", "veteran"))
}

#' Process veteran-status by sex and age dataset into veteran-status by age
#'
#' Processes unprocessed veteran-status by sex and age dataset, turning it into a dataset of veteran-status by age
#'
#' @param df Unprocessed ACS5 veteran-status by sex and age dataset
#' @param bindoverall Default "age"; set to NULL to not add an extra row to the dataset containing results for veteran-status for the overall population
#'
#' @return Processed datset of veteran-status by sex and age
#' @export
#'
#' @examples
process_vetage <- function(df, bindoverall = "age"){
  df %>%
    acsprocess::base_process_vetagesex() %>%
    dplyr::filter(!grepl("veteran", sex, ignore.case = T) & !grepl("veteran", age, ignore.case = T)) %>%
    acsprocess::total_col_add(list("poptot" = "sex", "sextot" = "age", "agetot" = "veteran"), join_col = c("name", "sex", "age")) %>%
    acsprocess::process_df(group_cols = c("name", "age", "veteran"), overall_cols = c("name", "veteran"), name_col = "agevet", bind_overall = bindoverall)  %>%
    acsprocess::age_recode(age)
}



# sex ----

#' Process sex data
#'
#' Processes tidycensus downloaded ACS5 data on sex
#'
#' @param df tidycensus downloaded ACS5 data on sex
#'
#' @return Processed data on sex
#' @export
#'
#' @examples
process_gender <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "gender")) %>%
    acsprocess::total_col_add(total_cols = c("total" = "gender")) %>%
    acsprocess::derive_pct_est_moe("pct_gender", "total", "total_moe")
}

#' Process age/sex data
#'
#' Processes tidycensus downloaded ACS5 data on sex by age
#'
#' @param df tidycensus downloaded ACS5 data on sex by age
#'
#' @return Processed data on age by sex
#' @export
#'
#' @examples
process_age_gender <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "gender", "age")) %>%
    acsprocess::total_col_add(c("tot_people" = "gender", "tot_gender" = "age"), c("name", "gender")) %>%
    acsprocess::derive_pct_est_moe("pct_age",
                       "tot_gender",
                       "tot_gender_moe")
}

# age ----

#' Process age data
#'
#' Processes tidycensus downloaded age data, table B01001
#'
#' @param df Dataframe containing SEX BY AGE table, table B01001 in 2020 5-year ACS
#'
#' @return Dataframe grouped by age group at given geography
#' @export
#'
#' @examples
process_age_overall <- function(df){
  loccols <- grep("(^location$)|(^name$)", colnames(df), value = T)

  df %>%
    acsprocess::separate_label(c(NA, NA, "gender", "age")) %>%
    acsprocess::total_col_add(c("tot_people" = "gender", "tot_gender" = "age"), c("name", "gender")) %>%
    acsprocess::est_moe_derive(c("name", "age"), name_col = "age_tot") %>%
    dplyr::select(geoid, tidyselect::all_of(loccols), tot_people, tot_people_moe, age, age_tot_est, age_tot_moe) %>%
    dplyr::distinct() %>%
    dplyr::rename(estimate = age_tot_est,
                  moe = age_tot_moe) %>%
    acsprocess::derive_pct_est_moe("pct_age",
                       "tot_people",
                       "tot_people_moe")

}

#' Process sex/age to under 18 and over 65
#'
#' Recodes age/sex data to produce a dataset showing the total people under age 18, between 18 and 65, and 65+
#'
#' @param df_agesex tidycensus downloaded/unprocessed age/sex dataframe
#'
#' @return Processed dataframe with age recoded to under 18, between 18 and 65, and 65+
#' @export
#'
#' @examples
process_age_un18over65 <- function(df_agesex){

  df_agesex %>%
    acsprocess::process_age_overall() %>%
    dplyr::mutate(
      agegrp = dplyr::case_when(
        grepl("(^Under)|(^[1-9] to)|(^1[0-7] )", age) ~ "Under 18",
        grepl("(^6[5-9] )|(^[7-9][0-9] )|(over$)", age) ~ "65 and over",
        T ~ "18-65"
      )
    ) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "agegrp")) %>%
    dplyr::select(
      geoid, name, agegrp, tot_people, tot_people_moe, name_agegrp_est, name_agegrp_moe
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(estimate = name_agegrp_est,
                  moe = name_agegrp_moe) %>%
    acsprocess::derive_pct_est_moe("pct_age", "tot_people", "tot_people_moe")

}

# poverty ----

#' Process poverty by race and age data
#'
#' Processes tidycensus downloaded ACS5 data on poverty by race by age
#'
#' @param df tidycensus downloaded ACS5 data on poverty by race by age
#'
#' @return Processes data on poverty by race by age
#' @export
#'
#' @examples
process_poverty_race_age <- function(df) {
  df %>%
    acsprocess::separate_label(c(NA, NA, "poverty", "age")) %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(name_race = paste0(name, "_", race)) %>%
    acsprocess::total_col_add(c("race_tot" = "poverty", "poverty_tot" = "age"),
                  c("name_race", "poverty")) %>%
    acsprocess::est_moe_derive(c("name", "age", "race"), name_col = "age_race_tot") %>%
    acsprocess::derive_pct_est_moe("pct_age",
                       "age_race_tot_est",
                       "age_race_tot_moe")
}


#' Process detailed poverty data
#'
#' Processes detailed tidycensus downloaded ACS5 data on poverty
#'
#' @param df tidycensus downloaded ACS5 data on poverty
#'
#' @return ACS5 data on poverty
#' @export
#'
#' @examples
process_poverty_detail <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "incpov_ratio")) %>%
    acsprocess::total_col_add(c("pop_tot" = "incpov_ratio")) %>%
    acsprocess::derive_pct_est_moe("pct_incpov",
                       "pop_tot",
                       "pop_tot_moe")
}

#' Process income-poverty df to under 1 and under 2x ratio
#'
#' Processes tidycensus downloaded income-poverty ratio dataset to show under 1x the poverty level and under 2x the poverty level
#'
#' @param df_povratio tidycensus downloaded income-poverty ratio dataset
#'
#' @return Income-poverty ratio dataset with totals of under the poverty level and 2 times the poverty level
#' @export
#'
#' @examples
process_incpov_un1un2 <- function(df_povratio){

  df %>%
    acsprocess::process_poverty_detail() %>%
    acsprocess::incpov_recode(incpov_ratio) %>%
    dplyr::group_by(name) %>%
    arrange(incpov_new) %>%
    dplyr::mutate(estimate = cumsum(estimate),
                  pct_incpov = estimate/ pop_tot) %>%
    dplyr::ungroup() %>%
    dplyr::filter(incpov_new %in% c(".50 to .99", "1.85 to 1.99")) %>%
    dplyr::mutate(incpov_ratio = dplyr::case_when(
      incpov_new == ".50 to .99" ~ "Under poverty line",
      incpov_new == "1.85 to 1.99"~ "Under 2X poverty line"
    )) %>%
    dplyr::mutate(
      dplyr::across(grep("^pct_", colnames(.), value = T),
             ~ round(.x * 100, 2))
    )
}

#' Process poverty by sex data
#'
#' Processes tidycensus downloaded ACS5 data on poverty by sex
#'
#' @param df tidycensus downloaded ACS5 data on poverty by sex
#'
#' @return Processed data on poverty by sex
#' @export
#'
#' @examples
process_poverty_sex <- function(df){
  return <- df %>%
    acsprocess::separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    acsprocess::total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"),
                  join_col = c("name", "pov_status", "sex")) %>%
    acsprocess::est_moe_derive(c("name", "sex"), name_col = "pop_sex") %>%
    dplyr::select(-c(variable, estimate, moe, label, age)) %>%
    dplyr::distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_sex_pov",
                       "pop_sex_est",
                       "pop_sex_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_sex = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_sex_pop",
                       "pop",
                       "pop_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_pop = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_pov_all",
                       "pov_tot",
                       "pov_tot_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_pov_all = pct_moe)
}

#' Process poverty by race and sex
#'
#' Processes tidycensus downloaded ACS5 data on poverty by race and sex
#'
#' @param df tidycensus downloaded ACS5 data on poverty by race and sex
#'
#' @return tidycensus downloaded ACS5 data on poverty by race and sex
#' @export
#'
#' @examples
process_poverty_race_sex <- function(df){
  return <- df %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(name_race = paste0(name, race)) %>%
    acsprocess::separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    acsprocess::total_col_add(c("race_pop" = "pov_status",
                    "pov_race_tot" = "sex",
                    "pov_race_sex_tot" = "age"),
                  join_col = c("name_race", "pov_status", "sex")) %>%
    acsprocess::est_moe_derive(c("name_race", "sex"), name_col = "race_sex")

  overall <- return %>%
    # filter out overlapping ethnicity
    dplyr::filter(!grepl("hispanic", race, ignore.case = T)) %>%
    acsprocess::est_moe_derive(c("name", "sex"), name_col = "sex") %>%
    acsprocess::est_moe_derive(c("name"), name_col = "pop") %>%
    acsprocess::est_moe_derive(c("name", "pov_status"), name_col = "pov") %>%
    acsprocess::est_moe_derive(c("name", "sex", "pov_status"), name_col = "sex_pov") %>%
    dplyr::select(geoid, name, sex, pov_status, pop_est, pop_moe, sex_est, sex_moe, pov_est, pov_moe, sex_pov_est, sex_pov_moe) %>%
    dplyr::distinct()

  return_final <- return %>%
    dplyr::left_join(overall) %>%
    dplyr::select(-c(variable, estimate, moe, label, age)) %>%
    dplyr::distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    acsprocess::derive_pct_est_moe("pct_race_sex_pov",
                       "race_sex_est",
                       "race_sex_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_race_sex_pov = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_race_pov",
                       "race_pop",
                       "race_pop_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_race_pov = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_pov",
                       "pov_est",
                       "pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_pov = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_sex_pov",
                       "sex_pov_est",
                       "sex_pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    dplyr::rename(pct_moe_sex_pov = pct_moe)
}


#' Process poverty by sex and age
#'
#' Processes tidycensus downloaded ACS5 data on poverty by sex and age
#'
#' @param df tidycensus downloaded ACS5 data on poverty by sex and age
#'
#' @return Processed data on poverty by sex and age
#' @export
#'
#' @examples
process_poverty_sex_age <- function(df){
  return <- df %>%
    acsprocess::separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    acsprocess::total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"),
                  join_col = c("name", "pov_status", "sex")) %>%
    est_moe_derive(c("name", "sex"), name_col = "sex_pop") %>%
    est_moe_derive(c("name", "sex", "age"), name_col = "sex_age_pop") %>%
    est_moe_derive(c("name", "pov_tot"), name_col = "pov_tot") %>%
    dplyr::select(-c(variable, label)) %>%
    dplyr::distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    acsprocess::derive_pct_est_moe("pct_sex_pov",
                       "pov_sex_tot",
                       "pov_sex_tot_moe"
    ) %>%
    dplyr::rename(pct_moe_sex_pov = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_pop",
                       "pop",
                       "pop_moe",
    ) %>%
    dplyr::rename(pct_moe_pop = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_sex_pop",
                       "sex_pop_est",
                       "sex_pop_moe") %>%
    dplyr::rename(pct_moe_sex_pop = pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_pop_pov",
                       "pov_tot_est",
                       "pov_tot_moe") %>%
    dplyr::rename(pct_moe_pop_pov= pct_moe) %>%
    acsprocess::derive_pct_est_moe("pct_sex_age",
                       "sex_age_pop_est",
                       "sex_age_pop_moe")
}

#' Process poverty data
#'
#' Processes tidycensus downloaded ACS5 data on poverty
#'
#' @param df tidycensus downloaded ACS5 data on poverty
#'
#' @return Processed data on poverty
#' @export
#'
#' @examples
process_poverty <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "income_rel_poverty")) %>%
    acsprocess::total_col_add(total_cols = list("pop_overall" = "income_rel_poverty")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_income",
                                   aggregate_est = "pop_overall",
                                   aggregate_moe = "pop_overall_moe") %>%
    dplyr::mutate(pct_cumul_income = cumsum(pct_income))
}


# family ----

#' Process family-status data
#'
#' Processes tidycensus downloaded ACS5 data on family status
#'
#' @param df tidycensus downloaded ACS5 data on family
#'
#' @return Processed data on family status
#' @export
#'
#' @examples
process_family <- function(df){
  process <- df %>%
    acsprocess::separate_label(c(NA, NA, "hous_type", "hous_people")) %>%
    acsprocess::total_col_add(c("housholds" = "hous_type",
                    "tot_type" = "hous_people"), join_col = c("name", "hous_type"))
}

#' Process poverty by family status
#'
#' Processes tidycensus downloaded ACS5 data on poverty by family status
#'
#' @param df tidycensus downloaded ACS5 data on poverty by family status
#'
#' @return Processed data on poverty by family status
#' @export
#'
#' @examples
process_family_poverty <- function(df){
  process <- df %>%
    acsprocess::separate_label(c(NA, NA, "pov_status", "fam_type", "fam_occupants", "occupants_age", "other_fam_extra"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", fam_type)) %>%
    dplyr::filter(!is.na(fam_occupants)) %>%
    dplyr::mutate(fam_type = fam_occupants,
                  fam_occupants = occupants_age,
                  occupants_age = other_fam_extra) %>%
    dplyr::select(-other_fam_extra)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", fam_type)) %>%
    dplyr::select(-other_fam_extra) %>%
    rbind(other_process) %>%
    dplyr::mutate(occupants_age = ifelse(grepl("No related", fam_occupants), "No children", occupants_age)) %>%
    acsprocess::total_col_add(c("households" = "pov_status", "pov_status_tot" = "fam_type", "pov_type_tot" = "fam_occupants", "pov_type_occ_tot" = "occupants_age"), join_col = c("name", "pov_status", "fam_type", "fam_occupants"))
}

#' Process family by poverty and race
#'
#' Processes tidycensus downloaded ACS5 data on family-status by poverty and race
#'
#' @param df tidycensus downloaded ACS5 data on family-status by poverty and race
#'
#' @return
#' @export
#'
#' @examples
process_family_poverty_race <- function(df){
  process <- df %>%
    acsprocess::race_pull() %>%
    dplyr::mutate(name_race = paste0(name, race)) %>%
    acsprocess::separate_label(c(NA, NA, "pov_status", "fam_type", "fam_occupants", "occupants_age", "other_fam_extra"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", fam_type)) %>%
    dplyr::filter(!is.na(fam_occupants)) %>%
    dplyr::mutate(fam_type = fam_occupants,
           fam_occupants = occupants_age,
           occupants_age = other_fam_extra) %>%
    dplyr::select(-other_fam_extra)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", fam_type)) %>%
    dplyr::select(-other_fam_extra) %>%
    rbind(other_process) %>%
    dplyr::mutate(occupants_age = ifelse(grepl("No related", fam_occupants), "No children", occupants_age)) %>%
    acsprocess::total_col_add(
      total_cols = c(
        "race_tot" = "pov_status",
        "race_pov_tot" = "fam_type",
        "race_pov_fam_tot" = "fam_occupants",
        "race_pov_fam_occ_tot" = "occupants_age"
      ),
      join_col = c(
        "name_race",
        "pov_status",
        "fam_type",
        "fam_occupants"
      )
    )
}


#' Process family type by income
#'
#' Processes tidycensus downloaded ACS5 data on family type by income
#'
#' @param df tidycensus downloaded ACS5 data on family type by income
#'
#' @return Processed data on family type by income
#' @export
#'
#' @examples
process_family_type_income <- function(df){
  process <- df %>%
    acsprocess::separate_label(c(NA, NA, NA, "fam_type", NA, "child", NA, "other_fam_extra"))

  other_process <- process %>%
    dplyr::filter(grepl("Other family", fam_type)) %>%
    dplyr::filter(!is.na(child)) %>%
    dplyr::mutate(fam_type = child,
           child = other_fam_extra) %>%
    dplyr::select(-other_fam_extra)

  process_final <- process %>%
    dplyr::filter(!grepl("Other family", fam_type)) %>%
    dplyr::select(-other_fam_extra) %>%
    rbind(other_process) %>%
    acsprocess::total_col_add(c("overall_med" = "fam_type", "fam_med" = "child"),
                  join_col = c("name", "fam_type"))
}

#' Process family-type by tenure
#'
#' Processes tidycensus downloaded ACS5 data on family-type by tenure data
#'
#' @param df tidycensus downloaded ACS5 data on family-type by tenure
#'
#' @return tidycensus downloaded ACS5 data on family-type by tenure
#' @export
#'
#' @examples
process_family_tenure <- function(df){
  process <- df %>%
    acsprocess::separate_label(
      c(NA, NA, "tenure", "fam_type", "own_child", "child_age")
    ) %>%
    dplyr::mutate(own_child = ifelse(grepl("No related", fam_type), fam_type, own_child),
           child_age = ifelse(grepl("(No related)|(No own)", own_child),own_child, child_age)) %>%
    acsprocess::total_col_add(
      total_cols = c("pop_tot" = "tenure", "tenure_tot" = "fam_type", "tenure_fam_tot" = "own_child", "tenure_fam_own" = "child_age"),
      join_col = c("name", "tenure", "fam_type", "own_child")
    )
}

#' Process family-type by public assistance
#'
#' @param df tidycensus downloaded ACS5 data on family-type by receipt of public assistance
#'
#' @return tidycensus downloaded ACS5 data on family-type by public assistance
#' @export
#'
#' @examples
process_family_pubassist <- function(df){
  process <- df %>%
    acsprocess::separate_label(
      c(NA, NA, "pubassist", "child", "fam_type", "other_fam")
    )

  other_process <- process %>%
    dplyr::filter(grepl("Other", fam_type)) %>%
    dplyr::filter(!is.na(other_fam)) %>%
    dplyr::mutate(fam_type = other_fam) %>%
    dplyr::select(-other_fam)

  process_final <- process %>%
    dplyr::filter(!grepl("Other", fam_type)) %>%
    dplyr::select(-other_fam) %>%
    rbind(other_process) %>%
    acsprocess::total_col_add(c("hous_tot" = "pubassist", "pub_tot" = "child", "child_tot" = "fam_type"), join_col = c("name", "pubassist", "child"))

}


#' Process Tenure by Family-type and Age
#'
#' Processes ACS5 tidycensus downloaded data on tenure by family status and age
#'
#' @param df Processes ACS5 tidycensus downloaded data on tenure by family status and age
#'
#' @return Lightly processed data on tenure by family status and age
#' @export
#'
#' @examples
proces_family_age_tenure <- function(df){

  df_age_tenure <- df %>%
    acsprocess::separate_label(c(NA, NA, "tenure", "fam_type", "livalone", "houseage", "other_fam_extra"))

  other_process <- df_age_tenure %>%
    dplyr::filter(grepl("Other", livalone)) %>%
    # exclude totals
    # dplyr::filter(!is.na(houseage)) %>%
    dplyr::mutate(fam_type = livalone,
                  livalone = houseage,
                  houseage = other_fam_extra) %>%
    dplyr::select(-other_fam_extra)

  process_final <- df_age_tenure %>%
    dplyr::filter(!grepl("Other", livalone)) %>%
    dplyr::select(-other_fam_extra) %>%
    rbind(other_process) %>%
    acsprocess::total_col_add(list("households" = "tenure", "tenuretot" = "fam_type", "famtot" = "livalone", "occ" = "houseage"), join_col = c("name", "tenure", "fam_type", "livalone"))

}

# public assistance ----

#' Process race by public assistance data
#' Processes tidycensus downloaded ACS5 data on receipt of public assistance by race
#'
#' @param df tidycensus downloaded ACS5 data on receipt of public assistance by race
#'
#' @return Processed data on receipt of public assistance by race
#' @export
#'
#' @examples
process_race_assist <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "public_assist")) %>%
    acsprocess::race_pull() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "public_assist")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

# transportation ----

#' Base-process transport to work by race
#'
#' Processes tidycensus downloaded ACS5 data on transport to work by race
#'
#' @param df tidycensus downloaded ACS5 data on transport to work by race
#'
#' @return Processed data on transport to work by race
#' @export
#'
#' @examples
process_race_transport_base <- function(df){
  df %>%
    acsprocess::separate_label(c(NA, NA, "transport_to_work")) %>%
    acsprocess::race_pull()
}

#' Process transport to work by race
#'
#' Processes tidycensus downloaded ACS5 data on transport to work by race
#'
#' @param df tidycensus downloaded ACS5 data on transport to work by race
#'
#' @return Processed data on transport to work by race
#' @export
#'
#' @examples
process_race_transport_complete <- function(df) {
  df %>%
    acsprocess::process_race_transport_base() %>%
    acsprocess::total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "transport_to_work")) %>%
    acsprocess::derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

#' Process vehicle-ownership data by tenure
#'
#' Processes tidycensus-downloaded tenure by number of vehicles owned data, table B25044 in 2020 5-year ACS
#'
#' @param df Dataframe on number of vehicles owned by tenure, reflecting table B25044 in 2020 5-year ACS
#' @param overall Whether to aggregate data up to level of owning a vehicle (without tenure); default FALSE
#'
#' @return Processed-dataframe on vehicle ownership by tenure, or households by vehicle ownership
#' @export
#'
#' @examples
process_tenure_vehicleown <- function(df, overall = F){

  vehicledf <- df %>%
    acsprocess::separate_label(c(NA, NA, "tenure", "vehicle")) %>%
    acsprocess::total_col_add(total_cols = c("tothous" = "tenure", "tottenure" = "vehicle"), join_col = c("name", "tenure"))

  if (overall){
    vehicledf <- vehicledf %>%
      acsprocess::est_moe_derive(
        group_cols = c("name", "vehicle")
      ) %>%
      dplyr::select(geoid, name, vehicle, tothous, tothous_moe, name_vehicle_est, name_vehicle_moe) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        estimate = name_vehicle_est,
        moe = name_vehicle_moe
      ) %>%
      acsprocess::derive_pct_est_moe(proportion_col = "pct_vehicle", aggregate_est = "tothous", aggregate_moe = "tothous_moe") %>%
      dplyr::mutate(
        anyvehicle = dplyr::case_when(
          vehicle == "No vehicle available" ~ "No vehicle",
          TRUE ~ "Owns a vehicle"
        )
      ) %>%
      acsprocess::est_moe_derive(
        group_cols = c("name", "anyvehicle"),
        name_col = "anyveh"
      ) %>%
      acsprocess::derive_pct_est_moe(proportion_col = "pct_anyveh",
                                     "tothous",
                                     "tothous_moe",
                                     "anyveh_est",
                                     "anyveh_moe") %>%
      dplyr::select(geoid, name, tothous, tothous_moe, anyvehicle, anyveh_est, anyveh_moe, pct_anyveh, pct_moe, pct_upper, pct_lower) %>%
      dplyr::distinct() %>%
      rename(estimate = anyveh_est,
             moe = anyveh_moe)

  }

  else {
    vehicledf <- vehicledf %>%
      acsprocess::derive_pct_est_moe("pct_vehicle",
                                     "tottenure",
                                     "tottenure_moe") %>%
      dplyr::mutate(
        anyvehicle = dplyr::case_when(
          vehicle == "No vehicle available" ~ "No vehicle",
          TRUE ~ "Owns a vehicle"
        )
      ) %>%
      acsprocess::est_moe_derive(
        group_cols = c("name", "tenure", "anyvehicle"),
        name_col = "anyveh"
      ) %>%
      acsprocess::derive_pct_est_moe(proportion_col = "pct_anyveh",
                                     "tottenure",
                                     "tottenure_moe",
                                     "anyveh_est",
                                     "anyveh_moe")

  }

  vehicledf
}


