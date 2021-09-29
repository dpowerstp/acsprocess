# function to copy labels in and filter to tp
#' Prepare ACS data
#' Function to standardize tidycensus downloaded ACS data, with option to filter to a specific place
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
puller_funct <- function(df, place_string = "", no_pull = FALSE, variables = variables_2019) {

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

process_race_ethn <- function(df){

  race_ethn_processed <- df %>%
    separate_label(c(NA, NA, "race_ethnicity", "two"))

  race_ethn_processed <- race_ethn_processed %>%
    mutate(race_ethnicity = ifelse(two == "White alone" & !is.na(two), "NH White alone", race_ethnicity),
           two = ifelse(two == "White alone", NA, two)) %>%
    filter(is.na(two) & (!is.na(race_ethnicity) | !grepl("HISPANIC", concept, ignore.case = T)))

  # browser()

  race_ethn_processed <- race_ethn_processed %>%
    total_col_add(total_cols = list("pop_total" = "race_ethnicity")) %>%
    select(-c(concept, variable)) %>%
    distinct() %>%
    filter(!(grepl("Two or more", race_ethnicity) & !is.na(two))) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "pop_total",
                       aggregate_moe = "pop_total_moe") %>%
    select(-c(label, two))

}

process_computer_overall <- function(df){
  df %>%
    separate_label(c(NA, NA, "comp_access", "int_type")) %>%
    race_pull() %>%
    mutate(place_race = paste0(name, "_", race)) %>%
    mutate(int_type = ifelse(grepl("No computer", comp_access, ignore.case = T), "No computer", int_type)) %>%
    total_col_add(c("tot_people" = "comp_access", "tot_comp" = "int_type"), join_col = c("place_race")) %>%
    derive_pct_est_moe(proportion_col = "percent_race",
                       aggregate_est = "tot_people",
                       aggregate_moe = "tot_people_moe")
}

# function to process education data overall
process_educ_overall <- function(df){
  df %>%
    separate_label(c(NA, NA, "education")) %>%
    total_col_add(total_cols = list("population" = "education")) %>%
    derive_pct_est_moe(proportion_col = "percent_pop",
                       aggregate_est = "population",
                       "population_moe")
}

# function to process race/household income
process_income_race <- function(df){

  df %>%
    race_pull() %>%
    separate_label(c(NA, NA, "income_range")) %>%
    filter(race != "white alone") %>%
    total_col_add(total_cols = list("race_households" = "income_range"), join_col = c("name", "race")) %>%
    derive_pct_est_moe(proportion_col = "percent_race_households",
                       aggregate_est = "race_households",
                       aggregate_moe = "race_households_moe")
}

process_income_race_median <- function(df) {
  df %>%
    race_pull() %>%
    mutate(race = ifelse(grepl("in 2019 inflation", race), NA, race)) %>%
    separate_label(c(NA, "Median household income")) %>%
    total_col_add(total_cols = list("overall_median_income" = "race")) %>%
    filter(race != "white alone") %>%
    derive_pct_est_moe(proportion_col = "ratio_median_income",
                       aggregate_est = "overall_median_income",
                       aggregate_moe = "overall_median_income_moe",
                       type_moe = "ratio")
}

process_employment_age_gender <- function(df){
  df %>%
    filter(grepl("In labor force", labor_force_status)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force)) %>%
    total_col_add(list("total_workers" = "type_labor_force"), join_col = c("name", "gender", "age_range")) %>%
    filter(!(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    est_moe_derive(group_cols = c("name", "gender", "age_range", "type_labor_force", "employment_status"),
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_gender_age",
                       aggregate_est = "total_workers",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, estimate, moe, label, concept, race)) %>%
    distinct()
}

educ_race_process <- function(df){

  # education/race overall
  educ_race_df_overall <- df %>%
    process_educ_race() %>%
    filter(!is.na(education) | is.na(education) & is.na(gender)) %>%
    total_col_add(list("race_total" = "gender"), join_col = c("name", "race")) %>%
    est_moe_derive(c("name", "race", "education")) %>%
    select(-c(variable, gender, label, concept, estimate, moe)) %>%
    distinct()

  educ_race_df_overall <- educ_race_df_overall %>%
    derive_pct_est_moe(proportion_col = "race_education_pct",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe",
                       component_est = "name_race_education_est",
                       component_moe = "name_race_education_moe")

  educ_race_df_overall

}


tenure_race_process <- function(df){

  df %>%
    process_race_home_own() %>%
    filter(!is.na(tenure)) %>%
    total_col_add(join_col = c("name", "tenure"), total_cols = list("tenure_overall" = "race")) %>%
    select(-c(variable, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "race_pct_tenure",
                       aggregate_est = "tenure_overall",
                       aggregate_moe = "tenure_overall_moe") %>%
    arrange(tenure, race)
}


burden_owners_process <- function(df){

  burden_owners_overall <- df %>%
    separate_label(c(NA, NA, "mortgage_status", "percent_household_income")) %>%
    filter(!(!is.na(mortgage_status) & is.na(percent_household_income))) %>%
    total_col_add(total_cols = list("total_homeowners" = "mortgage_status"), join_col = c("name")) %>%
    filter(!is.na(percent_household_income)) %>%
    est_moe_derive(group_cols = c("name", "percent_household_income")) %>%
    select(-c(concept, mortgage_status, variable, label, estimate, moe)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners",
                       aggregate_est = "total_homeowners",
                       aggregate_moe = "total_homeowners_moe",
                       component_est = "name_percent_household_income_est",
                       component_moe = "name_percent_household_income_moe") %>%
    mutate(cumulpct = cumsum(pct_homeowners))

  return(burden_owners_overall)
}

burden_owners_mortgage_process <- function(df){
  df %>%
    separate_label(c(NA, NA, "mortgage_status", "percent_household_income")) %>%
    filter(!is.na(mortgage_status)) %>%
    total_col_add(total_cols = list("total_homeowners_mortgage" = "percent_household_income"), join_col = c("name", "mortgage_status")) %>%
    select(-c(concept, variable, label)) %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners_mortgage",
                       aggregate_est = "total_homeowners_mortgage",
                       aggregate_moe = "total_homeowners_mortgage_moe")
}


process_race_employ_gender_u65 <- function(df) {
  df %>%
    process_race_employment_age_gender_base() %>%
    filter(grepl("C23002", variable)) %>%
    filter(!grepl("sex by age", race) &
             grepl("In labor force", labor_force_status) &
             grepl("16 to 64", age_range) &
             !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status)) %>%
    total_col_add(list("total_labor_force" = "type_labor_force"), c("name", "gender", "race")) %>%
    est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "total_labor_force",
                       aggregate_moe = "total_labor_force_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    distinct()
}


process_race_employment_u65_base <- function(df, tot_df) {

  df %>%
    left_join(tot_df) %>%
    filter(!is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "race", "type_labor_force", "employment_status"),
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "total_workers_est",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, gender, age_range, estimate, moe, label, concept)) %>%
    distinct()
}

process_race_employment_u65 <- function(df){
  race_employment_u65_preprocess <- df %>%
    process_race_employment_age_gender_base() %>%
    filter(grepl("C23002", variable)) %>%
    filter(!grepl("sex by age", race) &
             grepl("In labor force", labor_force_status) &
             grepl("16 to 64", age_range) &
             !(is.na(employment_status) & grepl("Civilian", type_labor_force))) %>%
    mutate(employment_status = ifelse(grepl("In Armed Forces", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status))

  total_df <- race_employment_u65_preprocess %>%
    filter(is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "race"), name_col = "total_workers") %>%
    select(name, race, total_workers_est, total_workers_moe) %>%
    distinct()

  race_employment_u65 <- race_employment_u65_preprocess %>%
    process_race_employment_u65_base(tot_df = total_df)

  race_employment_u65
}

process_household_income_ownercosts <- function(df) {
  df %>%
    total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    filter(housing_costs != "Not computed") %>%
    est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")

}

process_household_income_rentercosts <- function(df){
  df %>%
    total_col_add(list("households_bracket" = "housing_costs"), join_col = c("name", "household_income")) %>%
    filter(housing_costs != "Not computed") %>%
    est_moe_derive(group_cols = c("name", "household_income"), name_col = "households_bracket") %>%
    derive_pct_est_moe(proportion_col = "pct_bracket",
                       aggregate_est = "households_bracket_est",
                       aggregate_moe = "households_bracket_moe")
}

process_ancestry <- function(df, sub = F){

  ancestry <- df %>%
    separate_label(c(NA, NA, "ancestry", "country"))

  if (sub){
    ancestry_combos <- ancestry %>%
      filter(!is.na(country)) %>%
      pull(ancestry) %>%
      unique()

    ancestry <- ancestry %>%
      filter(ancestry %in% ancestry_combos) %>%
      total_col_add(list("tot_ancestry" = "country"), join_col = c("name", "ancestry")) %>%
      derive_pct_est_moe("pct_country",
                         "tot_ancestry",
                         "tot_ancestry_moe")

  }

  else{
    ancestry <- df %>%
      separate_label(c(NA, NA, "ancestry", "country")) %>%
      filter(is.na(country)) %>%
      select(-country) %>%
      total_col_add(total_cols = list("tot_pop" = "ancestry")) %>%
      derive_pct_est_moe("pct_ancestry",
                         "tot_pop",
                         "tot_pop_moe")
  }

  return(ancestry)

}

process_health_race <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "health")) %>%
    race_pull() %>%
    mutate(name_race = paste0(name, "_", race)) %>%
    total_col_add(total_cols = list("race_total" = "age", "race_age_total" = "health"), join_col = c("name_race", "age")) %>%
    derive_pct_est_moe(proportion_col = "pct_age",
                       aggregate_est = "race_age_total",
                       aggregate_moe = "race_age_total_moe")


}


process_disability_overall <- function(df){
  df %>%
    separate_label(c(NA, NA, "sex", "age", "disability")) %>%
    total_col_add(c("tot_pop" = "sex",
                    "tot_sex" = "age",
                    "tot_age" = "disability"),
                  c("name",
                    "sex",
                    "age")) %>%
    derive_pct_est_moe(proportion_col = "pct_disability",
                       aggregate_est = "tot_age",
                       aggregate_moe = "tot_age_moe")
}

process_health_age <- function(df) {
  # browser()
  df <- df %>%
    separate_label(c(NA, NA, "age", "num_plans", "type_coverage"))

  # correct for no insurance
  df_none <- df %>%
    filter(grepl("No health insurance", num_plans)) %>%
    distinct() %>%
    # correct for msissing
    mutate(type_coverage = ifelse(grepl("No health insurance", num_plans), "No insurance", type_coverage))


  df %>%
    rbind(df_none) %>%
    total_col_add(total_cols = c("total" = "age", "age_total" = "num_plans", "age_num_plans_total" = "type_coverage"), join_col = c("name", "age", "num_plans")) %>%
    derive_pct_est_moe("pct_hc",
                       "age_num_plans_total",
                       "age_num_plans_total_moe")
}

process_gender <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender")) %>%
    total_col_add(total_cols = c("total" = "gender")) %>%
    derive_pct_est_moe("pct_gender", "total", "total_moe")
}

process_age_gender <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender", "age")) %>%
    total_col_add(c("tot_people" = "gender", "tot_gender" = "age"), c("name", "gender")) %>%
    derive_pct_est_moe("pct_age",
                       "tot_gender",
                       "tot_gender_moe")
}

process_race_age_gender <- function(df){

  df %>%
    separate_label(c(NA, NA, "gender", "age")) %>%
    acsprocess::race_pull() %>%
    mutate(race_name = paste0(name, "_", race)) %>%
    total_col_add(c("race_tot"= "gender", "gender_tot" = "age"), c("race_name", "gender")) %>%
    derive_pct_est_moe("pct_age",
                       "gender_tot",
                       "gender_tot_moe")

}


process_computer_age <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "comp", "int")) %>%
    mutate(int = ifelse(grepl("No", comp), comp, int)) %>%
    total_col_add(c("pop_tot" = "age", "age_tot" = "comp", "comp_tot" = "int"),
                  c("name", "age", "comp")) %>%
    comp_recode(comp_col = comp, int) %>%
    derive_pct_est_moe("pct_int",
                       "age_tot",
                       "age_tot_moe")
}

process_poverty_race_age <- function(df) {
  df %>%
    separate_label(c(NA, NA, "poverty", "age")) %>%
    race_pull() %>%
    mutate(name_race = paste0(name, "_", race)) %>%
    total_col_add(c("race_tot" = "poverty", "poverty_tot" = "age"),
                  c("name_race", "poverty")) %>%
    est_moe_derive(c("name", "age", "race"), name_col = "age_race_tot") %>%
    derive_pct_est_moe("pct_age",
                       "age_race_tot_est",
                       "age_race_tot_moe")
}

process_asian_disaggregated <- function(df){
  df %>%
    separate_label(c(NA, NA, "ethnicity")) %>%
    total_col_add(list("tot_asian" = "ethnicity")) %>%
    derive_pct_est_moe("pct_asian",
                       "tot_asian",
                       "tot_asian_moe")
}

process_poverty_detail <- function(df){
  df %>%
    separate_label(c(NA, NA, "incpov_ratio")) %>%
    total_col_add(c("pop_tot" = "incpov_ratio")) %>%
    derive_pct_est_moe("pct_incpov",
                       "pop_tot",
                       "pop_tot_moe")
}


process_tenure_age <- function(df){
  df %>%
    separate_label(c(NA, NA, "tenure", "age")) %>%
    total_col_add(c("pop_tot" = "tenure", "tenure_tot" = "age"),
                  c("name", "tenure")) %>%
    derive_pct_est_moe("pct_age",
                       "tenure_tot",
                       "tenure_tot_moe")
}

process_age_lang <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "homelang")) %>%
    total_col_add(c("pop_tot" = "age", "age_tot" = "homelang"), c("name", "age")) %>%
    derive_pct_est_moe("pct_age",
                       "age_tot",
                       "age_tot_moe")
}

process_read_disab_type <- function(df, pop_tot_df){
  df %>%
    disab_pull(disability, concept) %>%
    mutate(name_disab = paste0(name, "_", disability)) %>%
    separate_label(c(NA, NA, "sex", "age", "disab_type")) %>%
    total_col_add(total_cols =
                    c("pop_tot" = "sex",
                      "sex_tot" = "age",
                      "age_tot" = "disab_type"),
                  join_col = c("name_disab", "sex", "age")) %>%
    mutate(disab_pop = ifelse(grepl("With a",
                                    disab_type),
                              "Has a disability",
                              "No disability")) %>%
    # drop population total and join population total data because some disability categories dont include all pop
    select(-c(pop_tot, pop_tot_moe)) %>%
    left_join(pop_tot_df)
}

process_pop_tot <- function(race_ethn_processed_df){
  race_ethn_processed_df %>%
    select(geoid, name, pop_total, pop_total_moe) %>%
    distinct()
}

process_disability_pop <- function (df_disability){
  df_disability %>%
    est_moe_derive(c("name", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, name_disability_est, name_disability_moe) %>%
    distinct()
}


process_disability_age <- function (df_disability){
  df_disability %>%
    est_moe_derive(c("name", "age", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, age, name_age_disability_est, name_age_disability_moe) %>%
    distinct()
}


process_disab_type <- function(df, df_disab_pop){
  df %>%
    select(-c(sex, age, variable, label)) %>%
    est_moe_derive(group_cols = c("name", "disab_type"),
                   est_col = "estimate",
                   moe_col = "moe") %>%
    select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    distinct() %>%
    left_join(df_disab_pop) %>%
    filter(disab_pop == "Has a disability") %>%
    derive_pct_est_moe("name_disab_type_pct_pop",
                       "pop_total",
                       "pop_total_moe",
                       "name_disab_type_est",
                       "name_disab_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("name_disab_type_pct_disab",
                       "name_disability_est",
                       "name_disability_moe",
                       "name_disab_type_est",
                       "name_disab_type_moe") %>%
    rename(pct_moe_disab = pct_moe)
}


process_sex_disab_overall <- function(df){
  df %>%
    est_moe_derive(c("name", "sex", "disability")) %>%
    filter(grepl("With a disability", disability)) %>%
    select(geoid, name, sex, tot_sex, tot_sex_moe, name_sex_disability_est, name_sex_disability_moe) %>%
    distinct()
}


process_disab_sex <- function(df, df_disab_sex){
  # browser()

  df %>%
    est_moe_derive(c("name", "sex", "disability", "disab_type"),
                   name_col = "sex_disability_type") %>%
    filter(grepl("With a", disab_type)) %>%
    select(geoid, name, sex, disability, pop_total, pop_total_moe, sex_disability_type_est, sex_disability_type_moe) %>%
    distinct() %>%
    left_join(df_disab_sex) %>%
    derive_pct_est_moe("sex_disability_type_pct_disability",
                       "name_sex_disability_est",
                       "name_sex_disability_est",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_disab = pct_moe) %>%
    derive_pct_est_moe("sex_disability_type_pct_pop",
                       "pop_total",
                       "pop_total_moe",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("sex_disability_type_pct_sex",
                       "tot_sex",
                       "tot_sex_moe",
                       "sex_disability_type_est",
                       "sex_disability_type_moe") %>%
    rename(pct_moe_sex = pct_moe)
}

process_disab_age <- function(df, disab_age_df) {
  df %>%
    select(-c(sex, variable, label)) %>%
    left_join(disab_age_df) %>%
    est_moe_derive(group_cols = c("name_disab", "age"),
                   est_col = "estimate",
                   moe_col = "moe",
                   name_col = "age_pop") %>%
    # est_moe_derive(group_cols = c("name", "age", "disab_pop"),
    #                est_col = "estimate",
    #                moe_col = "moe",
    #                name_col = "age_disab_pop") %>%
    est_moe_derive(group_cols = c("name", "age", "disab_type"),
                   est_col = "estimate",
                   moe_col = "moe",
                   "age_disab_type") %>%
    select(-c(estimate, moe, sex_tot, sex_tot_moe, age_tot, age_tot_moe)) %>%
    distinct() %>%
    filter(disab_pop == "Has a disability") %>%
    derive_pct_est_moe("age_disab_type_pct_age",
                       "age_pop_est",
                       "age_pop_moe",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_age = pct_moe) %>%
    derive_pct_est_moe("age_disab_type_pct_pop",
                       "pop_total",
                       "pop_total_moe",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("age_disab_type_pct_disab",
                       "name_age_disability_est",
                       "name_age_disability_est",
                       "age_disab_type_est",
                       "age_disab_type_moe") %>%
    rename(pct_moe_disab = pct_moe)
}


process_poverty_sex <- function(df){
  return <- df %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"),
                  join_col = c("name", "pov_status", "sex")) %>%
    est_moe_derive(c("name", "sex"), name_col = "pop_sex") %>%
    select(-c(variable, estimate, moe, label, age)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_sex_pov",
                       "pop_sex_est",
                       "pop_sex_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    rename(pct_moe_sex = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pop",
                       "pop",
                       "pop_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("pct_pov_all",
                       "pov_tot",
                       "pov_tot_moe",
                       "pov_sex_tot",
                       "pov_sex_tot_moe",
    ) %>%
    rename(pct_moe_pov_all = pct_moe)
}

process_poverty_race_sex <- function(df){
  return <- df %>%
    race_pull() %>%
    mutate(name_race = paste0(name, race)) %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("race_pop" = "pov_status",
                    "pov_race_tot" = "sex",
                    "pov_race_sex_tot" = "age"),
                  join_col = c("name_race", "pov_status", "sex")) %>%
    est_moe_derive(c("name_race", "sex"), name_col = "race_sex")

  overall <- return %>%
    # filter out overlapping ethnicity
    filter(!grepl("hispanic", race, ignore.case = T)) %>%
    est_moe_derive(c("name", "sex"), name_col = "sex") %>%
    est_moe_derive(c("name"), name_col = "pop") %>%
    est_moe_derive(c("name", "pov_status"), name_col = "pov") %>%
    est_moe_derive(c("name", "sex", "pov_status"), name_col = "sex_pov") %>%
    select(geoid, name, sex, pov_status, pop_est, pop_moe, sex_est, sex_moe, pov_est, pov_moe, sex_pov_est, sex_pov_moe) %>%
    distinct()

  return_final <- return %>%
    left_join(overall) %>%
    select(-c(variable, estimate, moe, label, age)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_race_sex_pov",
                       "race_sex_est",
                       "race_sex_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_race_sex_pov = pct_moe) %>%
    derive_pct_est_moe("pct_race_pov",
                       "race_pop",
                       "race_pop_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_race_pov = pct_moe) %>%
    derive_pct_est_moe("pct_pov",
                       "pov_est",
                       "pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_pov = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pov",
                       "sex_pov_est",
                       "sex_pov_moe",
                       "pov_race_sex_tot",
                       "pov_race_sex_tot_moe",
    ) %>%
    rename(pct_moe_sex_pov = pct_moe)
}


process_poverty_sex_age <- function(df){
  return <- df %>%
    separate_label(c(NA, NA, "pov_status", "sex", "age")) %>%
    total_col_add(c("pop" = "pov_status",
                    "pov_tot" = "sex",
                    "pov_sex_tot" = "age"),
                  join_col = c("name", "pov_status", "sex")) %>%
    est_moe_derive(c("name", "sex"), name_col = "sex_pop") %>%
    est_moe_derive(c("name", "sex", "age"), name_col = "sex_age_pop") %>%
    est_moe_derive(c("name", "pov_tot"), name_col = "pov_tot") %>%
    select(-c(variable, label)) %>%
    distinct() %>%
    # filter(grepl("below poverty", pov_status)) %>%
    derive_pct_est_moe("pct_sex_pov",
                       "pov_sex_tot",
                       "pov_sex_tot_moe"
    ) %>%
    rename(pct_moe_sex_pov = pct_moe) %>%
    derive_pct_est_moe("pct_pop",
                       "pop",
                       "pop_moe",
    ) %>%
    rename(pct_moe_pop = pct_moe) %>%
    derive_pct_est_moe("pct_sex_pop",
                       "sex_pop_est",
                       "sex_pop_moe") %>%
    rename(pct_moe_sex_pop = pct_moe) %>%
    derive_pct_est_moe("pct_pop_pov",
                       "pov_tot_est",
                       "pov_tot_moe") %>%
    rename(pct_moe_pop_pov= pct_moe) %>%
    derive_pct_est_moe("pct_sex_age",
                       "sex_age_pop_est",
                       "sex_age_pop_moe")
}

process_family <- function(df){
  process <- df %>%
    separate_label(c(NA, NA, "hous_type", "hous_people")) %>%
    total_col_add(c("housholds" = "hous_type",
                    "tot_type" = "hous_people"), join_col = c("name", "hous_type"))
}

process_family_poverty <- function(df){
  process <- df %>%
    separate_label(c(NA, NA, "pov_status", "fam_type", "fam_occupants", "occupants_age")) %>%
    total_col_add(c("households" = "pov_status", "pov_status_tot" = "fam_type", "pov_type_tot" = "fam_occupants", "pov_type_occ_tot" = "occupants_age"), join_col = c("name", "pov_status", "fam_type", "fam_occupants"))
}

process_educ_race <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender", "education")) %>%
    race_pull() %>%
    filter(race != "white alone")
}

process_educ_race_gender <- function(df){
  df %>%
    process_educ_race()
  filter(!is.na(gender)) %>%
    mutate(race_place = paste0(name, "_", race)) %>%
    total_col_add(list("race_gender_total" = "education"), join_col = c("race_place", "gender")) %>%
    filter(!is.na(education)) %>%
    est_moe_derive(c("name", "race", "education", "gender")) %>%
    select(-c(variable, label, concept, estimate, moe)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "race_education_gender_pct",
                       aggregate_est = "race_gender_total",
                       aggregate_moe = "race_gender_total_moe",
                       component_est = "name_race_education_gender_est",
                       component_moe = "name_race_education_gender_moe")
}

process_foreign_born_birth <- function(df){
  df %>%
    separate_label(c(NA, NA, "continent", "americas")) %>%
    mutate(continent = ifelse(!is.na(americas), americas, continent)) %>%
    select(-americas) %>%
    total_col_add(join_col = "name", total_cols = list("continent_overall" = "continent")) %>%
    select(-c(variable, label, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "continent_pct",
                       aggregate_est = "continent_overall",
                       aggregate_moe = "continent_overall_moe",
                       component_est = "estimate",
                       component_moe = "moe")
}

process_foreign_born <- function(df){
  df %>%
    separate_label(c(NA, NA, "birthplace")) %>%
    total_col_add(join_col = "name", total_cols = list("birth_overall" = "birthplace")) %>%
    derive_pct_est_moe(proportion_col = "birth_pct",
                       aggregate_est = "birth_overall",
                       aggregate_moe = "birth_overall_moe",
                       component_est = "estimate",
                       component_moe = "moe")
}

process_race_home_own <- function(df){
  df %>%
    separate_label(c(NA, NA, "tenure")) %>%
    race_pull() %>%
    mutate(race = ifelse(race == "tenure", NA, race))
}

process_race_home_own_race_tenure <- function(df){
  df %>%
    process_race_home_own() %>%
    filter(!is.na(race)) %>%
    total_col_add(join_col = c("name", "race"),
                  total_cols = list("race_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "race_pct",
                       aggregate_est = "race_overall",
                       aggregate_moe = "race_overall_moe",
                       component_est = "estimate", component_moe = "moe")
}

process_tenure_df <- function(df){
  df %>%
    separate_label(c(NA, NA, "tenure")) %>%
    total_col_add(total_cols = list("tenure_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "pct_tenure",
                       aggregate_est = "tenure_overall",
                       aggregate_moe = "tenure_overall_moe")
}

process_tenure_income_median <- function(df){
  df %>%
    separate_label(c(NA, NA, NA, "tenure")) %>%
    total_col_add(total_cols = list("median_income_overall" = "tenure")) %>%
    derive_pct_est_moe(proportion_col = "ratio_income",
                       aggregate_est = "median_income_overall",
                       aggregate_moe = "median_income_overall_moe",
                       type_moe = "ratio") %>%
    select(-c(label, concept, variable))
}

process_income_last12 <- function(df){
  df %>%
    separate_label(c(NA, NA, "income_range")) %>%
    total_col_add(total_cols = list("pop_overall" = "income_range")) %>%
    derive_pct_est_moe(proportion_col = "pct_income_range",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe") %>%
    group_by(name) %>%
    mutate(pct_cumul = cumsum(pct_income_range))
}

process_home_lang <- function(df){
  df %>%
    separate_label(c(NA, NA, "language")) %>%
    total_col_add(total_cols = list("pop_overall" = "language")) %>%
    derive_pct_est_moe(proportion_col = "pct_language",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe")
}

process_poverty <- function(df){
  df %>%
    separate_label(c(NA, NA, "income_rel_poverty")) %>%
    total_col_add(total_cols = list("pop_overall" = "income_rel_poverty")) %>%
    derive_pct_est_moe(proportion_col = "pct_income",
                       aggregate_est = "pop_overall",
                       aggregate_moe = "pop_overall_moe") %>%
    mutate(pct_cumul_income = cumsum(pct_income))
}

process_race_assist <- function(df){
  df %>%
    separate_label(c(NA, NA, "public_assist")) %>%
    race_pull() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "public_assist")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

process_race_foreign_born <-function(df){
  df %>%
    separate_label(c(NA, NA, "birthplace")) %>%
    race_pull() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "birthplace")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

process_race_disability_age <- function(df){
  df %>%
    separate_label(c(NA, NA, "age", "disability")) %>%
    race_pull()
}

process_race_disability <- function(df){
  df %>%
    process_race_disability_age() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "age")) %>%
    filter(!is.na(disability)) %>%
    est_moe_derive(group_cols = c("name", "race", "disability")) %>%
    select(-c(variable, estimate, moe, label, age, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_race_disability",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe",
                       component_est = "name_race_disability_est",
                       component_moe = "name_race_disability_moe")
}

process_race_internet_base <- function(df){
  df %>%
    separate_label(c(NA, NA, "computer_access", "internet_access")) %>%
    race_pull()
}

process_race_computer <- function(df){
  df %>%
    process_race_internet_base() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    filter(!is.na(computer_access) & is.na(internet_access)) %>%
    select(-c(variable, label, internet_access, concept)) %>%
    distinct() %>%
    derive_pct_est_moe(proportion_col = "pct_race_computer",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

process_race_internet_complete <- function(df){
  df %>%
    process_race_internet_base() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "computer_access")) %>%
    mutate(internet_access = ifelse(computer_access == "No Computer", computer_access, internet_access)) %>%
    filter(!is.na(internet_access)) %>%
    derive_pct_est_moe(proportion_col = "pct_internet",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

process_race_transport_base <- function(df){
  df %>%
    separate_label(c(NA, NA, "transport_to_work")) %>%
    race_pull()
}

process_race_transport_complete <- function(df) {
  df %>%
    process_race_transport_base() %>%
    total_col_add(join_col = c("name", "race"), total_cols = list("race_total" = "transport_to_work")) %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_total",
                       aggregate_moe = "race_total_moe")
}

process_burden_renters <- function(df) {
  df %>%
    separate_label(c(NA, NA, "percent_income")) %>%
    total_col_add(list("total_renters"="percent_income"))  %>%
    derive_pct_est_moe(proportion_col = "pct_renters",
                       aggregate_est = "total_renters",
                       aggregate_moe = "total_renters_moe") %>%
    mutate(cumul_pct = cumsum(pct_renters))
}

process_race_employment_age_gender_base <- function(df){
  df %>%
    separate_label(c(NA, NA, "gender", "age_range", "labor_force_status", "type_labor_force", "employment_status")) %>%
    race_pull()
}

process_race_gender_employment_general <- function(df){
  process <- df %>%
    process_race_employment_age_gender_base() %>%
    filter(grepl("C23002", variable)) %>%
    filter(!grepl("sex by age", race) & grepl("In labor force", labor_force_status) & !is.na(type_labor_force)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           # employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force))

  total_df <- process %>%
    filter(is.na(employment_status) | grepl("65 years", age_range)) %>%
    est_moe_derive(group_cols = c("name","race", "gender"), name_col = "race_gender_laborforce") %>%
    select(name, race, gender, race_gender_laborforce_est, race_gender_laborforce_moe) %>%
    distinct()

  process %>%
    left_join(total_df) %>%
    # calculate total across age groups in labor force
    filter(!is.na(employment_status) | grepl("Armed Forces", type_labor_force)) %>%
    mutate(employment_status= ifelse(is.na(employment_status), type_labor_force, employment_status)) %>%
    est_moe_derive(group_cols = c("name", "race", "gender", "type_labor_force", "employment_status"), name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_race",
                       aggregate_est = "race_gender_laborforce_est",
                       aggregate_moe = "race_gender_laborforce_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, age_range, estimate, moe, label, concept)) %>%
    distinct()
}

process_employment_age_gender_base <- function(df){
  df %>%
    process_race_employment_age_gender_base() %>%
    filter(!grepl("C23002", variable))
}

process_employment_age <- function(df){

  employment_age_preprocess <- df %>%
    process_employment_age_gender_base() %>%
    filter(grepl("In labor force", labor_force_status)) %>%
    mutate(employment_status = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), type_labor_force, employment_status),
           employment_status = ifelse(grepl("In Armed Forces", type_labor_force), "In Armed Forces", employment_status),
           type_labor_force = ifelse(grepl("employed", type_labor_force, ignore.case = TRUE), "Civilian:", type_labor_force))

  total_df <- employment_age_preprocess %>%
    filter(is.na(type_labor_force)) %>%
    est_moe_derive(group_cols = c("name", "age_range"), name_col = "total_workers") %>%
    select(name, age_range, total_workers_est, total_workers_moe) %>%
    distinct()

  employment_age <- employment_age_preprocess %>%
    filter(!is.na(type_labor_force) & !is.na(employment_status)) %>%
    left_join(total_df) %>%
    est_moe_derive(group_cols = c("name", "age_range", "type_labor_force", "employment_status"),
                   name_col = "workers") %>%
    derive_pct_est_moe(proportion_col = "pct_age",
                       aggregate_est = "total_workers_est",
                       aggregate_moe = "total_workers_moe",
                       component_est = "workers_est",
                       component_moe = "workers_moe") %>%
    select(-c(variable, estimate, moe, label, concept, race, gender)) %>%
    distinct()

  employment_age
}

process_race_laborforce_gender_u65 <- function(df){
  race_laborforce_gender_u65 <- df %>%
    process_race_employment_age_gender_base() %>%
    filter(grepl("C23002", variable)) %>%
    filter(grepl("16 to 64", age_range) & is.na(type_labor_force)) %>%
    total_col_add(list("total_persons" = "labor_force_status"), c("name", "race", "gender")) %>%
    est_moe_derive(c("name", "race", "gender", "labor_force_status"), name_col = "persons") %>%
    derive_pct_est_moe(proportion_col = "pct_race_gender",
                       aggregate_est = "total_persons",
                       aggregate_moe = "total_persons_moe",
                       component_est = "persons_est",
                       component_moe = "persons_moe") %>%
    select(-c(variable, estimate, moe, label, concept, age_range, type_labor_force, employment_status)) %>%
    distinct()

}

disab_pull <- function(df, new_col = disability, concept_col = concept){
  df %>%
    mutate({{new_col}} := {{concept_col}} %>%
             tolower %>%
             gsub("sex by age by ", replacement = "", x = .) %>%
             gsub(" difficulty", "", .) %>%
             stringr::str_to_title(.))
}

process_owner_income_num <- function(df){
  df %>%
    filter(is.na(housing_costs)) %>%
    total_col_add(list("num_homeowners" = "household_income")) %>%
    derive_pct_est_moe(proportion_col = "pct_homeowners",
                       "num_homeowners",
                       "num_homeowners_moe")

  data_saver(owner_income_num, place, "owner_income_num", desc_year = desc_year)
}

process_renter_income_num <- function(df){
  df %>%
    filter(is.na(housing_costs)) %>%
    total_col_add(list("num_renters" = "household_income")) %>%
    derive_pct_est_moe(proportion_col = "pct_renters",
                       "num_renters",
                       "num_renters_moe")

}
