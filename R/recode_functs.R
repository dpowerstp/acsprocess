#' Recode ACS race  values
#'
#' Function to recode a tidycensus ACS dataframe with a race column to shorter labels. This is best used after the race_pull function, which separates the tidycensus dataframe "concept" column into a meaningful race value.
#'
#'
#' @param df A dataframe with tidycensus ACS race values
#' @param race_col The column in the dataframe containing tidycensus ACS race values
#' @param new_race_col The name of the new column with renamed race values as an ordered factor based on what race values are present in the dataframe
#'
#' @return A dataframe with a renamed ACS race column
#' @export
#'
#' @examples

race_recode <- function(df, race_col, new_race_col = "race"){

  # browser()
  race_lookup <- function(string){

    # browser()
    val <- dplyr::case_when(grepl("black or", string, ignore.case = TRUE) ~ "Black",
                     grepl("asian alone", string, ignore.case = TRUE) ~ "Asian",
                     grepl("two or more", string, ignore.case = TRUE) ~ "Multiracial",
                     grepl("some other", string, ignore.case = TRUE) ~ "Other",
                     grepl("white alone", string, ignore.case = TRUE) ~ "White",
                     grepl("hispanic or latin", string, ignore.case = TRUE) ~ "Hispanic",
                     grepl("American Indian and", string, ignore.case = TRUE) ~ "AIAN",
                     grepl("Native Hawaiian", string, ignore.case = TRUE)~ "NHPI",
                     grepl("Overall", string, ignore.case = T) ~ "Overall")

    return(val)

  }

  race_vector_order <- c("White", "Black", "Hispanic", "Asian", "Other", "Multiracial", "AIAN", "NHPI", "Overall")

  return_df <- df %>%
    dplyr::mutate(!!dplyr::sym(new_race_col) := race_lookup({{race_col}}))

  race_present <- return_df %>%
    dplyr::pull(!!dplyr::sym(new_race_col))

  race_vector_order <- race_vector_order[race_vector_order %in% race_present]

  return_df <- return_df %>%
    dplyr::mutate(!!dplyr::sym(new_race_col) := factor(!!dplyr::sym(new_race_col), race_vector_order, race_vector_order))

  if (any(is.na(return_df[[new_race_col]]))){
    stop("Error; missing race values")
  }

  return_df

}

#' Overall check
#' Function to check if vector has an overall value and to sort so that's last
#'
#' @param vec Vector of values that might contain an "overall" value
#'
#' @return Ordered vector with "overall" moved to end if it's present
#' @export
#'
#' @examples
overall_check <- function(vec){

  # check if overall present in vector
  overall_check <- grepl("overall", vec, ignore.case = T)

  # reorder vector so overall last
  if (any(overall_check)){
    vec <- c(vec[!overall_check], vec[overall_check])
  }

  vec

}

#' Age recode
#' Function to recode age-values in ACS dataframe, removing extraneous text and ordering column as factor
#'
#' @param df ACS dataframe with age values
#' @param age_col Name of age column as data-masked object
#' @param regroup Whether to regroup-age values. Accepts "decennial" for decennial Census results, "race_age" for regrouping race/age column, and "sex_age" for regrouping sex/age groups.
#'
#' @return
#' @export
#'
#' @examples
age_recode <- function(df, age_col, regroup = "decennial") {

  if (!regroup %in% c("decennial", "race_age", "sex_age")){
    warning("regroup parameter not valid")
  }

  # replace unecessary characters in string
  age_sub <- function(string){
    string %>%
      gsub("Under ", "< ", .) %>%
      gsub(" to ", "-", .) %>%
      gsub(" years", "", .) %>%
      gsub(" and over", "+", .) %>%
      gsub(" and ", "-", .) %>%
      gsub(":", "", .)
  }

  # create new column with unecessary strings replaced
  df <- df %>%
    dplyr::mutate(age_new = age_sub({{ age_col }}))

  # pull vector
  age_vec_order <- dplyr::pull(df, age_new) %>%
    unique()

  # pull less than value
  less_age <- grep("<", age_vec_order, value = T)

  # pull single digits and sort
  single_dig <- grep("(^[0-9]-)|(^[0-9]$)", age_vec_order, value = T) %>%
    sort

  # reorder so 1) less than 2) signle digits 3) age vector order, and then check if overall value present and put last if so
  age_vec_order <- c(less_age, single_dig, age_vec_order[!age_vec_order %in% c(single_dig, less_age)]) %>%
    acsprocess::overall_check()

  # create factor of age vector
  df <- df %>%
    dplyr::mutate(age_new = factor(age_new, age_vec_order, age_vec_order))

  if (regroup == "decennial"){

    age_grp_vec = c(
      "0-9",
      "10-19",
      "20-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70-79",
      "80+")

    process_age_group <- function(string){
      string <- as.character(string)

      dplyr::case_when(string %in% c(less_age, single_dig) ~ age_grp_vec[1],
                       grepl("^1", string) ~ age_grp_vec[2],
                       grepl("^2", string) ~ age_grp_vec[3],
                       grepl("^3", string) ~ age_grp_vec[4],
                       grepl("^4", string) ~ age_grp_vec[5],
                       grepl("^5", string) ~ age_grp_vec[6],
                       grepl("^6", string) ~ age_grp_vec[7],
                       grepl("^7", string) ~ age_grp_vec[8],
                       !grepl("Overall", string, ignore.case = T, ) ~ age_grp_vec[9],
                       grepl("Overall",string, ignore.case = T) ~ string)

    }

    # create factor of order
    df <- df %>%
      dplyr::mutate(age_group = process_age_group(age_new),
                    age_group = factor(age_group, age_grp_vec, age_grp_vec))

  }

  else if (regroup == "race_age"){

    age_grp_vec = c(
      "0-9",
      "10-17",
      "18-24",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65-74",
      "75-84",
      "85+")

    process_age_group <- function(string){
      string <- as.character(string)

      dplyr::case_when(string %in% c(less_age, single_dig) ~ age_grp_vec[1],
                       grepl("(10)|(15)", string) ~ age_grp_vec[2],
                       grepl("(18)|(20)", string) ~ age_grp_vec[3],
                       grepl("(25)|(30)", string) ~ age_grp_vec[4],
                       grepl("35", string) ~ age_grp_vec[5],
                       grepl("^4", string) ~ age_grp_vec[6],
                       grepl("^5", string) ~ age_grp_vec[7],
                       grepl("^6", string) ~ age_grp_vec[8],
                       grepl("^7", string) ~ age_grp_vec[9],
                       !grepl("Overall", string, ignore.case = T, ) ~ age_grp_vec[10],
                       grepl("Overall",string, ignore.case = T) ~ string)

    }

    # create factor of order
    df <- df %>%
      dplyr::mutate(age_group = process_age_group(age_new),
                    age_group = factor(age_group, age_grp_vec, age_grp_vec))

  }

  else if (regroup == "sex_age"){
    age_grp_vec = c(
      "0-11",
      "12-17",
      "18-24",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65-74",
      "75+")

    process_age_group <- function(string){
      string <- as.character(string)

      dplyr::case_when(string %in% c(less_age, single_dig) ~ age_grp_vec[1],
                       grepl("^1[1-7]", string) ~ age_grp_vec[2],
                       grepl("(18)", string) ~ age_grp_vec[3],
                       grepl("^2", string) ~ age_grp_vec[4],
                       grepl("^3", string) ~ age_grp_vec[5],
                       grepl("^4", string) ~ age_grp_vec[6],
                       grepl("^5", string) ~ age_grp_vec[7],
                       grepl("^6", string) ~ age_grp_vec[8],
                       grepl("^7", string) ~ age_grp_vec[9],
                       !grepl("Overall", string, ignore.case = T, ) ~ age_grp_vec[10],
                       grepl("Overall",string, ignore.case = T) ~ string)

    }

    # create factor of order
    df <- df %>%
      dplyr::mutate(age_group = process_age_group(age_new),
                    age_group = factor(age_group, age_grp_vec, age_grp_vec))

  }


  df
}


#' Birth recode
#' Function to recode birth values in ACS dataframe containing column for place of birth
#'
#' @param df Lightly-processed ACS dataframe with birth column
#' @param birth_col Name of birth column to recode
#'
#' @return Dataframe with additional column for place of birth
#' @export
#'
#' @examples
birth_recode <- function(df, birth_col){

  birth_vec <- c("In-state",
                 "Out of state",
                 "Native: outside US",
                 "Foreign born")

  birth_change <- function(string){
    dplyr::case_when(grepl("state of residence", string) ~ birth_vec[1],
              grepl("other state", string) ~ birth_vec[2],
              grepl("Native", string) ~ birth_vec[3],
              grepl("Foreign", string) ~ birth_vec[4])
  }

  df <- df %>%
    dplyr::mutate(birth_new = birth_change({{ birth_col }}),
           birth_new = factor(birth_new, birth_vec, birth_vec))

  if (any(is.na(df[["birth_new"]]))){
    stop("error; some missing values in new birth column")
  }

  df
}


#' Recode tenure
#' Extracts tenure values from ACS dataframe
#'
#' @param df Dataframe with tenure column
#' @param tenure_col Name of tenure column
#'
#' @return Dataframe with tenure column
#' @export
#'
#' @examples
recode_tenure <- function(df, tenure_col){
  df %>%
    dplyr::mutate({{ tenure_col }} := stringr::str_extract({{ tenure_col }}, "^.*occupied"))
}


#' Recode ACS Asian countries
#' Recodes Asian country names in ACS data, shortening India/China and two or more
#'
#' @param df ACS dataframe
#' @param ethnicity_col Column to recode
#'
#' @return Dataframe with recoded Asian country names
#' @export
#'
#' @examples
asian_country_recode <- function(df, ethnicity_col){

  change_ethnicity <- function(string){
    dplyr::case_when(grepl("Indian", string) ~ "Indian",
              grepl("Chinese", string) ~ "Chinese",
              grepl("Two or ", string) ~ "Two +",
              TRUE ~ string)
  }

  df %>%
    dplyr::mutate(new_ethnicity = change_ethnicity( {{ ethnicity_col }}))
}


#' Ancestry recode
#'
#' @param df ACS dataframe with ancestry values contained in one column
#' @param ancestry_col Name of ancestry column to recode
#'
#' @return Dataframe with recoded ancestry values
#' @export
#'
#' @examples
ancestry_recode <- function(df, ancestry_col){

  ancestry_change <- function(string){
    dplyr::case_when(grepl("Arab:", string) ~ "Arab",
              grepl("French \\(except", string) ~ "French",
              grepl("Subsaharan ", string) ~ "sub-Saharan African",
              grepl("West Indian ", string) ~ "West Indian",
              grepl("Unclassified or not", string) ~ "Not reported",
              TRUE ~ string)
  }

  df %>%
    dplyr::mutate(ancestry_new = ancestry_change({{ ancestry_col }}))

}


#' Health insurance recode
#' Recodes ACS health insurance number of plans and type columns into one column, and creates factor column
#'
#' @param df ACS dataframe with type of health insurance plan column and number of plans column
#' @param num Name of 'number of plans' column
#' @param type Name of type of plans column
#'
#' @return Dataframe with health-insurance categories recoded in one column
#' @export
#'
#' @examples
health_insur_recode <- function(df, num, type){

  health_vec <- c("Employer-based",
                  "Direct-purchase",
                  "Two or more",
                  "VA/military",
                  "Medicare",
                  "Medicaid/means-tested",
                  "Medicaid and Medicare",
                  "No insurance")

  string_recode <- function(num_plans, type_coverage){

    # check whether involves has multiple plans
    multi_plans <- dplyr::case_when(grepl("one", num_plans) ~ F,
                             grepl("two", num_plans) ~ T)

    # code plans as medicaid and medicare if  includes medicaid, or code as two + plans otherwise if involves multiple plans
    multi_funct <- function(type_cov){
      dplyr::case_when(grepl("Medicaid", type_cov) ~ health_vec[7],
                TRUE ~ health_vec[3])
    }

    # run multi-plan function if involves multiple plans; otherwise, recode
    dplyr::case_when(multi_plans ~ multi_funct(type_cov = type_coverage),
              grepl("employer-based", type_coverage) ~ health_vec[1],
              grepl("direct-purchase", type_coverage) ~ health_vec[2],
              grepl("Medicare", type_coverage) ~ health_vec[5],
              grepl("Medicaid", type_coverage) ~ health_vec[6],
              grepl("(TRICARE)|(VA Health)", type_coverage) ~ health_vec[4],
              grepl("No insurance", type_coverage) ~ health_vec[8])
  }

  df <- df %>%
    dplyr::mutate(health_new := string_recode({{ num }}, {{ type }}),
           health_new = factor(health_new, health_vec, health_vec))

  if (any(is.na(df[["health_new"]]))){

    miss <- df %>%
      dplyr::filter(is.na(health_new)) %>%
      dplyr::pull({{type}}) %>%
      unique
    # browser()

    stop(glue::glue("Error; missing values in returned vector; types coverage producing missing values are {paste(miss, collapse = ',')}"))
  }

  df

}

#' Cost burden recode
#' Function to recode ACS cost burden values in one column
#'
#' @param df Dataframe with ACS cost burden values
#' @param cost_col Name of column containing cost burden categories
#'
#' @return Dataframe with recoded cost burden values
#' @export
#'
#' @examples
cost_burden_recode <- function(df, cost_col){

  string_recode <- function(string){
    string %>%
      gsub(" to ", "-", .) %>%
      gsub(" percent", "%", .) %>%
      gsub("Less than ", "< ", .) %>%
      ifelse(grepl(" or more", .), paste0("> ", .), .) %>%
      gsub(" or more", "", .) %>%
      gsub("\\.0", "", .)

  }

  # define order

  # pull cost column values
  unique_costs <- dplyr::pull(df, {{ cost_col }}) %>%
    unique

  factor_levels <- c(
    dplyr::case_when(
      any(grepl("Less than 20", unique_costs)) ~ c("< 20%"),
      any(grepl("Less than 10", unique_costs)) ~ c("< 10%",
                                                   "10-14.9%",
                                                   "15-19.9%")
    ),
    c("20-24.9%",
      "25-29.9%",
      "30-34.9%",
      "35-39.9%",
      "40-49.9%",
      "> 50%")) %>%
    rev

  df %>%
    dplyr::mutate({{ cost_col }} := string_recode({{ cost_col }})) %>%
    dplyr::mutate({{ cost_col }} := factor({{ cost_col }}, factor_levels, factor_levels))

  # burden_overall$percent_income %>% unique

}


#' Recode income/poverty
#' Recodes ACS dataframe income to poverty ratio column
#'
#' @param df Dataframe with income to poverty ratio column
#' @param var Name of income to poverty ratio column
#'
#' @return Dataframe with income to poverty ratio column recoded and sorted/factored
#' @export
#'
#' @examples
incpov_recode <- function(df, var){

  # pull unique values
  incpov_vec <- df %>%
    dplyr::pull({{ var }}) %>%
    unique %>%
    sort

  # identify under category
  incpov_first <- grep("Under", incpov_vec, value = T, ignore.case = T)

  # exclude under category from vector
  incpov_vec <- grep("Under", incpov_vec, value = T, ignore.case = T, invert = T)

  # add under category first in order
  incpov_vec <- c(incpov_first, incpov_vec)

  df %>%
    dplyr::mutate(incpov_new = factor({{ var }}, incpov_vec, incpov_vec))

}


#' Old recode
#' Function to recode ACS dataframe with age values greater than 75 to 75 and older
#'
#' @param df Dataframe with age column
#' @param age_col Name of age column
#'
#' @return Dataframe with recoded age column
#' @export
#'
#' @examples
old_recode <- function(df, age_col){
  age_old <- function(string){
    dplyr::case_when(grepl("(75)|(85)", string) ~ "75 years and over",
              T ~ string)
  }

  df %>%
    dplyr::mutate({{ age_col }} := age_old({{ age_col }}))
}



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
    dplyr::mutate(!!dplyr::sym(new_income) := change_income({{income_col}}))

  factor_vector <- dplyr::pull(df, !!dplyr::sym(new_income)) %>% unique

  factor_vector_big <- grep(">", factor_vector, value = T)
  factor_vector_small <- grep("<", factor_vector, value = T)

  factor_vector <- c(factor_vector_small,
                     factor_vector[!factor_vector %in% c(factor_vector_small, factor_vector_big)],
                     factor_vector_big)

  df <- df %>%
    mutate(!!dplyr::sym(new_income) := factor(!!dplyr::sym(new_income), factor_vector, factor_vector))

  if (rent_own){

    rent_own_replacer <- function(string){
      recode(string, "100k-149k" = "Owner: 100k-150k \n Renter: > 100k",
             "> 100k" = "Owner: 100k-150k \n Renter: > 100k")
    }

    df <- df %>%
      dplyr::mutate(!!dplyr::sym(new_income) := rent_own_replacer(!!dplyr::sym(new_income)))
  }

  df

}


#' Recode education values
#'
#' A function to recode tidycensus education columns to a shorter string for graphing purposes
#'
#' @param df A tidycensus dataframe with an education column
#' @param educ_col The column name of  to the education column in the tidycensus dataframe
#' @param new_educ The name of the new column containing the recoded education values; by edfault, educ_recode
#'
#' @return A dataframe with a recoded education column with shorter names
#' @export
#'
#' @examples
educ_recode <- function(df, educ_col, new_educ = "educ_recode"){

  vec_order <- c("Less than HS",
                 "HS graduate",
                 "Associate's/some college")

  if (any(grepl("Professional", dplyr::pull(df, {{ educ_col }}), ignore.case = T))) {
    vec_order <- c(vec_order, "Bachelor's degree", "Graduate degree")
  }

  else {
    vec_order <- c(vec_order, "Bachelor's or higher")
  }

  educ_change <- function(string){
    dplyr::case_when(grepl("Less than", string, ignore.case = T) ~ vec_order[1],
                     grepl("equivalency", string, ignore.case = T) ~ vec_order[2],
                     grepl("Some college", string, ignore.case = T) ~ vec_order[3],
                     grepl("Bachelor's", string, ignore.case = T) ~ vec_order[4],
                     grepl("Graduate", string, ignore.case = T) ~ vec_order[5],
                     TRUE ~ "Missing")
  }

  return <- df %>%
    dplyr::mutate(!!dplyr::sym(new_educ) := factor(educ_change({{educ_col}}), vec_order, vec_order))

  if (any(return[[new_educ]] == "Missing")){
    stop("Error; returned missing education values")
  }

  return(return)
}

#' Recode ACS computer/internet access variables
#'
#' Function to recode tidycensus ACS dataset with a column for computer access and a column for internet access to one column with shortened descriptors.
#'
#' @param df A tidycensus ACS dataset with a column for computer access and one for internet access.
#' @param comp_col The name of the computer access column.
#' @param int_col The name of the internet accesss column.
#' @param new_comp The name of the new column
#'
#' @return A tidycensus ACS dataset with a factor-ordered computer/internet access column
#' @export
#'
#' @examples
comp_recode <- function(df, comp_col, int_col, new_comp = "comp_int"){
  comp_order <- c("Broadband internet",
                  "Dial-up internet",
                  "No internet",
                  "No computer")

  comp_change <- function(string_comp, string_int){

    comp_int <- ifelse(!grepl("No", string_comp), paste0(string_comp, " ", string_int), comp_order[4])

    comp_int <- dplyr::case_when(grepl("Without an Internet", comp_int) ~ comp_order[3],
                                 grepl("dial-up", comp_int) ~ comp_order[2],
                                 grepl("broadband", comp_int) ~ comp_order[1],
                                 TRUE ~ comp_int)

    return(comp_int)
  }

  return <- df %>%
    dplyr::mutate(!!dplyr::sym(new_comp) := comp_change({{ comp_col }}, {{ int_col }}),
                  !!dplyr::sym(new_comp) := factor(!!dplyr::sym(new_comp), comp_order, comp_order))

  if (any(is.na(return[[new_comp]]))){
    stop("Error; missing values in return dataset")
  }

  return(return)

}

recode_hous_ppl <- function(df, col_name = hous_ppl){

  factor <- c(
    "With own children (< 18)",
    "With roommates",
    "With relatives",
    "Alone"
  )

  recode_string <- function(string){
    dplyr::case_when(
      grepl("With relatives", string) ~ factor[3],
      grepl("(no own children)|(alone)", string ~ factor[4],
            "nonrelatives"),
      grepl(" own children ", string) ~ factor[1],
      grepl("nonrelatives", string) ~ factor[2]
    )
  }

  df %>%
    dplyr::mutate({{col_name}} := factor(
      recode_string({{col_name}}),
      factor,
      factor
    ))

}

recode_houstype <- function(df, col_name = hous_type){
  recode_string <- function(string){
    process <- gsub(" householder, no (spouse or partner|spouse) present:", "", string) %>%
      gsub(":", "", .) %>%
      gsub("Male", "Single male", .) %>%
      gsub("Female", "Single female", .) %>%
      gsub(" household", "", .) %>%
      gsub("-", " ", .) %>%
      gsub(" family", "", .)

  }

  df %>%
    dplyr::mutate({{col_name}} := recode_string({{col_name}}))
}

recode_fam_occupants <- function(df, col_name = fam_occupants){
  recode_string <- function(string){
    dplyr::case_when(grepl("With related ", string) ~ "With children",
              grepl("No related", string) ~ "No children")
  }

  df %>%
    dplyr::mutate({{col_name}} := recode_string({{col_name}}))

}

recode_child_age <- function(df, col_name = occupants_age){
  factor <- c("< 5", "5-17", "< 5 and 5-17", "No children")

  recode_string <- function(string){
    dplyr::case_when(grepl("years and ", string) ~ factor[3],
                     grepl("Under 5", string) ~ factor[1],
                     grepl("5 to 17", string) ~ factor[2],
                     grepl("No children", string) ~ factor[4])
  }

  df %>%
    dplyr::mutate({{col_name}} := recode_string({{col_name}}))
}
