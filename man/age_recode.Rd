% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_functs.R
\name{age_recode}
\alias{age_recode}
\title{Age recode
Function to recode age-values in ACS dataframe, removing extraneous text and ordering column as factor}
\usage{
age_recode(df, age_col, regroup = "decennial")
}
\arguments{
\item{df}{ACS dataframe with age values}

\item{age_col}{Name of age column as data-masked object}

\item{regroup}{Whether to regroup-age values. Accepts "decennial" for decennial Census results, "race_age" for regrouping race/age column, and "sex_age" for regrouping sex/age groups.}
}
\description{
Age recode
Function to recode age-values in ACS dataframe, removing extraneous text and ordering column as factor
}
