% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_functions.R
\name{process_age_un18over65}
\alias{process_age_un18over65}
\title{Process sex/age to under 18 and over 65}
\usage{
process_age_un18over65(df_agesex)
}
\arguments{
\item{df_agesex}{tidycensus downloaded/unprocessed age/sex dataframe}
}
\value{
Processed dataframe with age recoded to under 18, between 18 and 65, and 65+
}
\description{
Recodes age/sex data to produce a dataset showing the total people under age 18, between 18 and 65, and 65+
}
