% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode_functs.R
\name{income_recode}
\alias{income_recode}
\title{Recode income values}
\usage{
income_recode(df, income_col, new_income = "income_recode", rent_own = F)
}
\arguments{
\item{df}{A tidycensus downloaded dataset with an income column}

\item{income_col}{The name of the column in the tidycensus dataset containing original income values}

\item{new_income}{The name of the new, factorized column with shortened income values}

\item{rent_own}{Whether to revalue the 100k income range to be inclusive of different income ranges for owners and renters (i.e., if you want to compare renter and owner incomes).}
}
\description{
Creates shortened names for incomes in tidycensus downloaded ACS data
}
