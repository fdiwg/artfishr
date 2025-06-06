% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/artfish_new_flouca.R
\name{artfish_flouca_by_period}
\alias{artfish_flouca_by_period}
\title{artfish_flouca_by_period}
\usage{
artfish_flouca_by_period(
  year = NULL,
  month = NULL,
  active_vessels,
  effort,
  effort_source = c("survey", "registry"),
  active_days = NULL,
  landings,
  minor_strata = NULL,
  validate = TRUE
)
}
\arguments{
\item{active_vessels}{active vessels}

\item{effort}{effort}

\item{effort_source}{effort source whether it's derived from survey -B1- (fishers interviews) 
or registry -B2- (boat counting)}

\item{active_days}{active days}

\item{landings}{landings}

\item{minor_strata}{minor_strata (to investigate further later)}
}
\value{
the result of Artfish for a given year/month
}
\description{
artfish_flouca_by_period
}
