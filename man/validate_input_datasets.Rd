% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/artfish_data_validators.R
\name{validate_input_datasets}
\alias{validate_input_datasets}
\title{Validate input datasets}
\usage{
validate_input_datasets(
  active_vessels,
  effort,
  effort_source = c("fisher_interview", "boat_counting"),
  active_days = NULL,
  landings
)
}
\arguments{
\item{active_vessels}{active vessels}

\item{effort}{effort}

\item{effort_source}{effort source whether it's derived from -B1- (fishers interviews) 
or -B2- (boat counting)}

\item{active_days}{active days}

\item{landings}{landings}
}
\description{
Validate input datasets
}
