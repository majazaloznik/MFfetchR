% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{prepare_quarterly_eo}
\alias{prepare_quarterly_eo}
\title{Calculate and prepare the table for quarterly data for EO}
\usage{
prepare_quarterly_eo(data_frame)
}
\arguments{
\item{data_frame}{output from transform_series_eo}
}
\value{
list of three objects, the main table and two header rows.
}
\description{
This function aggregates the amounts over quarters (only for complete quarters)
then calculates the year-on-year growth rates, and the relative contributions
for each account and formats the table appropriately. It also prepares the
two header rows required for a pretty (but def not tidy) Excel presentation.
}
