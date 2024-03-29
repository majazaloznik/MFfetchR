% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_writing.R
\name{insert_new_data}
\alias{insert_new_data}
\title{Insert new data for a table i.e. a vintage}
\usage{
insert_new_data(file_path, table_name, sheet_name, con)
}
\arguments{
\item{file_path}{path to excel file}

\item{table_name}{name of table}

\item{sheet_name}{name of sheet}

\item{con}{connection to database}
}
\value{
list of tables with counts for each inserted row.
}
\description{
When new data for a table (one of the Excel's) is added, these are new
vintages. This function inserts a set of new vintages and their corresponding
data points to the database. It is possible to only have new monthly not annual
vintages.
}
\examples{
\dontrun{
purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
}
}
