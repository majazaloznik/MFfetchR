% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_setup_n_utilities.R
\name{get_published_time}
\alias{get_published_time}
\title{Get published timestamp for database operations}
\usage{
get_published_time()
}
\value{
A POSIXct timestamp in UTC timezone
}
\description{
Returns a deterministic timestamp during testing (1900-01-01 00:00:00 UTC)
and the current system time in production. This ensures reproducible test
fixtures while maintaining correct timestamps in production.
}
\details{
The function checks if code is running under testthat by examining the
TESTTHAT environment variable. When testing, it returns a fixed timestamp
from 1900-01-01 to ensure consistent database fixtures. In production,
it returns the current system time.
}
\examples{
\dontrun{
# In production
get_published_time()
#> [1] "2025-06-01 14:30:15 UTC"

# During testing (when TESTTHAT=true)
get_published_time()
#> [1] "1900-01-01 UTC"
}
}
