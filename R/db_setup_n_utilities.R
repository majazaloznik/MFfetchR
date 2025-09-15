#' Get published timestamp for database operations
#'
#' Returns a deterministic timestamp during testing (1900-01-01 00:00:00 UTC)
#' and the current system time in production. This ensures reproducible test
#' fixtures while maintaining correct timestamps in production.
#'
#' @details
#' The function checks if code is running under testthat by examining the
#' TESTTHAT environment variable. When testing, it returns a fixed timestamp
#' from 1900-01-01 to ensure consistent database fixtures. In production,
#' it returns the current system time.
#'
#' @return A POSIXct timestamp in UTC timezone
#' @export
#'
#' @examples
#' \dontrun{
#' # In production
#' get_published_time()
#' #> [1] "2025-06-01 14:30:15 UTC"
#'
#' # During testing (when TESTTHAT=true)
#' get_published_time()
#' #> [1] "1900-01-01 UTC"
#' }
get_published_time <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  } else {
    Sys.time()
  }
}



#' Get most recent file from regex pattern
#'
#' @param folder folder to search
#' @param pattern regex pattern to match
#'
#' @returns full name of file
#' @export
#'

get_most_recent_file_from_pattern <- function(folder, pattern){
  files <- list.files(folder, pattern = pattern,
                      full.names = TRUE)

  if(length(files) == 0) stop("No  files found")
  # Get most recent
  latest <- files[which.max(file.mtime(files))]
}




#' Update konto lookup table on database when new kontos arrive
#'
#' This funciton is called in the main script under the condition that
#' new kontos have been added. it recreates the konto lookup table on the
#' database, which is used in the joins for the materialised views
#'
#' @param file_path what it says on the tin
#'
#' @returns nothin
#' @export
#'
update_JF_lookup_table_on_db <- function(file_path){

  data_raw <- mf_csv_parser_new(file_path)

  konti <- data_raw$series  |>
    dplyr::group_by(konto) |>
    dplyr::summarise(konto = unique(konto),
                     description = unique(description))

  codes_to_7 <- c("901", "914", "915")
  codes_to_4 <- c("911", "912", "913", "916", "917", "918", "919", "920", "921")
  codes_to_na <- c("902", "903", "904", "905", "906", "907", "908")

  konti <- konti %>%
    dplyr::mutate(group_code = case_when(
      stringr::str_starts(konto, "44") ~ NA,
      stringr::str_starts(konto, "75") ~ NA,
      stringr::str_starts(konto, "50") ~ NA,
      stringr::str_starts(konto, "55") ~ NA,
      konto %in% codes_to_7 ~ "7",
      konto %in% codes_to_4 ~ "4",
      konto %in% codes_to_na ~ NA,
      TRUE ~ stringr::str_sub(konto, 1, 1)
    ))

  # Write to database
  # Clear and repopulate without dropping
  DBI::dbExecute(con, "DELETE FROM views.\"JF_konto_lookup\"")
  DBI::dbAppendTable(con,
                     name = DBI::Id(schema = "views", table = "JF_konto_lookup"),
                     value = konti)
}
