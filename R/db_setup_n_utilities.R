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
  return(latest)
}




#' Update konto lookup table on database when new kontos arrive
#'
#' This funciton is called in the main script under the condition that
#' new kontos have been added. it recreates the konto lookup table on the
#' database, which is used in the joins for the materialised views
#'
#' @param file_path what it says on the tin
#' @param con database connection
#'
#' @returns nothin
#' @export
#'
update_JF_lookup_table_on_db <- function(file_path, con){

  data_raw <- mf_csv_parser_new(file_path)

  konti <- data_raw$series  |>
    dplyr::group_by(konto) |>
    dplyr::summarise(konto = unique(konto),
                     description = unique(description))

  codes_to_7 <- c("901", "914", "915")
  codes_to_4 <- c("911", "912", "913", "916", "917", "918", "919", "920", "921")
  codes_to_na <- c("902", "903", "904", "905", "906", "907", "908")

  konti <- konti %>%
    dplyr::mutate(group_code = dplyr::case_when(
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
  out <- DBI::dbAppendTable(con,
                     name = DBI::Id(schema = "views", table = "JF_konto_lookup"),
                     value = konti)
  return(out)
}

#' Regex patterns for MF export file names
#'
#' Anchored to the timestamp so that ad-hoc exports such as
#' `Export_4BJF_ZZZZ_<timestamp>.csv` are not picked up.
#'
#' @export
mf_file_patterns <- c(
  bjf = "^Export_4BJF_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.csv$",
  ek  = "^Export_EK_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.csv$")

#' Read an MF tab-delimited export
#'
#' Detects the file encoding from the byte order mark (or null bytes) and
#' transcodes UTF-16 exports to UTF-8 before parsing. MF switched to UTF-16LE
#' with the September 2026 delivery.
#'
#' @param file path to the csv file
#' @param decimal_mark decimal mark passed to [readr::locale()]
#'
#' @return tibble
#' @export
read_mf_csv <- function(file, decimal_mark = ".") {
  bom <- readBin(file, "raw", n = 2L)
  enc <- if (identical(bom, as.raw(c(0xFF, 0xFE)))) "UTF-16LE" else
    if (identical(bom, as.raw(c(0xFE, 0xFF)))) "UTF-16BE" else
      if (length(bom) == 2L && bom[2] == as.raw(0)) "UTF-16LE" else "UTF-8"
  loc <- readr::locale(encoding = "UTF-8", decimal_mark = decimal_mark)
  if (enc == "UTF-8") {
    return(readr::read_delim(file, delim = "\t", locale = loc,
                             show_col_types = FALSE))
  }
  message("Detected ", enc, " encoding, transcoding to UTF-8.")
  txt <- iconv(list(readBin(file, "raw", n = file.size(file))),
               from = enc, to = "UTF-8")
  if (is.na(txt)) stop("Transcoding from ", enc, " failed: ", file)
  txt <- sub("^\ufeff", "", txt)
  readr::read_delim(I(txt), delim = "\t", locale = loc, show_col_types = FALSE)
}
