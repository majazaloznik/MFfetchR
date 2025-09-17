#' Get current list of valid konto numbers from newest EK file
#'
#' @param folder folder path of the EK file
#' @param file file for testing
#'
#' @returns table of kontos
#' @export

get_konto_list_full <- function(folder, file = NULL){
  if (is.null(file)){
    file <- get_most_recent_file_from_pattern(folder,"^Export_EK.*\\.csv$")}
  konto_raw <- readr::read_delim(file,
                                 delim = "\t",
                                 locale = readr::locale(encoding = "UTF-8"))
  konto_6 <- konto_raw |>
    dplyr::select(K6_ID, K6_NAME) |>
    dplyr::distinct() |>
    dplyr::rename(konto = K6_ID, description = K6_NAME)

  konto_4 <- konto_raw |>
    dplyr::select(K4_ID, K4_NAME) |>
    dplyr::distinct() |>
    dplyr::rename(konto = K4_ID, description = K4_NAME)

  konto_3 <- konto_raw |>
    dplyr::select(K3_ID, K3_NAME) |>
    dplyr::distinct() |>
    dplyr::rename(konto = K3_ID, description = K3_NAME)

  konto_2 <- konto_raw |>
    dplyr::select(K2_ID, K2_NAME) |>
    dplyr::distinct() |>
    dplyr::rename(konto = K2_ID, description = K2_NAME)

  konto_1 <- dplyr::tribble(
    ~konto, ~description,
    "4", "II. SKUPAJ ODHODKI (40+41+42+43+45)",
    "7", "I. SKUPAJ PRIHODKI (70+71+72+73+74+78)")

  konto_lookup <- dplyr::bind_rows(konto_1,
                            konto_2,
                            konto_3,
                            konto_4,
                            konto_6,
                            calculated_series_lookup) |>
    dplyr::arrange(konto)
  return(konto_lookup)
}

#' Get current list of delivered konto numbers from newest 4BJF file
#'
#' @param folder folder path of the 4BJF file
#' @param file file for testing
#'
#' @returns table of kontos
#' @export

get_konto_list_data <- function(folder, file = NULL){
  if (is.null(file)){
    file <- get_most_recent_file_from_pattern(folder,"^Export_4BJF.*\\.csv$")}

  konto_raw <- readr::read_delim(file,
                                 delim = "\t",
                                 locale = readr::locale(encoding = "UTF-8"))
  konto_6 <- konto_raw |>
    dplyr::select(K6_ID) |>
    dplyr::distinct() |>
    dplyr::rename(konto = K6_ID)

  konto_4 <- konto_6 |>
    dplyr::mutate(konto = substr(sprintf("%.0f", konto), 1,4)) |>
    dplyr::distinct()

  konto_3 <- konto_6 |>
    dplyr::mutate(konto = substr(sprintf("%.0f", konto), 1,3)) |>
    dplyr::distinct()

  konto_2 <- konto_6 |>
    dplyr::mutate(konto = substr(sprintf("%.0f", konto), 1,2)) |>
    dplyr::distinct()

  konto_1 <- konto_6 |>
    dplyr::mutate(konto = substr(sprintf("%.0f", konto), 1,1)) |>
    dplyr::distinct() |>
    dplyr::filter(konto != 5)

  konto_list <- dplyr::bind_rows(konto_1,
                                   konto_2,
                                   konto_3,
                                   konto_4,
                                   konto_6|>
                                   dplyr::mutate(konto = sprintf("%.0f", konto)),
                                   calculated_series_lookup |> dplyr::select(konto)|>
                                   dplyr::mutate(konto = sprintf("%.0f", as.numeric(konto)))) |>
    dplyr::arrange(konto)
  return(konto_list)
}

#' Get current list of konto numbers in database
#'
#' @param con database connection
#'
#' @returns table of kontos
#' @export

get_db_konto_list <- function(con){
  if (identical(Sys.getenv("TESTTHAT"), "true")) {idz <- c(179:183)} else {
    idz <- c(296:300)}
     levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(idz, con) |>
    dplyr::filter(dimension == "Konto") |>
    dplyr::select(level_value) |>
    dplyr::distinct()
return(levels)
}


#' Check for new kontos
#'
#' @param folder folder path of the 4BJF file
#' @param file file for testing
#' @param con database connection
#'
#' @returns logical for new kotnos
#' @export
#'
check_for_extra_kontos <- function(folder, file = NULL, con){
  message("Checking if there are any new kontos.")
  data_kontos <- get_konto_list_data(folder, file)
  db_kontos <- get_db_konto_list(con)
  diff <- dplyr::anti_join(data_kontos, db_kontos, by = c("konto"="level_value" ))
  check <- nrow(diff) != 0
  if(check)   message("Found new kontos in data, need tu update structure in database.")
  return(check)
}

