#' Prepare table to insert into `vintage` table
#'
#' Helper function that prepares the vintage table with the new vintages.
#' First checks if new vintages are even necessary, by parsing the excel file
#' to get the most recent month, (and year?) and then checking with the most
#' recent period in the database. If there is new data,
#' Prepare a dataframe with the `series_id`s of the table and the current time
#' as their publication date - since we don't have anything better.
#'
#' Returns table ready to insert into the `vintage`table with the
#' db_writing family of functions.
#'
#' @param file_path path to excel file
#' @param table_name name of table
#' @param sheet_name name of sheet
#' @param con connection to database
#' @param schema schema name defaults to "platform"
#'
#' @return list with a dataframe with the `series_id` and `published` columns
#' for all monthly and annual series in this table. and the parsed data.
#' @export
#'

prepare_vintage_table_and_merge_data_points <- function(file_path, table_name, con, schema = "platform"){
  DBI::dbExecute(con, paste("set search_path to", schema))

  parsed_data <- mf_csv_parser_new(file_path)
  # keep only series and data for table_name
  parsed_data$monthly <- parsed_data$monthly |> dplyr::filter(stringr::str_extract(
    code, "(?<=--)[^-]+(?=--)") == table_name)

  parsed_data$annual <- parsed_data$annual |> dplyr::filter(stringr::str_extract(
    code, "(?<=--)[^-]+(?=--)") == table_name)

  # add series_id to monthly and annual data
  # get series_id for monthly codes
  monthly_codes <- data.frame(code = unique(parsed_data$monthly$code)) |>
    dplyr::mutate(series_id = UMARaccessR::sql_get_series_id_from_series_code(
      code, con, schema = schema))
  # join with parsed data
  parsed_data$monthly <- parsed_data$monthly |>
    dplyr::left_join(monthly_codes, by = "code")
  # get series_id for annual codes
  annual_codes <- data.frame(code = unique(parsed_data$annual$code)) |>
    dplyr::mutate(series_id = UMARaccessR::sql_get_series_id_from_series_code(
      code, con, schema = schema))
  # join with parsed data
  parsed_data$annual <- parsed_data$annual |>
    dplyr::left_join(annual_codes, by = "code") |>
    dplyr::mutate(period_id = as.character(period_id))

  ## new version test:
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  old_data <- UMARaccessR::sql_get_data_points_full_from_table_id(tbl_id, con, schema)

  merged_data <- old_data |>
    dplyr::full_join(dplyr::bind_rows(parsed_data$monthly, parsed_data$annual), by = c("period_id", "series_id", "code"))
  # overwrite old data with new
  final <- merged_data |>
    dplyr::mutate(value = ifelse(!is.na(value.y), value.y, value.x)) |>
    dplyr::group_by(series_id) |>
    dplyr::mutate(valid_old = sum(!is.na(dplyr::last(value.x))),
                  valid_new = sum(!is.na(dplyr::last(value.y)))) |>
    dplyr::filter(valid_old < valid_new) |>
    dplyr::select(series_id, period_id, value) |>
    dplyr::ungroup()

  annual_vintages <- final |>
    dplyr::filter(grepl("[0-9]{4}$", period_id)) |>
    dplyr::group_by(series_id) |>
    dplyr::summarise(series_id = unique(series_id)) |>
    dplyr::mutate(published = get_published_time())

  monthly_vintages <- final |>
    dplyr::filter(grepl("[0-9]{4}M[0-9]{2}$", period_id)) |>
    dplyr::group_by(series_id) |>
    dplyr::summarise(series_id = unique(series_id)) |>
    dplyr::mutate(published = get_published_time())
  # # check if there are any differences
  # x <- merged_data |>
  #   tidyr::drop_na() |>
  #   dplyr::mutate(diff = value.x-value.y)  |>
  #   dplyr::filter(diff > 0.1 | diff < -0.1,
  #                 period_id < "2025M01")

  # # figure out new month and year
  # new_month <- max(parsed_data$monthly$period_id)
  # new_year <- max(parsed_data$annual$period_id)
  #
  # # get first two series last vintages if they exist. (M & A)
  # series_ids <- dplyr::tbl(con, "series") %>%
  #   dplyr::filter(table_id == tbl_id) %>%
  #   dplyr::slice_min(id, n = 2) %>%
  #   dplyr::select(id) %>% dplyr::pull() %>%
  #   as.numeric()
  # vint_id_m <- UMARaccessR::sql_get_vintage_from_series(con, series_ids[1], schema = schema)
  # vint_id_a <- UMARaccessR::sql_get_vintage_from_series(con, series_ids[2], schema = schema)
  #
  # old_data <- UMARaccessR::sql_get_data_points_full_from_table_id(tbl_id, con, schema)
  #
  # if(is.null(vint_id_a)){
  #   annual_vintages <- vintage_table("A", tbl_id, con)} else {
  #     # get latest period from latest vintage
  #     max_year <- UMARaccessR::sql_get_last_period_from_vintage(con, vint_id_a)
  #     if(identical(max_year, new_year)) {
  #       warning(paste0("These annual vintages for table ", table_name,
  #                      " are not new, they will not be inserted again."))
  #       annual_vintages <- NULL
  #       parsed_data$annual <- NULL
  #       old_data <- old_data |>
  #         dplyr::filter(grepl("M$", code)) # keep only monthly data
  #     } else {
  #       annual_vintages <- vintage_table("A", tbl_id, con)}
  #   }
  #
  # if(is.null(vint_id_m)){
  #   monthly_vintages <- vintage_table("M", tbl_id, con)} else {
  #     # get latest period from latest vintage
  #     max_month <- UMARaccessR::sql_get_last_period_from_vintage(con, vint_id_m)
  #     if(identical(max_month, new_month)) {
  #       stop(paste0("These monthly vintages for table ", table_name,
  #                   " are not new, they will not be inserted again."))
  #       monthly_vintages <- NULL
  #       parsed_data$monthly <- NULL
  #       old_data <- old_data |>
  #         dplyr::filter(grepl("A$", code)) # keep only annual data
  #     } else {
  #       monthly_vintages <- vintage_table("M", tbl_id, con)
  #     }
  #   }
  #
  # # merge old data with new data
  #
  # merged_data <- old_data |>
  #   dplyr::full_join(dplyr::bind_rows(parsed_data$monthly, parsed_data$annual), by = c("period_id", "series_id", "code"))
  # # overwrite old data with new
  # final <- merged_data |>
  #   dplyr::mutate(value = ifelse(!is.na(value.y), value.y, value.x)) |>
  #   dplyr::select(series_id, period_id, value)
  # # check if there are any differences
  # x <- merged_data |>
  #   tidyr::drop_na() |>
  #   dplyr::mutate(diff = value.x-value.y)  |>
  #   dplyr::filter(diff > 0.1 | diff < -0.1,
  #                 period_id < "2025M01")



  mget(c("monthly_vintages", "annual_vintages", "final"))
}


#'
#' #' Prepare vintage table for M or A
#' #'
#' #' Helper function preparing for the vintage table for a specific table
#' #' and either the monthly or the annual data. Uses current time as `published`
#' #'
#' #' @param interval "M" or "A"
#' #' @param tbl_id numeric table id
#' #' @param con connection to the database.
#' #'
#' #' @return data frame with `series_id` and `published` columns
#' #' @keywords internal
#'
#' vintage_table <- function(interval, tbl_id, con) {
#'   dplyr::tbl(con, "series") %>%
#'     dplyr::filter(table_id == tbl_id,
#'                   interval_id == interval) %>%
#'     dplyr::select(series_id=id) %>%
#'     dplyr::collect() %>%
#'     dplyr::mutate(published = get_published_time())
#' }
#'
#'
#'
#' Get and prepare data for import
#'
#' Prepares the timeseries data for importing into the database. Only works after
#' vintages have been imported
#'
#'
#' @param con connection to database
#' @param parsed_data list with at least monthly and annual dataframes with the data_points
#' output of \link[MFfetchR]{mf_excel_parser}.
#' @param schema schema name defaults to "parameter"
#'
#' @return a dataframe with the period_id, value and id values for all the vintages in the table.
#'
#' @export
prepare_mf_data_for_insert_new <- function(final_data, con, schema = "platform"){
  DBI::dbExecute(con, paste("set search_path to", schema))

  # get table name
  tbl_id <- UMARaccessR::sql_get_table_id_form_series_id(final_data$series_id[1], con, schema)
  # get freshly inserted vintage ids
  vintage_lookup <- UMARaccessR::sql_get_latest_vintages_for_table_id(tbl_id, con, schema)

  final_data |>
    dplyr::left_join(vintage_lookup) |>
    dplyr::select(-series_id, -published, - code) |>
    dplyr::rename(id =vintage_id)
}
