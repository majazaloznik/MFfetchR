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

prepare_vintage_table <- function(file_path, table_name, sheet_name, con, schema = "platform"){
  DBI::dbExecute(con, paste("set search_path to", schema))

  parsed_data <- mf_excel_parser(file_path, table_name, sheet_name)
  new_month <- dplyr::arrange(parsed_data[[1]], period_id)  |>
    dplyr::summarise(max = max(period_id))  |>
    dplyr::pull()
  new_year <- dplyr::arrange(parsed_data[[2]], period_id)  |>
    dplyr::summarise(max = max(period_id))  |>
    dplyr::pull()
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  # get first two series last vintages if they exist. (M & A)
  series_ids <- dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::slice_min(id, n = 2) %>%
    dplyr::select(id) %>% dplyr::pull() %>%
    as.numeric()
  vint_id_m <- UMARaccessR::sql_get_vintage_from_series(con, series_ids[1], schema = schema)
  vint_id_a <- UMARaccessR::sql_get_vintage_from_series(con, series_ids[2], schema = schema)
  if(is.null(vint_id_a)){
    annual_vintages <- vintage_table("A", tbl_id, con)} else {
      # get latest period from latest vintage
      max_year <- dplyr::tbl(con, "data_points") %>%
        dplyr::filter(vintage_id == vint_id_a) %>%
        dplyr::slice_max(period_id) %>%
        dplyr::pull(period_id)
      if(identical(max_year, new_year)) {
        warning(paste0("These annual vintages for table ", table_name,
                    " are not new, they will not be inserted again."))
        annual_vintages <- NULL
      } else {
        annual_vintages <- vintage_table("A", tbl_id, con)}
    }

  if(is.null(vint_id_m)){
    monthly_vintages <- vintage_table("M", tbl_id, con)} else {
      # get latest period from latest vintage
      max_month <- dplyr::tbl(con, "data_points") %>%
        dplyr::filter(vintage_id == vint_id_m) %>%
        dplyr::slice_max(period_id) %>%
        dplyr::pull(period_id)
      if(identical(max_month, new_month)) {
        stop(paste0("These monthly vintages for table ", table_name,
                    " are not new, they will not be inserted again."))
      } else {
        monthly_vintages <- vintage_table("M", tbl_id, con)
      }
    }
  mget(c("monthly_vintages", "annual_vintages", "parsed_data"))
}

#' Prepare vintage table for M or A
#'
#' Helper function preparing for the vintage table for a specific table
#' and either the monthly or the annual data. Uses current time as `published`
#'
#' @param interval "M" or "A"
#' @param tbl_id numeric table id
#' @param con connection to the database.
#'
#' @return data frame with `series_id` and `published` columns
#' @keywords internal

vintage_table <- function(interval, tbl_id, con) {
  dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id,
                  interval_id == interval) %>%
    dplyr::select(series_id=id) %>%
    dplyr::collect() %>%
    dplyr::mutate(published = get_published_time())
}



#' Get and prepare data for import
#'
#' Prepares the timeseries data for importing into the database. Only works after
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
prepare_mf_data_for_insert <- function(parsed_data, con, schema = "platform"){
  DBI::dbExecute(con, paste("set search_path to", schema))

  # get table name
  tbl_name <- regmatches(parsed_data$monthly$code[1],
                         regexpr("(?<=--).*?(?=--)",
                                 parsed_data$monthly$code[1], perl = TRUE))
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, tbl_name, schema)
  vintage_lookup <- dplyr::tbl(con, "series") |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(series_id = id, code) |>
    dplyr::left_join(dplyr::tbl(con, "vintage"), by = "series_id") |>
    dplyr::select(series_id, code, id, published) |>
    dplyr::collect() |>
    dplyr::group_by(series_id) |>
    dplyr::slice_max(published)

  parsed_data$monthly %>%
    dplyr::left_join(vintage_lookup) %>%
    dplyr::bind_rows(
      parsed_data$annual %>%
        dplyr::left_join(vintage_lookup) %>%
        dplyr::select(-code)) %>%
    dplyr::select(-series_id, -published)
}
