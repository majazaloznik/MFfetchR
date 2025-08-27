#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that manually prepares the dimension_levels for each
#' table and get their codes and text.
#' Returns table ready to insert into the `dimension_levels`table with the
#' db_writing family of functions.
#'
#' @param file_path path to the csv file
#' @param encoding of the file
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `tab_dim_id`, `level_value` and `level_text`
#' columns for this table.
#' @export
#'
prepare_dimension_levels_table_new <- function(file_path, encoding = "UTF-16LE", table_name,
                                           con, schema = "platform") {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  dim_ids <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Konto", con, schema)

  df <- mf_csv_parser_new(file_path, encoding)$series
  message("Preparing dimension levels table for table ", table_name, ".")
  df <- df  |>
    dplyr::filter(blg == table_name) |>
    dplyr::mutate(tab_dim_id = dim_ids) |>
    dplyr::rename(code = konto) |>
    dplyr::rename(level_value = code, level_text = description) |>
    dplyr::select(-blg)

  dim_ids <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Interval", con, schema)
  df  |>
    dplyr::bind_rows(data.frame(level_value = c("M", "A"),
                                level_text = c("Mese\u010dno", "Letno"),
                                tab_dim_id = c(dim_ids, dim_ids)))
}


#' Prepare table to insert into `series` table
#'
#'
#'
#' @param file_path path to the excel file
#' @param encoding of the file
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the following columns: `name_long`, `code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export


prepare_series_table_new <- function(file_path, encoding = "UTF-16LE", table_name, con, schema = "platform"){
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Konto", con, schema)

  df <- mf_csv_parser_new(file_path, encoding)$series
  message("Preparing series table for table ", table_name, ".")

  df  |>
    dplyr::rename(level_text = description) |>
    dplyr::filter(blg == table_name) |>
    dplyr::mutate(unit_id = UMARaccessR::sql_get_unit_id_from_unit_name("eur", con, schema),
                  table_id = tbl_id,
                  order = dplyr::row_number()) |>
    dplyr::slice(rep(1:dplyr::n(), each = 2)) |>
    dplyr::mutate(interval_id = rep(c("M", "A"), dplyr::n()/2)) |>
    dplyr::rename(name_long = level_text)  |>
    dplyr::rowwise() |>
    dplyr::mutate(name_long = ifelse(interval_id == "M", paste(name_long, "-- Mese\u010dno"),
                                     paste(name_long, "-- Letno"))) |>
    dplyr::mutate(code = paste0("MF--", blg, "--", konto, "--", interval_id)) |>
    dplyr:: select(-order, -blg, -konto) |>
    dplyr::relocate(table_id, name_long, unit_id, code, interval_id)
}


#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#' @param table_name the table name (eg "DP")
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table_new <- function(table_name, con, schema = "platform") {
  message("Preparing series levels table for table ", table_name, ".")
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(is_time != TRUE) |>
    dplyr::pull(id)

  UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
    dplyr::select(table_id, id, code) |>
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz)), sep = "--") |>
    dplyr::select(series_id = id,  paste0(dimz)) |>
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") |>
    dplyr::rename(level_value = value) |>
    as.data.frame()
}

