#' Prepare table to insert into `source` table

#' Function that manually prepares the new line for the source table.
#'
#' @param con connection to the database.
#' @param schema the schema to use for the connection, default is "platform"
#' @return a dataframe with the `name`, `name_long`, and `url`, columns.
#' for this table.
#' @export
prepare_source_table <- function(con, schema = "platform") {
  DBI::dbExecute(con, paste0("set search_path to ", schema))
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "MF", schema)
  if (is.null(source_id)){
    id <- dplyr::tbl(con, "source") |>
      dplyr::summarise(max = max(id, na.rm = TRUE)) |>
      dplyr::pull() + 1
    data.frame(id = id,
               name = "MF",
               name_long = "Ministrstvo za finance",
               url = "https://www.gov.si/teme/fiskalna-in-javnofinancna-politika/")} else {
                 message("MF already listed in the source table.")}
}

#' Prepare table to insert into `table` table
#'
#' Returns table ready to insert into the `table` table with the db_writing family
#' of functions.
#'
#' @param table_name character string of table code
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#' @param keep_vintage boolean whether to keep vintages
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(table_name, keep_vintage = FALSE, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "MF", schema)
  data.frame(code = table_name,
             name = meta |> dplyr::filter(table_name == !!table_name) |> dplyr::pull(file_path),
             source_id = source_id,
             url = c("https://mferac.gov.si/4bjf/sl"),
             notes = NA,
             keep_vintage = keep_vintage)
}



#' Prepare table to insert into `category` table
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and prepares the category table with field ids and
#' their names. Actually duplicates the overall category, but who gives a shit.
#'
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaulting to "platform"
#'
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(table_name, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "MF", schema)
  data.frame(id = c(0, meta |> dplyr::mutate(n = dplyr::n()) |>
                      dplyr::filter(table_name == !!table_name) |>
                      dplyr::pull(n)),
             name = c("Blagajne javnega financiranja",
                      meta |> dplyr::filter(table_name == !!table_name) |>
                        dplyr::pull(file_path)),
             source_id = rep(source_id,2))
}

#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that manually prepares the category_relationship table.
#' Returns table ready to insert into the `category` table.
#'
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `id`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(table_name, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "MF", schema)
  data.frame(id = prepare_category_table(table_name, con, schema)$id[-1],
               parent_id = 0,
               source_id = source_id)
}




#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category` table.
#'
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#'
prepare_category_table_table <- function(table_name, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "MF", schema)
  data.frame(code = table_name,
             source_id = source_id,
             category_id = meta |> dplyr::mutate(n = dplyr::n()) |>
               dplyr::filter(table_name == !!table_name) |>
               dplyr::pull(n)) |>
    dplyr::mutate(table_id =
                    UMARaccessR::sql_get_table_id_from_table_code(con, code,
                                                                  schema)) |>
    dplyr::select(-code)
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that manually prepares the table_dimensions table.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param table_name character string of table code
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#'
prepare_table_dimensions_table <- function(table_name, con, schema = "platform"){
  data.frame(code = rep(table_name, 2))  |>
    dplyr::rowwise()  |>
    dplyr::mutate(table_id = UMARaccessR::sql_get_table_id_from_table_code(
      con, code, schema)) |>
    dplyr::ungroup()  |>
    dplyr::mutate(dimension = c("Konto", "Interval"),
                  is_time = rep(0,2)) |>
    dplyr::select(-code)
}


#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that manually prepares the dimension_levels for each
#' table and get their codes and text.
#' Returns table ready to insert into the `dimension_levels`table with the
#' db_writing family of functions.
#'
#' @param file_path path to the excel file
#' @param table_name character string of table code
#' @param sheet_name character string of sheet name with table in excel.
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `dimension_id`, `values` and `valueTexts`
#' columns for this table.
#' @export
#'
prepare_dimension_levels_table <- function(file_path, table_name,
                                           sheet_name, con,
                                           schema = "platform") {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  dim_ids <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Konto", con, schema)

  df <- mf_excel_parser(file_path, table_name, sheet_name)$series
  df <- df  |>
    dplyr::mutate(tab_dim_id = dim_ids) |>
    dplyr::mutate(code = sub(".*[--]", "", code)) |>
    dplyr::rename(level_value = code, level_text = description)

  dim_ids <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Interval", con, schema)
  df  |>
    dplyr::bind_rows(data.frame(level_value = c("M", "A"),
              level_text = c("Mese\u010dno", "Letno"),
              tab_dim_id = c(dim_ids, dim_ids)))
}


#' Prepare table to insert into `units` table
#'
#' Helper function that manually prepares the unit row.
#' Returns table ready to insert into the `units`table with the
#' db_writing family of functions.
#'
#' @param con connection to the database
#' @return a dataframe with the single column containing the different units used
#' in this table.
#' @export
#'
prepare_unit_table <- function(con) {
  data.frame(name = "eur")
}



#' Prepare table to insert into `series` table
#'
#'
#'
#' @param file_path path to the excel file
#' @param table_name character string of table code
#' @param sheet_name character string of sheet name with table in excel.
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the following columns: `name_long`, `code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export


prepare_series_table <- function(file_path, table_name, sheet_name, con, schema = "platform"){
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, table_name, schema)
  dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Konto", con, schema)

  df <- mf_excel_parser(file_path, table_name, sheet_name)$series

  df  |>
    dplyr::rename(level_text = description) |>
    dplyr::mutate(unit_id = UMARaccessR::sql_get_unit_id_from_unit_name("eur", con, schema),
                  table_id = tbl_id,
                  order = dplyr::row_number()) |>
    dplyr::slice(rep(1:dplyr::n(), each = 2)) |>
    dplyr::mutate(interval_id = rep(c("M", "A"), dplyr::n()/2)) |>
    dplyr::rename(name_long = level_text)  |>
    dplyr::rowwise() |>
    dplyr::mutate(name_long = ifelse(interval_id == "M", paste(name_long, "-- Mese\u010dno"),
                              paste(name_long, "-- Letno"))) |>
    dplyr::mutate(code = paste0(code, "--", interval_id)) |>
    dplyr:: select(-order) |>
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
#'
#' @param table_name the table name (eg "DP")
#' @param con connection to the database
#' @param schema schema name defaults to "platform"
#'
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(table_name, con, schema = "platform") {
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

