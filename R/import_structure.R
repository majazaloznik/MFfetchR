#' Prepare table to insert into `table` table
#'
#' Helper function that manually prepares the table table.
#' Returns table ready to insert into the `category` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("MF", con)[1,1]
  data.frame(code = c("DP", "ZPIZ", "ZZZS", "OB", "KBJF"),
             name = c("Dr\u017eavni prora\u010dun",
                       "Zavod za pokojninsko in invalidsko zavarovanje Slovenije",
                       "Zavod za zdravstveno zavarovanje Slovenije",
                       "Prora\u010duni ob\u010din",
                       "Konsolidirana bilanca javnega financiranja"),
             source_id = rep(source_id, 5),
             url = c("https://www.gov.si/podrocja/finance-in-davki/drzavni-proracun/",
                       "https://www.gov.si/assets/ministrstva/MF/ekonomska-in-fiskalna-poltika/Blagajne-JF/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2022.xlsx",
                       "https://www.gov.si/assets/ministrstva/MF/ekonomska-in-fiskalna-poltika/Blagajne-JF/Zavod_za_zdravstveno_zavarovanje_Slovenije_1992-2022.xlsx",
                       "https://www.gov.si/assets/ministrstva/MF/ekonomska-in-fiskalna-poltika/Blagajne-JF/Bilance_proracunov_obcin_1992-2022.xlsx",
                       "https://www.gov.si/assets/ministrstva/MF/ekonomska-in-fiskalna-poltika/Blagajne-JF/Konsolidirana_bilanca_javnega_financiranja_1992-2022.xlsx"),
             notes = rep(NA, 5)
  )
}


#' Prepare table to insert into `category` table
#'
#' Helper function that manually prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#'
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(con) {
    source_id <- UMARaccessR::get_source_code_from_source_name("MF", con)[1,1]
  data.frame(id = c(0:5),
             name = c("Blagajne javnega financiranja",
                      "Dr\u017eavni prora\u010dun",
                      "Zavod za pokojninsko in invalidsko zavarovanje Slovenije",
                      "Zavod za zdravstveno zavarovanje Slovenije",
                      "Prora\u010duni ob\u010din",
                      "Konsolidirana bilanca javnega financiranja"),
             source_id = rep(source_id,6))
}

#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that manually prepares the category_relationship table.
#' Returns table ready to insert into the `category` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#'
#' @return a dataframe with the `id`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("MF", con)[1,1]
  data.frame(id = prepare_category_table(con)$id[-1],
               parent_id =rep(0,5),
               source_id = rep(source_id,5))
}




#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#' A single table can have multiple parents - meaning
#' it is member of several categories (usually no more than two tho). .
#'
#'
#' @param con connection to the database
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#'
prepare_category_table_table <- function(con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("MF", con)[1,1]

    data.frame(code = c("DP", "ZPIZ", "ZZZS", "OB", "KBJF"),
                     category_id = 1:5,
                     source_id = rep(source_id,5)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(table_id = UMARaccessR::get_table_id_from_table_code(code, con)) %>%
      dplyr::select(-code)
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that manually prepares the table_dimensions table.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param con connection to the database
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#'
prepare_table_dimensions_table <- function(con){
  data.frame(code = rep(c("DP", "ZPIZ", "ZZZS", "OB", "KBJF"), 2)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(table_id = UMARaccessR::get_table_id_from_table_code(code, con)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dimension = rep(c("Konto", "Interval"), each = 5),
                  is_time = rep(0,10)) %>%
    dplyr::select(-code) %>%
    dplyr::arrange(table_id)
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
#' @return a dataframe with the `dimension_id`, `values` and `valueTexts`
#' columns for this table.
#' @export
#'
prepare_dimension_levels_table <- function(file_path, table_name, sheet_name, con) {
  table_id <- UMARaccessR::get_table_id_from_table_code(table_name, con)
  dim_id <- UMARaccessR::get_dim_id_from_table_id(table_id, "Konto", con)
  df <- mf_excel_parser(file_path, table_name, sheet_name)[[3]]
  df %>%
    dplyr::mutate(tab_dim_id = dim_id) %>%
    dplyr::rename(level_value = code, level_text = description)
  dim_id <- UMARaccessR::get_dim_id_from_table_id(table_id, "Interval", con)
  df %>%
    dplyr::bind_rows(data.frame(level_value = c("M", "A"),
              level_text = c("Mese\u010dno", "Letno"),
              tab_dim_id = c(dim_id, dim_id)))
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
#' @param table_name character string e.g. "DP"
#' @param con connection to the database
#'
#' @return a dataframe with the following columns: `name_long`, `code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export

prepare_series_table <- function(table_name, con){
  tbl_id <- UMARaccessR::get_table_id_from_table_code(table_name, con)
  dim_id <- UMARaccessR::get_dim_id_from_table_id(tbl_id, "Konto", con)

  dplyr::tbl(con, "dimension_levels") %>%
    dplyr::filter(tab_dim_id == dim_id) %>%
    dplyr::collect() %>%
    dplyr::mutate(unit_id = UMARaccessR::get_unit_id_from_unit_name("eur", con),
                  table_id = tbl_id) %>%
    dplyr::slice(rep(1:dplyr::n(), each = 2)) %>%
    dplyr::mutate(interval_id = rep(c("M", "A"), dplyr::n()/2)) %>%
    dplyr::rename(name_long = level_text)  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name_long = ifelse(interval_id == "M", paste(name_long, "-- Mese\u010dno"),
                              paste(name_long, "-- Letno"))) %>%
    dplyr::mutate(code = paste0("MF--", table_name, "--", level_value, "--", interval_id)) %>%
    dplyr:: select(-tab_dim_id, -level_value) %>%
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
#' @param code_no the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(code_no, con) {
  tbl_id <- UMARaccessR::get_table_id_from_table_code(table_name, con)[1,1]

  SURSfetchR:::get_table_id("DP", con)
  dplyr::tbl(con, "table_dimensions") %>%
    dplyr::filter(table_id == tbl_id,
                  is_time != TRUE) %>%
    dplyr::pull(id) -> dimz

  dplyr::tbl(con, "series") %>%
    dplyr::filter(table_id == tbl_id) %>%
    dplyr::collect() %>%
    dplyr::select(table_id, id, code) %>%
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "x3"), sep = "--") %>%
    dplyr::select(series_id = id,  paste0(dimz)) %>%
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") %>%
    as.data.frame()
}

