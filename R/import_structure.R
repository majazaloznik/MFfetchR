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
      dplyr::mutate(table_id = as.numeric(UMARaccessR::get_table_id_from_table_code(code, con)[1,1])) %>%
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
    dplyr::mutate(table_id = as.numeric(UMARaccessR::get_table_id_from_table_code(code, con)[1,1])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dimension = rep(c("Konto", "Interval"), each = 5),
                  is_time = rep(0,10)) %>%
    dplyr::select(-code) %>%
    dplyr::arrange(table_id)
}

