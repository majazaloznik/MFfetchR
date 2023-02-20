#' Insert table structure data for the tables
#'
#' This is here for the record/reprodcibility, because it was only done once.
#' WHen the five tables were first added to the database, a set of nine
#' tables needed to be populated with appropriate metadata about those tables.
#' This umbrella function calls the respective SQL functions for each
#' of the nine tables and the table preparation functions to insert the data.
#'
#' @param meta is the dataframe in the package with the table names
#' @param con connection to the database.
#'
#' @return nothing
#' @export

insert_new_table_structures <- function(meta, con) {

  SURSfetchR::sql_function_call(con,
                                "insert_new_category",
                                as.list(prepare_category_table()))
  SURSfetchR::sql_function_call(con,
                                "insert_new_table",
                                as.list(prepare_table_table()))
  SURSfetchR::sql_function_call(con,
                                "insert_new_category_relationship",
                                as.list(prepare_category_relationship_table()))
  SURSfetchR::sql_function_call(con,
                                "insert_new_category_table",
                                as.list(prepare_category_table_table(con)))

  SURSfetchR::sql_function_call(con,
                                "insert_new_table_dimensions",
                                as.list(prepare_table_dimensions_table(con)))

  SURSfetchR::sql_function_call(con,
                                "insert_new_dimension_levels",
                                as.list(purrr::pmap(meta, prepare_dimension_levels_table, con) %>% purrr::list_rbind()))

  SURSfetchR::sql_function_call(con,
                                "insert_new_unit",
                                as.list(prepare_unit_table(con)))

  SURSfetchR::sql_function_call(con,
                                "insert_new_series",
                                unname(as.list(purrr::map(meta$table_name, prepare_series_table, con) %>% purrr::list_rbind())))

  SURSfetchR::sql_function_call(con,
                                "insert_new_series_levels",
                                unname(as.list(purrr::map(meta$table_name, prepare_series_levels_table, con) %>% purrr::list_rbind())))
}



#' Insert new data for a table i.e. a vintage
#'
#' When new data for a table (one of the Excel's) is added, these are new
#' vintages. This function inserts a set of new vintages and their corresponding
#' data points to the database.
#'
#' @param file_path path to excel file
#' @param table_name name of table
#' @param sheet_name name of sheet
#' @param con connection to database
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
#' }
insert_new_data <- function(file_path, table_name, sheet_name, con) {
  l <- prepare_vintage_table(file_path, table_name, sheet_name, con)
# insert monthly data
  res <- list()
  res[[1]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_vintage",
                                as.list(l$monthly_vintages))
  res[[2]] <- SURSfetchR::sql_function_call(con,
                                            "insert_new_vintage",
                                            as.list(l$annual_vintages))



  lapply(res, sum)
}
