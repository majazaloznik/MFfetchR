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
                                as.list(prepare_category_table(con)))
  SURSfetchR::sql_function_call(con,
                                "insert_new_table",
                                as.list(prepare_table_table(con)))
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
                                as.list(purrr::pmap(meta, MFfetchR::prepare_dimension_levels_table, con) %>% purrr::list_rbind()))

  SURSfetchR::sql_function_call(con,
                                "insert_new_unit",
                                as.list(prepare_unit_table(con)))

  SURSfetchR::sql_function_call(con,
                                "insert_new_series",
                                unname(as.list(purrr::pmap(meta, MFfetchR::prepare_series_table, con) %>% purrr::list_rbind())))

  SURSfetchR::sql_function_call(con,
                                "insert_new_series_levels",
                                unname(as.list(purrr::map(meta$table_name, MFfetchR::prepare_series_levels_table, con) %>% purrr::list_rbind())))
}



#' Insert new data for a table i.e. a vintage
#'
#' When new data for a table (one of the Excel's) is added, these are new
#' vintages. This function inserts a set of new vintages and their corresponding
#' data points to the database. It is possible to only have new monthly not annual
#' vintages.
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
  if(!is.null(l$annual_vintages)){
    res[[2]] <- SURSfetchR::sql_function_call(con,
                                              "insert_new_vintage",
                                              as.list(l$annual_vintages))
  }

  insert_data_points(l[[3]], con)

  lapply(res, sum)
}




#' Insert datapoints into data_point table
#'
#'
#' So, the function extracts and preps the data with \link[MFfetchR]{prepare_data_table}
#' and writes it to a temporary table in the database.
#'
#' It inserts any new periods into the period table,
#' adds the data points to the data point table.
#' @param parsed_data list with at least monthly and annual dataframes with the data_points
#' output of \link[MFfetchR]{mf_excel_parser}.
#' @param con connection to database
#'
#' @return nothing, just some printing along the way
#' @export
#'
insert_data_points <- function(parsed_data, con){
  on.exit(dbExecute(con, sprintf("drop table tmp")))
  df <- prepare_data_table(parsed_data, con)

  dbWriteTable(con,
               "tmp",
               df,
               temporary = TRUE,
               overwrite = TRUE)

  dbExecute(con, sprintf("alter table \"tmp\" add \"interval_id\" varchar"))

  # add interval_id so i can check if the periods are new and need adding
  dbExecute(con,           "
    update  \"tmp\"
    set  \"interval_id\" =  CASE WHEN (LENGTH(\"tmp\".\"period_id\") = 4.0) then 'A' else 'M' end
  ")

  # insert into period table periods that are not already in there.
  x <- dbExecute(con, sprintf("insert into %s.period
                       select distinct on (\"period_id\") \"period_id\", tmp.interval_id from tmp
                       left join %s.period on period_id = period.id
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform"),
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows inserted into the period table"))

  # insert data into main data_point table
  x <- dbExecute(con, sprintf("insert into %s.data_points
                       select id, period_id, value from tmp
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows inserted into the data_points table"))

}
