#' Import structural metadata for a MF table new
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. It uses the functions from the UMARimportR package to
#' insert the data into the database. But is used only for adding new
#' series to the existing database from the new formatted csv files.
#'
#' @param file_path file path to data file
#' @param table_name the table name (eg "DP")
#' @param encoding what it says on the tin
#' @param con connection to database
#' @param schema schema name, defaults to "platform"
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
MF_import_structure_new <- function(file_path, encoding = "UTF-16LE", table_name, con, schema = "platform",
                                    keep_vintage = FALSE) {

  message("Importing structure data table ", table_name, " into schema ", schema)
  # Create list to store all results
  insert_results <- list()
   # prepare and select dimension levels before inserting them
  dimension_levels_table <- prepare_dimension_levels_table_new(file_path, encoding,
                                                               table_name, con, schema)
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table_new(file_path, encoding, table_name, con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table_new(table_name, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  insert_results <- lapply(insert_results, sum)
  invisible(insert_results)
}



#' Insert new data for a table i.e. a vintage
#'
#' When new data for a table (one of the Excel's) is added, these are new
#' vintages. This function inserts a set of new vintages and their corresponding
#' data points to the database. It is possible to only have new monthly not annual
#' vintages.
#'
#' @param file_path path to excel file
#' @param encoding what it says on the tin
#' @param table_name name of table
#' @param con connection to database
#' @param schema Schema name
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
#' }
MF_import_data_points_new <- function(file_path, encoding = "UTF-16LE", table_name, con,  schema = "platform") {
  l <- prepare_vintage_table_and_merge_data_points(file_path, encoding, table_name, con, schema)
  # insert monthly data
  res <- list()
  res[[1]] <- UMARimportR::sql_function_call(con,
                                             "insert_new_vintage",
                                             as.list(l$monthly_vintages))
  message("Monthly vintages inserted: ", sum(res[[1]]), " rows")

  if(!is.null(l$annual_vintages)){
    res[[2]] <- UMARimportR::sql_function_call(con,
                                               "insert_new_vintage",
                                               as.list(l$annual_vintages))
    message("Annual vintages inserted: ", sum(res[[2]]), " rows")
  }

  insert_data_points_new(l[[3]], con)
  lapply(res, sum)
}




#' Insert datapoints into data_point table
#'
#'
#' So, the function extracts and preps the data with \link[MFfetchR]{prepare_mf_data_for_insert_new}
#' and writes it to a temporary table in the database.
#'
#' It inserts any new periods into the period table,
#' adds the data points to the data point table.
#' @param final_data list with at least monthly and annual dataframes with the data_points
#' output of \link[MFfetchR]{prepare_vintage_table_and_merge_data_points}.
#' @param con connection to database
#' @param schema schema name defaults to "platform"
#'
#' @return nothing, just some printing along the way
#' @export
#'
insert_data_points_new <- function(final_data, con, schema = "platform"){
  on.exit(dbExecute(con, sprintf("drop table tmp")))
  df <- prepare_mf_data_for_insert_new(final_data, con)

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
                              dbQuoteIdentifier(con, schema),
                              dbQuoteIdentifier(con, schema)))
  print(paste(x, "new rows inserted into the period table"))

  # insert data into main data_point table
  x <- dbExecute(con, sprintf("insert into %s.data_points
                       select id, period_id, value from tmp
                       on conflict do nothing",
                              dbQuoteIdentifier(con, schema)))
  print(paste(x, "new rows inserted into the data_points table"))

}

