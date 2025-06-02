#' Import structural metadata for a BS table
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. It uses the functions from the UMARimportR package to
#' insert the data into the database.
#'
#' @param table_name the table name (eg "DP")
#' @param con connection to database
#' @param schema schema name, defaults to "platform"
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#' @param data_path path to where the data is.
#'
#' @returns nothing
#' @export
#'
MF_import_structure_old <- function(table_name, con, schema = "platform",
                                keep_vintage = FALSE, data_path) {
  message("Importing structure data table ", table_name, " into schema ", schema)
  file_path <- paste0(data_path, "/", meta |>
                        dplyr::filter(table_name == !!table_name) |>
                        dplyr::pull(file_path),
                      "_1992-2025.xlsx")
  sheet_name <- meta |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(sheet_name)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(table_name, keep_vintage, con, schema)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert for table ", table_name, ": ", insert_results$table$count, " rows")
  # preapre and insert category table
  category_table <- prepare_category_table(table_name, con, schema)
  insert_results$category <- UMARimportR::insert_new_category(con, category_table, schema)
  message("Category insert: ", insert_results$category$count, " rows")
  # prepare and insert category relationship table
  category_relationship_table <- prepare_category_relationship_table(table_name, con, schema)
  insert_results$category_relationship <- UMARimportR::insert_new_category_relationship(
    con, category_relationship_table, schema)
  message("Category relationship insert: ", insert_results$category_relantionship$count, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(table_name, con, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(table_name, con, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table <- prepare_dimension_levels_table(file_path, table_name,
    sheet_name, con, schema )
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert unit table
  unit_table <-  prepare_unit_table(table_name)
  insert_results$units <- UMARimportR::insert_new_unit(con, unit_table, schema)
  message("Units insert: ", insert_results$units$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(file_path, table_name, sheet_name, con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(table_name, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}



#' Insert data points from BS
#'
#' Function to prepare and insert BS data points. The function first prepares
#' the required vintages and inserts them, then prepares the data points
#' table and inserts it. The function returns the results invisibly.
#'
#' This is a BS specific function, which should be followed by the generic
#' UMARimportR function to write the vintage hashes and clean up redundant
#' vintages.
#'
#' @param data_path path to folder with
#' @param table_name the table name (eg "DP")
#' @param con Database connection
#' @param schema Schema name
#'
#' @return Insertion results (invisibly)
#' @export
MF_import_data_points <- function(data_path, table_name, con, schema = "platform") {
  message("Importing data points from: ", table_name, " into schema ", schema)
  # collect outputs from the functions into one result list
  result <- list()
  # Try to prepare MF vintage table but catch any errors
  file_path <- paste0(data_path, "/", meta |>
                        dplyr::filter(table_name == !!table_name) |>
                        dplyr::pull(file_path),
                      "_1992-2025.xlsx")
  sheet_name <- meta |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(sheet_name)
  vintage_result <- tryCatch(
    expr = {list(
      vintages = prepare_vintage_table(file_path, table_name, sheet_name, con, schema),
      error = NULL)},
    error = function(e) {
      error_msg <- conditionMessage(e)
      message("Note: ", error_msg)
      return(list(
        vintages = NULL,
        error = error_msg))})
  # Store error message if any
  result$vintage_error <- vintage_result$error
  # Only proceed with import if vintages were prepared successfully
  if (!is.null(vintage_result$vintages)) {
    # import vintages
    result$vintages_m <- UMARimportR::insert_new_vintage(con, vintage_result$monthly_vintages, schema)
    result$vintages_a <- UMARimportR::insert_new_vintage(con, vintage_result$annual_vintages, schema)
    # Prepare data in SURS-specific way
    prep_data <- prepare_mf_data_for_insert(vintage_resutl$parsed_data, con, schema)
    # Insert the prepared data
    result$data <- UMARimportR::insert_prepared_data_points(prep_data, con, schema)
  } else {
    message("Skipping import for ", table_name, " due to vintage preparation issue: ", vintage_result$error)
  }
  invisible(result)
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
#' @param schema Schema name
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
#' }
MF_import_data_points_old <- function(file_path, table_name, sheet_name, con,  schema = "platform") {
  l <- prepare_vintage_table(file_path, table_name, sheet_name, con, schema)
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

  insert_data_points(l[[3]], con)

  lapply(res, sum)
}




#' Insert datapoints into data_point table
#'
#'
#' So, the function extracts and preps the data with \link[MFfetchR]{prepare_mf_data_for_insert}
#' and writes it to a temporary table in the database.
#'
#' It inserts any new periods into the period table,
#' adds the data points to the data point table.
#' @param parsed_data list with at least monthly and annual dataframes with the data_points
#' output of \link[MFfetchR]{mf_excel_parser}.
#' @param con connection to database
#' @param schema schema name defaults to "platform"
#'
#' @return nothing, just some printing along the way
#' @export
#'
insert_data_points <- function(parsed_data, con, schema = "platform"){
  on.exit(dbExecute(con, sprintf("drop table tmp")))
  df <- prepare_mf_data_for_insert(parsed_data, con)

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

