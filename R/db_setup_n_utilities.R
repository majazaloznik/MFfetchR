#' Build all database tables
#'
#' Creates all the tables required to run the database in a given schema by
#' running the appropriate sql file. (Excluded from testing). Location of
#' sql file is in compiled package, hence no "inst/"
#'
#' @param con connection to database
#' @param schema db schema
#'
#' @export
build_db_tables <- function(con, schema = "test_platform"){
  SURSfetchR::execute_sql_file(con,
                   file =system.file("sql/build_db.sql",
                                     package = "MFfetchR"),
                   schema = schema)
}

