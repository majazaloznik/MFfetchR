## This code was run once and is here for archival purposes.

source("tests/testthat/helper-connection.R")

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_category_table_table(con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_table_table(con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_category_table(con)
# stop_db_capturing()
#
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_category_relationship_table(con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_table_dimensions_table(con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_series_table("ZPIZ", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_series_levels_table("ZPIZ", con)
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# dp <- prepare_vintage_table( "tests/testthat/testdata/kbjf_test.xlsx", "KBJF", "GLOBALNA", con )
# stop_db_capturing()

# start_db_capturing()
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = "sandbox",
#                  host = "localhost",
#                  port = 5432,
#                  user = "mzaloznik",
#                  password = Sys.getenv("PG_local_MAJA_PSW"))
# dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# prepare_data_table(parsed, con)
# stop_db_capturing()


### new fixtures 20250520
start_db_capturing()
con_test <- make_test_connection()
dp <- prepare_source_table(con_test)
stop_db_capturing()


start_db_capturing()
con_test <- make_test_connection()
dp <- prepare_table_table("DP",keep_vintage = TRUE, con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
df <- prepare_series_levels_table("ZPIZ", con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
Sys.setenv("TESTTHAT"="true")
resutls <- MF_import_structure_old("DP", con_test, schema = "platform",
                                   keep_vintage = FALSE, "tests/testthat/testdata/zadnje_stare/")
df <- prepare_vintage_table("inst/extdata/zadnje_stare/Drzavni_proracun_1992-2025.xlsx",
                            "DP", "MESPROR", con_test, "platform")
stop_db_capturing()


start_db_capturing()
con_test <- make_test_connection()
Sys.setenv("TESTTHAT"="true")
resutls <- MF_import_structure_old("OB", con_test, schema = "platform",
                                    keep_vintage = FALSE, "tests/testthat/testdata/zadnje_stare/")
MF_import_data_points_old(testthat::test_path("tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
                          "OB", "OBCINE", con_test, schema = "platform")
stop_db_capturing()




