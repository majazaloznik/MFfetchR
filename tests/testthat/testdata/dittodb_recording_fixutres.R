## This code was run once and is here for archival purposes.

source("tests/testthat/helper-connection.R")

# renv::install("majazaloznik/UMARaccessR")
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
# start_db_capturing()
# con_test <- make_test_connection()
# dp <- prepare_source_table(con_test)
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# dp <- prepare_table_table("DP",keep_vintage = TRUE, con_test)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# df <- prepare_series_levels_table("ZPIZ", con_test)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# Sys.setenv("TESTTHAT"="true")
# resutls <- MF_import_structure_old("DP", con_test, schema = "platform",
#                                    keep_vintage = FALSE, "tests/testthat/testdata/zadnje_stare/")
# df <- prepare_vintage_table("inst/extdata/zadnje_stare/Drzavni_proracun_1992-2025.xlsx",
#                             "DP", "MESPROR", con_test, "platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# Sys.setenv("TESTTHAT"="true")
# resutls <- MF_import_structure_old("OB", con_test, schema = "platform",
#                                     keep_vintage = FALSE, "tests/testthat/testdata/zadnje_stare/")
# MF_import_data_points_old(testthat::test_path("tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
#                           "OB", "OBCINE", con_test, schema = "platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# Sys.setenv("TESTTHAT"="true")
# resutls <- MF_import_structure_old("ZPIZ", con_test, schema = "platform",
#                                    keep_vintage = FALSE, "tests/testthat/testdata/zadnje_stare/")
# df <- prepare_vintage_table(testthat::test_path("tests/testthat/testdata/zadnje_stare/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2025.xlsx"),
#                             "ZPIZ", "ZPIZ", con_test, "platform")
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
#
# xx <- prepare_dimension_levels_table_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
#                                    "KBJF", con_test)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# xx <- prepare_series_table_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
#                                          "KBJF", con_test)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# MF_import_structure_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
# "KBJF", con_test, schema = "platform", keep_vintage = TRUE)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# xx <- MF_import_structure_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
#                         "ZPIZ", con_test, schema = "platform", keep_vintage = TRUE)
# stop_db_capturing()


# start_db_capturing()
# con_test <- make_test_connection()
#
# xx <- prepare_vintage_table_and_merge_data_points("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
#                                   "ZPIZ", con_test, schema = "platform")
# xxx <- prepare_mf_data_for_insert(xx$parsed_data, con_test)
#

con_test <- make_test_connection()
debugonce(MF_import_structure_new)
MF_import_structure_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                        "KBJF", con_test, schema = "platform", keep_vintage = FALSE)
UMARaccessR::sql_get_series_id_from_series_code("MF--KBJF--921--M", con_test)
UMARaccessR::sql_get_series_id_from_series_code("MF--KBJF--921--A", con_test)

MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "KBJF", con_test, schema = "platform")
UMARimportR::remove_empty_vintages(con_test, schema = "platform")

MF_import_structure_new("tests/testthat/testdata/test_9xx.csv", "KBJF", con_test, schema = "platform",
                        keep_vintage = FALSE)
DBI::dbExecute(con_test, "set search_path to views")
DBI::dbExecute(con_test, "REFRESH MATERIALIZED VIEW latest_data_points")

debugonce(mf_csv_parser_new)

xx <- mf_csv_parser_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv")



## clean up start fresh

con_test <- make_test_connection()
# remove data points
ids <- UMARaccessR::sql_get_series_ids_from_table_id(180, con_test)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con_test, ids$id, schema = "platform")
UMARimportR::delete_vintage(con_test, vintages)

MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Kon",
                          "KBJF", "KBJF", con_test,  schema = "platform")






debugonce(mf_csv_parser_new)
x <- mf_csv_parser_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv")

debugonce(prepare_vintage_table_and_merge_data_points)
prepare_vintage_table_and_merge_data_points("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                                            "DP", con_test, schema = "platform")


MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "DP", con_test, schema = "platform")


MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "OB", con_test, schema = "platform")


MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "ZZZS", con_test, schema = "platform")


MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "ZPIZ", con_test, schema = "platform")

DBI::dbExecute(con_test, "set search_path to views")
DBI::dbExecute(con_test, "REFRESH MATERIALIZED VIEW latest_data_points")

UMARimportR::remove_empty_vintages(con_test)
