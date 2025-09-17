## This code was run once and is here for archival purposes.

source("tests/testthat/helper-connection.R")

# start_db_capturing()
# con_test <- make_test_connection()
# prepare_dimension_levels_table_new(test_path("testdata/test001.csv"), "UTF-8",
#                                    "KBJF", con_test, schema = "platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# prepare_series_table_new(test_path("testdata/test001.csv"), "UTF-8",
#                                    "KBJF", con_test, schema = "platform")
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# prepare_series_levels_table_new("KBJF", con_test, schema = "platform")
# stop_db_capturing()
#
# # create test table
# start_db_capturing()
# con_test <- make_test_connection()
# MF_import_structure_new(test_path("testdata/test006.csv"), "UTF-8",
#                         "test_mf", con_test, schema = "platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# prepare_vintage_table_and_merge_data_points(test_path("testdata/test006.csv"), "UTF-8",
#                                             "test_mf", con_test, schema = "platform")
# stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
Sys.setenv("TESTTHAT"="true")
MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/tests/testthat/testdata/test006.csv", "UTF-8",
                                            "test_mf", con_test, schema = "platform")
Sys.setenv("TESTTHAT"="false")
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
update_JF_lookup_table_on_db("O:/Avtomatizacija/umar-automation-scripts/data/mf_bilance/new_data/", con_test)
stop_db_capturing()

start_db_capturing()
con_test <- make_test_connection()
Sys.setenv("TESTTHAT"="true")
levels <- get_db_konto_list(con_test)
Sys.setenv("TESTTHAT"="false")
stop_db_capturing()




