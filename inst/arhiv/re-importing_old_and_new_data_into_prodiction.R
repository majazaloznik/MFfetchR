
con <- make_connection()

# remove data points
ids <- UMARaccessR::sql_get_series_ids_from_table_id(300, con)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)

# import 2023 old
MF_import_data_points_old("tests/testthat/testdata/Konsolidirana_bilanca_javnega_financiranja_1992-2023.xlsx",
                          "KBJF", "GLOBALNA", con,  schema = "platform")
# import 2025 old
MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Konsolidirana_bilanca_javnega_financiranja_1992-2025.xlsx",
                          "KBJF", "GLOBALNA", con,  schema = "platform")

UMARimportR::remove_empty_vintages(con)

# delete 2025 in sql
# !!!

MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "KBJF", con, schema = "platform")




# ZZZS #########################################################################


ids <- UMARaccessR::sql_get_series_ids_from_table_id(299, con)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)

# import 2023 old
MF_import_data_points_old("tests/testthat/testdata/Zavod_za_zdravstveno_zavarovanje_Slovenije_1992-2023.xlsx",
                          "ZZZS", "ZZZS", con,  schema = "platform")

MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Zavod_za_zdravstveno_zavarovanje_Slovenije_1992-2025.xlsx ",
                          "ZZZS", "ZZZS", con,  schema = "platform")

UMARimportR::remove_empty_vintages(con)

# delete 2025 in sql
### !!!
MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "ZZZS", con, schema = "platform")



# ZPIZ #########################################################################


ids <- UMARaccessR::sql_get_series_ids_from_table_id(298, con)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
# import 2023 old
MF_import_data_points_old("tests/testthat/testdata/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2023.xlsx",
                          "ZPIZ", "ZPIZ", con,  schema = "platform")

MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2025.xlsx",
                          "ZPIZ", "ZPIZ", con,  schema = "platform")
UMARimportR::remove_empty_vintages(con)

# delete 2025 in sql
### !!!

MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "ZPIZ", con, schema = "platform")


# OB ###########################################################################

ids <- UMARaccessR::sql_get_series_ids_from_table_id(297, con)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)

MF_import_data_points_old("tests/testthat/testdata/Bilance_proracunov_obcin_1992-2023.xlsx",
                          "OB", "OBCINE", con,  schema = "platform")

MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx",
                          "OB", "OBCINE", con,  schema = "platform")

UMARimportR::remove_empty_vintages(con)

# delete 2025 in sql
### !!!

MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "OB", con, schema = "platform")


## DP ##########################################################################
ids <- UMARaccessR::sql_get_series_ids_from_table_id(296, con)
# run twice until you get all NAs
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)
vintages <- UMARaccessR::sql_get_vintage_from_series(con, ids$id, schema = "platform")

UMARimportR::delete_vintage(con, vintages)

MF_import_data_points_old("tests/testthat/testdata/Drzavni_proracun_1992-2023.xlsx",
                          "DP", "MESPROR", con,  schema = "platform")

MF_import_data_points_old("tests/testthat/testdata/zadnje_stare/Drzavni_proracun_1992-2025.xlsx",
                          "DP", "MESPROR", con,  schema = "platform")

UMARimportR::remove_empty_vintages(con)

# delete 2025 in sql
### !!!

MF_import_data_points_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                          "DP", con, schema = "platform")


# final clean up of redundant vintages

UMARimportR::vintage_cleanup(con, table_id = 296)

UMARimportR::vintage_cleanup(con, table_id = 297)
UMARimportR::vintage_cleanup(con, table_id = 298)
UMARimportR::vintage_cleanup(con, table_id = 299)
UMARimportR::vintage_cleanup(con, table_id = 300)

DBI::dbExecute(con, "set search_path to views")
DBI::dbExecute(con, "REFRESH MATERIALIZED VIEW latest_data_points")

