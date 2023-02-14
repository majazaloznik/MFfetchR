#' script for data that is saved into R/sysdata.rda

meta <- data.frame(file_path = c("tests/testthat/testdata/Drzavni_proracun_1992-2022.xlsx",
                                 "tests/testthat/testdata/Bilance_proracunov_obcin_1992-2022.xlsx",
                                 "tests/testthat/testdata/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2022.xlsx",
                                 "tests/testthat/testdata/Zavod_za_zdravstveno_zavarovanje_Slovenije_1992-2022.xlsx",
                                 "tests/testthat/testdata/Konsolidirana_bilanca_javnega_financiranja_1992-2022.xlsx"),
                   table_name = c("DP", "OB", "ZPIZ", "ZZZS", "KBJF"),
                   sheet_name = c("MESPROR", "OBCINE", "ZPIZ", "ZZZS", "GLOBALNA"))
usethis::use_data(meta, internal = TRUE)
