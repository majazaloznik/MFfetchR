#' script for data that is saved into R/sysdata.rda

meta <- data.frame(file_path = c("tests/testthat/testdata/Drzavni_proracun_1992-2022.xlsx",
                                 "tests/testthat/testdata/Bilance_proracunov_obcin_1992-2022.xlsx",
                                 "tests/testthat/testdata/Zavod_za_pokojninsko_in_invalidsko_zavarovanje_1992-2022.xlsx",
                                 "tests/testthat/testdata/Zavod_za_zdravstveno_zavarovanje_Slovenije_1992-2022.xlsx",
                                 "tests/testthat/testdata/Konsolidirana_bilanca_javnega_financiranja_1992-2022.xlsx"),
                   table_name = c("DP", "OB", "ZPIZ", "ZZZS", "KBJF"),
                   sheet_name = c("MESPROR", "OBCINE", "ZPIZ", "ZZZS", "GLOBALNA"))

konto_codes <- data.frame(description =c("TEKO\u010cI PRIHODKI" ,
                                         "TEKOCI PRIHODKI",
                                         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH DELE\u017dEV",
                                         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH  DELEEV",
                                         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH DELE\u017dEV\u017e",
                                         "NETO ZADOL\u017dEVANJE" ,
                                         "NETO ZADOL\u017dEVANJE (ODPLA\u010cILA) DOLGA",
                                         "POVE\u010cANJE (ZMANJ\u0160ANJE) SREDSTEV NA RA\u010cUNIH" ,
                                         "(I.+IV.+VII.-II.-V.-VIII.)" ,
                                         "NETO FINANCRANJE",
                                         "NETO FINANCIRANJE" ,
                                         "PRESE\u017dEK (PRIMANJKLJAJ)",
                                         "PRIMARNI PRESE\u017dEK (PRIMANJKLJAJ)",
                                         "PLA\u010cE IN DRUGI IZDATKI ZAPOSLENIM" ,
                                         "PRISPEVKI DELODAJALCEV ZA SOCIALNO VARNOST",
                                         "IZDATKI ZA BLAGO IN STORITVE" ),
                          code = c(901, 901,902, 902,902,903, 903, 904, 904, 905, 905, 906, 907, 911, 912, 913))
usethis::use_data(meta, konto_codes, internal = TRUE, overwrite = TRUE)
