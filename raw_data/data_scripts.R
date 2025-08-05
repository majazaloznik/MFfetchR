library(dplyr)
#' script for data that is saved into R/sysdata.rda
meta <- data.frame(file_path = c("Drzavni_proracun",
                                         "Bilance_proracunov_obcin",
                                         "Zavod_za_pokojninsko_in_invalidsko_zavarovanje",
                                         "Zavod_za_zdravstveno_zavarovanje_Slovenije",
                                         "Konsolidirana_bilanca_javnega_financiranja"),
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
                                         "PRORAČUNSKI PRESEŽEK (PRIMANJKLJAJ)",
                                         "PRIMARNI PRESE\u017dEK (PRIMANJKLJAJ)",
                                         "TEKOČI PRESEŽEK (PRIMANJKLJAJ)",
                                         "PLA\u010cE IN DRUGI IZDATKI ZAPOSLENIM" ,
                                         "PRISPEVKI DELODAJALCEV ZA SOCIALNO VARNOST",
                                         "IZDATKI ZA BLAGO IN STORITVE" ),
                          code = c(901, 901,902, 902,902,903, 903, 904, 904, 905, 905, 906, 906, 907, 908, 911, 912, 913))

# list of series for KBJF rates tables
kbjf_series_list_eo <- list(
  "MF--KBJF--001--7--M",
  "MF--KBJF--003--70--M",
  "MF--KBJF--005--7000--M",
  "MF--KBJF--006--7001--M",
  "MF--KBJF--016--7030--M",
  "MF--KBJF--021--7040--M",
  "MF--KBJF--023--7042--M",
  "MF--KBJF--007--701--M",
  "MF--KBJF--035--71--M",
  "MF--KBJF--081--78--M",
  "MF--KBJF--050--72--M",
  "MF--KBJF--063--73--M",
  "MF--KBJF--073--74--M",
  "MF--KBJF--091--4--M",
  "MF--KBJF--094--400--M",
  "MF--KBJF--095--413300--M",
  "MF--KBJF--097--401--M",
  "MF--KBJF--098--413301--M",
  "MF--KBJF--099--413310--M",
  "MF--KBJF--101--402--M",
  "MF--KBJF--102--413302--M",
  "MF--KBJF--103--403--M",
  "MF--KBJF--110--404--M",
  "MF--KBJF--115--409--M",
  "MF--KBJF--124--411--M",
  "MF--KBJF--119--41--M",
  "MF--KBJF--124--411--M",
  "MF--KBJF--148--42--M",
  "MF--KBJF--160--43--M",
  "MF--KBJF--173--45--M",
  "MF--KBJF--182--906--M",
  "MF--KBJF--183--907--M")

# list of series for the 12 month cumulative tables
kbjf_series_list_12mK <- list(
  "MF--KBJF--001--7--M",
  "MF--KBJF--002--901--M",
  "MF--KBJF--003--70--M",
  "MF--KBJF--004--700--M",
  "MF--KBJF--007--701--M",
  "MF--KBJF--012--702--M",
  "MF--KBJF--015--703--M",
  "MF--KBJF--020--704--M",
  "MF--KBJF--030--705--M",
  "MF--KBJF--033--706--M",
  "MF--KBJF--035--71--M",
  "MF--KBJF--050--72--M",
  "MF--KBJF--063--73--M",
  "MF--KBJF--073--74--M",
  "MF--KBJF--081--78--M",
  "MF--KBJF--091--4--M",
  "MF--KBJF--092--40--M",
  "MF--KBJF--093--911--M",
  "MF--KBJF--096--912--M",
  "MF--KBJF--100--913--M",
  "MF--KBJF--103--403--M",
  "MF--KBJF--110--404--M",
  "MF--KBJF--115--409--M",
  "MF--KBJF--119--41--M",
  "MF--KBJF--120--410--M",
  "MF--KBJF--124--411--M",
  "MF--KBJF--134--412--M",
  "MF--KBJF--136--413--M",
  "MF--KBJF--143--414--M",
  "MF--KBJF--148--42--M",
  "MF--KBJF--160--43--M",
  "MF--KBJF--173--45--M",
  "MF--KBJF--182--906--M",
  "MF--KBJF--183--907--M")

# name of final rows in  KBJF rates tables
kbjf_row_names_eo <-c("SKUPAJ PRIHODKI",
              "Davčni prihodki brez soc.p.",
              "Dohodnina",
              "Davek od dohodkov pravnih oseb",
              "Davki na nepremičnine",
              "Davek na dodano vrednost",
              "Trošarine (akcize)",
              "PRISPEVKI ZA SOCIALNO VARNOST",
              "NEDAVČNI PRIHODKI",
              "PREJETA SREDSTVA IZ EU IN IZ DRUGIH DRŽAV",
              "Ostalo",
              "SKUPAJ ODHODKI",
              "Plače in drugi stroški dela",
              "Izdatki za blago in storitve",
              "Plačila obresti",
              "REZERVE",
              "TRANSFERI POSAMEZNIKOM IN GOSPODINJSTVOM",
              "Ostali tekoči transferi",
              "Izdatki za investicije",
              "PLAČILA SREDSTEV V PRORAČUN  EU",
              "JAVNOFINANČNI SALDO",
              "PRIMARNI SALDO")

# name of final rows in 12 month cumulative tables.
kbjf_row_names_12mK <-c("SKUPAJ PRIHODKI",
                      "TEKOČI PRIHODKI",
                      "DAVČNI PRIHODKI",
                      "DAVKI NA DOHODEK IN DOBIČEK",
                      "PRISPEVKI ZA SOCIALNO VARNOST",
                      "DAVKI NA PLAČILNO LISTO IN DELOVNO SILO",
                      "DAVKI NA PREMOŽENJE",
                      "DOMAČI DAVKI NA BLAGO IN STORITVE",
                      "DAVKI NA MEDNARODNO TRGOVINO IN TRANSAKCIJE",
                      "DRUGI DAVKI",
                      "NEDAVČNI PRIHODKI",
                      "KAPITALSKI PRIHODKI",
                      "PREJETE DONACIJE",
                      "TRANSFERNI PRIHODKI",
                      "PREJETA SREDSTVA IZ EU IN IZ DRUGIH DRŽAV",
                      "SKUPAJ ODHODKI",
                      "TEKOČI ODHODKI",
                      "PLAČE IN DRUGI IZDATKI ZAPOSLENIM, VKLJUČNO S PRISPEVKI ZA SOCIALNO VARNOST",
                      "IZDATKI ZA BLAGO IN STORITVE",
                      "PLAČILA DOMAČIH IN TUJIH OBRESTI",
                      "REZERVE",
                      "TEKOČI TRANSFERI",
                      "SUBVENCIJE",
                      "TRANSFERI POSAMEZNIKOM IN GOSPODINJSTVOM",
                      "TRANSFERI NEPRIDOBITNIM ORGANIZACIJAM IN USTANOVAM, DRUGI TEKOČI DOMAČI TRANSFERI",
                      "TEKOČI TRANSFERI V TUJINO",
                      "INVESTICIJSKI ODHODKI",
                      "INVESTICIJSKI TRANSFERI",
                      "PLAČILA SREDSTEV V PRORAČUN EU",
                      "JAVNOFINANČNI SALDO",
                      "PRIMARNI SALDO")

konto_raw <- readr::read_csv2("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/EK_Classification.csv", locale = readr::locale(encoding = "UTF-16LE"))
konto_6 <- konto_raw |>
  select(K6_ID, K6_NAME) |>
  distinct() |>
  rename(konto = K6_ID, description = K6_NAME)

konto_4 <- konto_raw |>
  select(K4_ID, K4_NAME) |>
  distinct() |>
  rename(konto = K4_ID, description = K4_NAME)

konto_3 <- konto_raw |>
  select(K3_ID, K3_NAME) |>
  distinct() |>
  rename(konto = K3_ID, description = K3_NAME)

konto_2 <- konto_raw |>
  select(K2_ID, K2_NAME) |>
  distinct() |>
  rename(konto = K2_ID, description = K2_NAME)

konto_1 <- konto_raw |>
  select(K2_SEKCIJA) |>
  distinct() |>
  rename(description = K2_SEKCIJA) |>
  mutate(konto = c("4", "44", "50", "55", "7", "75")) |>
  filter(konto == "4" | konto == 7)

# and the previous ones
more_codes <- konto_codes |>
  dplyr::group_by(code) |>
  dplyr::summarise(description = first(description)) |>
  dplyr::filter(code != 908) |>
  dplyr::rename(konto = code) |>
  dplyr::mutate(konto = as.character(konto))

konto_lookup <- bind_rows(
  konto_1,
  konto_2,
  konto_3,
  konto_4,
  konto_6, more_codes) |>
  arrange(konto)

# add new manually calculated kontos
konto_lookup <- bind_rows(
  konto_lookup,
  data.frame(konto = c("908", "914", "915", "916", "917", "918", "919", "920", "921"),
             description = c("Tekoči presežek (primanjkljaj)",
                             "Davčni prihodki brez socialnih prispevkov",
                             "Kapitalski in transferni prihodki, donacije",
                             "Plače in socialni prispevki",
                             "Plačila obresti",
                             "Ostali tekoči transferi (brez posameznikov in gospodinjstev)",
                             "Izdatki za investicije",
                             "Izdatki brez plačil obresti",
                             "Tekoči transferi v javne zavode - za izdatke za blago in storitve (413302-6)")))



################################################################################
## prepare transformations for calcuated series (90x)
################################################################################
# Define transformations with explicit operations
base_transforms <- list(
  "901" = list(add = c("70", "71"), subtract = c()),
  "902" = list(add = c("75"), subtract = c("44")),
  "903" = list(add = c("50"), subtract = c("55")),
  "904" = list(add = c("7", "75", "50"), subtract = c("4", "44", "55")),
  "905" = list(add = c("4"), subtract = c( "7")),
  "906" = list(add = c("7"), subtract = c( "4")),
  "907" = list(add = c("7", "403", "404"), subtract = c("7102", "4")),
  "908" = list(add = c("70", "71"), subtract = c( "40", "41")),
  "911" = list(add = c("400", "413300"), subtract = c()),
  "912" = list(add = c("401", "413301", "413310"), subtract = c()),
  "913" = list(add = c("402", "413302", "413303", "413304", "413305", "413306"), subtract = c()),
  "914" = list(add = c("70"), subtract = c("701")),
  "915" = list(add = c("72", "73", "74"), subtract = c()),
  "916" = list(add = c("400", "413300","401", "413301", "413310"), subtract = c()),
  "917" = list(add = c("403", "404"), subtract = c()),
  "918" = list(add = c("41"), subtract = c("411")),
  "919" = list(add = c("42", "43"), subtract = c()),
  "920" = list(add = c("4"), subtract = c("403", "404")),
  "921" = list(add = c("413302", "413303", "413304", "413305", "413306"), subtract = c()))



# Define the pattern variations
second_elements <- c("DP", "OB", "ZPIZ", "ZZZS", "KBJF")
last_elements <- c("M", "A")

# Generate transforms correctly - each row gets only matching patterns
transforms <- purrr::imap(base_transforms, \(transform_def, target_num) {
  tidyr::expand_grid(
    second = second_elements,
    last = last_elements
  ) |>
    dplyr::mutate(
      target_code = paste0("MF--", second, "--", target_num, "--", last),
      add_codes = purrr::map2(second, last, \(s, l) {
        paste0("MF--", s, "--", transform_def$add, "--", l)
      }),
      subtract_codes = purrr::map2(second, last, \(s, l) {
        paste0("MF--", s, "--", transform_def$subtract, "--", l)
      })
    )
}) |>
  purrr::list_rbind()

# remove nonexistent codes
transforms <- transforms |>
  dplyr::filter(!target_code %in% c("MF--DP--911--M",
                                   "MF--DP--911--A",
                                   "MF--DP--912--M",
                                   "MF--DP--912--A",
                                   "MF--DP--913--M",
                                   "MF--DP--913--A",
                                   "MF--DP--914--M",
                                   "MF--DP--914--A",
                                   "MF--DP--915--M",
                                   "MF--DP--915--A",
                                   "MF--DP--916--M",
                                   "MF--DP--916--A",
                                   "MF--DP--917--M",
                                   "MF--DP--917--A",
                                   "MF--DP--918--M",
                                   "MF--DP--918--A",
                                   "MF--DP--919--M",
                                   "MF--DP--919--A",
                                   "MF--DP--920--M",
                                   "MF--DP--920--A",
                                   "MF--DP--921--M",
                                   "MF--DP--921--A",
                                   "MF--OB--911--M",
                                   "MF--OB--911--A",
                                   "MF--OB--912--M",
                                   "MF--OB--912--A",
                                   "MF--OB--913--M",
                                   "MF--OB--913--A",
                                   "MF--OB--914--M",
                                   "MF--OB--914--A",
                                   "MF--OB--915--M",
                                   "MF--OB--915--A",
                                   "MF--OB--916--M",
                                   "MF--OB--916--A",
                                   "MF--OB--917--M",
                                   "MF--OB--917--A",
                                   "MF--OB--918--M",
                                   "MF--OB--918--A",
                                   "MF--OB--919--M",
                                   "MF--OB--919--A",
                                   "MF--OB--920--M",
                                   "MF--OB--920--A",
                                   "MF--OB--921--M",
                                   "MF--OB--921--A",
                                   "MF--ZPIZ--911--M",
                                   "MF--ZPIZ--911--A",
                                   "MF--ZPIZ--912--M",
                                   "MF--ZPIZ--912--A",
                                   "MF--ZPIZ--913--M",
                                   "MF--ZPIZ--913--A",
                                   "MF--ZPIZ--913--M",
                                   "MF--ZPIZ--913--A",
                                   "MF--ZPIZ--914--M",
                                   "MF--ZPIZ--914--A",
                                   "MF--ZPIZ--915--M",
                                   "MF--ZPIZ--915--A",
                                   "MF--ZPIZ--916--M",
                                   "MF--ZPIZ--916--A",
                                   "MF--ZPIZ--917--M",
                                   "MF--ZPIZ--917--A",
                                   "MF--ZPIZ--918--M",
                                   "MF--ZPIZ--918--A",
                                   "MF--ZPIZ--919--M",
                                   "MF--ZPIZ--919--A",
                                   "MF--ZPIZ--920--M",
                                   "MF--ZPIZ--920--A",
                                   "MF--ZPIZ--921--M",
                                   "MF--ZPIZ--921--A",
                                   "MF--ZZZS--911--M",
                                   "MF--ZZZS--911--A",
                                   "MF--ZZZS--912--M",
                                   "MF--ZZZS--912--A",
                                   "MF--ZZZS--913--M",
                                   "MF--ZZZS--913--A",
                                   "MF--ZZZS--914--M",
                                   "MF--ZZZS--914--A",
                                   "MF--ZZZS--915--M",
                                   "MF--ZZZS--915--A",
                                   "MF--ZZZS--916--M",
                                   "MF--ZZZS--916--A",
                                   "MF--ZZZS--917--M",
                                   "MF--ZZZS--917--A",
                                   "MF--ZZZS--918--M",
                                   "MF--ZZZS--918--A",
                                   "MF--ZZZS--919--M",
                                   "MF--ZZZS--919--A",
                                   "MF--ZZZS--920--M",
                                   "MF--ZZZS--920--A",
                                   "MF--ZZZS--921--M",
                                   "MF--ZZZS--921--A",
                                   "MF--OB--908--M",
                                   "MF--OB--908--A",
                                   "MF--ZPIZ--908--M",
                                   "MF--ZPIZ--908--A",
                                   "MF--ZZZS--908--M",
                                   "MF--ZZZS--908--A"))



usethis::use_data(meta,
                  konto_codes,
                  kbjf_series_list_eo,
                  kbjf_series_list_12mK,
                  kbjf_row_names_eo,
                  kbjf_row_names_12mK,
                  konto_lookup,
                  transforms,
                  internal = TRUE, overwrite = TRUE)
