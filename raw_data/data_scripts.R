# get raw codes and aggregated ones from classification
konto_raw <- readr::read_csv2("M:\\data\\MF_javne_finance\\nove_testni_dump\\Table_4BJF_BI_DATA\\EK_Classification.csv", locale = readr::locale(encoding = "UTF-16LE"))
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

konto_1 <- tibble(
  konto = c("4", "7"),
  description = c("II. SKUPAJ ODHODKI (40+41+42+43+45)",
                  "I. SKUPAJ PRIHODKI (70+71+72+73+74+78)"))
################################################################################
## prepare transformations for calcuated series (90x)
################################################################################
calculated_series_lookup <- tibble::tribble(
  ~konto, ~description,
  "901",   "TEKOČI PRIHODKI",
  "902",   "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH DELEŽEV",
  "903",   "NETO ZADOLŽEVANJE",
  "904",   "POVEČANJE (ZMANJŠANJE) SREDSTEV NA RAČUNIH",
  "905",   "NETO FINANCIRANJE",
  "906",   "PRESEŽEK (PRIMANJKLJAJ)",
  "907",   "PRIMARNI PRESEŽEK (PRIMANJKLJAJ)",
  "908",   "TEKOČI PRESEŽEK (PRIMANJKLJAJ)",
  "911",   "PLAČE IN DRUGI IZDATKI ZAPOSLENIM",
  "912",   "PRISPEVKI DELODAJALCEV ZA SOCIALNO VARNOST",
  "913",   "IZDATKI ZA BLAGO IN STORITVE",
  "914",  "Davčni prihodki brez socialnih prispevkov",
  "915",  "Kapitalski in transferni prihodki, donacije",
  "916",  "Plače in socialni prispevki",
  "917",  "Plačila obresti",
  "918",  "Ostali tekoči transferi (brez posameznikov in gospodinjstev)",
  "919",  "Izdatki za investicije",
  "920",  "Izdatki brez plačil obresti",
  "921",  "Tekoči transferi v javne zavode - za izdatke za blago in storitve (413302-6)")

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

konto_lookup <- bind_rows(konto_1,
                          konto_2,
                          konto_3,
                          konto_4,
                          konto_6,
                          calculated_series_lookup) |>
  arrange(konto)

# Define the pattern variations
second_elements <- c("DP", "OB", "ZPIZ", "ZZZS", "KBJF")
last_elements <- c("M", "A")

# excluded codes
# All 911-921 codes for DP, OB, ZPIZ, ZZZS
base_exclusions <- tidyr::expand_grid(
  type = c("DP", "OB", "ZPIZ", "ZZZS"),
  num = c("911", "912", "913", "914", "915", "916", "917", "918", "919", "920", "921"),
  period = last_elements) |>
  dplyr::mutate(code = paste0("MF--", type, "--", num, "--", period))
# Add 908 exclusions for OB, ZPIZ, ZZZS
type_908_exclusions <- tidyr::expand_grid(
  type = c("OB", "ZPIZ", "ZZZS"),
  num = "908",
  period = last_elements) |>
  dplyr::mutate(code = paste0("MF--", type, "--", num, "--", period))

excluded_codes <-   c(base_exclusions$code, type_908_exclusions$code)

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
  purrr::list_rbind() |>
  dplyr::filter(!target_code %in% excluded_codes)

usethis::use_data(
                  konto_lookup,
                  transforms,
                  internal = TRUE, overwrite = TRUE)
