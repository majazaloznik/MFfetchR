library(dplyr)
# location of file
file_path <- "C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv"
data_raw <- readr::read_csv2(file_path, locale = readr::locale(encoding = "UTF-16LE"))

# filter out all rows where KONTO is not a substring from the beginning of the column K6_ID
check <- data_raw %>%
  dplyr::filter(KONTO != K6_ID) %>%
  dplyr::filter(!stringr::str_starts(as.character(K6_ID), as.character(KONTO)))

check <- data_raw %>%
  dplyr::filter(KONTO != K6_ID) %>%
  dplyr::filter(!stringr::str_starts(
    format(K6_ID, scientific = FALSE, trim = TRUE),
    format(KONTO, scientific = FALSE, trim = TRUE)
  ))

# zeros in mesec

zeros <- data_raw |>
  dplyr::filter(MESEC == 0)

sums <- data_raw |>
  dplyr::filter(BLG_ID == "ZZZS", MESEC != 0, LETO == 2024) |>
  group_by(K6_ID) |>
  summarise(
    `izračunana letna vsota` = sum(VALUE),
    .groups = "drop"
  )

zeros_check <- zeros |>
  left_join(sums) |>
  filter(VALUE !=  `izračunana letna vsota`) |>
  mutate(razlika = VALUE - `izračunana letna vsota`,
         abs = abs(razlika)) |>
  arrange(desc(abs)) |>
  filter(abs > 0.01) |>
  select(-abs)


duplicates <- data_raw |>
  dplyr::group_by(BLG_ID, LETO, MESEC, K6_ID) |>
  dplyr::mutate(count = dplyr::n()) |>
  dplyr::summarise(count = dplyr::first(count)) |>
  dplyr::filter(count > 1)

duplicates_check <- data_raw |>
  right_join(duplicates, by = c("BLG_ID", "LETO", "MESEC", "K6_ID"), relationship = "many-to-many") |>
  arrange(BLG_ID, LETO, MESEC, K6_ID)


wb <- openxlsx2::wb_workbook()
wb$add_worksheet("konto_check")
wb$add_data_table(sheet = "konto_check", x = check, table_style = "TableStyleMedium2")
wb$add_worksheet("mesec_check")
wb$add_data_table(sheet = "mesec_check", x = zeros_check, table_style = "TableStyleMedium2")
wb$add_worksheet("duplikati_check")
wb$add_data_table(sheet = "duplikati_check", x = duplicates_check, table_style = "TableStyleMedium2")
wb$save("check.xlsx")
