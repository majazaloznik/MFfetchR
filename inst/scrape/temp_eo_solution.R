raw <- openxlsx2::read_xlsx("O:\\Users\\LJuznikR\\JAVNE_FINANCE\\Konsolidirana bilanca javnega financiranja\\EKONOMSKO_OGLEDALO_prispevek_JF\\2025\\EO - junij 2025\\data.xlsx")

library(dplyr)
library(tidyr)
level1 <- raw |>
  group_by(`1-nivo konsolidacija`,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)

level2 <- raw |>
  mutate(level2 = as.numeric(substr(`2-nivo konsolidacija`,1,2))) |>
  group_by(level2 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(level2 %in% c(70, 71, 72, 73, 74, 78, 40, 41,42,43, 45)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)


level3 <- raw |>
  mutate(level3 = as.numeric(substr(`3-nivo konsolidacija`,1,3))) |>
  group_by(level3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(level3 %in% c(700,701, 702,703,704,705,706, 400, 401, 402, 403, 404, 409,
                       410, 411, 412, 413, 414)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)

level4 <- raw |>
  mutate(level4 = as.numeric(substr(`4-nivo konoslidacija`,1,4))) |>
  select(Mesec, level4, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(level4 %in% c(7000, 7001, 7030, 7040, 7042, 7102)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)

place <-  raw |>
  dplyr::filter(!grepl("^[0-9]", `3-nivo konsolidacija`)) |>
  dplyr::mutate(konto = stringr::str_extract(`4-nivo konoslidacija`, "^[0-9]+")) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  select(Mesec, konto, znesek) |>
  pivot_wider(names_from = Mesec, values_from = znesek)


openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("level1") |>
  openxlsx2::wb_add_data(x = level1) |>
  openxlsx2::wb_add_worksheet("level2") |>
  openxlsx2::wb_add_data(x = level2) |>
  openxlsx2::wb_add_worksheet("level3") |>
  openxlsx2::wb_add_data(x = level3) |>
  openxlsx2::wb_add_worksheet("level4") |>
  openxlsx2::wb_add_data(x = level4) |>
  openxlsx2::wb_add_worksheet("place in prispevki") |>
  openxlsx2::wb_add_data(x = place) |>
  openxlsx2::wb_save("O:\\Users\\LJuznikR\\JAVNE_FINANCE\\Konsolidirana bilanca javnega financiranja\\EKONOMSKO_OGLEDALO_prispevek_JF\\2025\\EO - junij 2025\\out.xlsx")
