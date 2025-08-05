library(dplyr)
raw <- openxlsx2::read_xlsx("O:\\Users\\LJuznikR\\JAVNE_FINANCE\\Konsolidirana bilanca javnega financiranja\\EKONOMSKO_OGLEDALO_prispevek_JF\\2025\\EO - avgust 2025\\data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))
library(dplyr)
library(tidyr)
level1 <- raw |>
  group_by(`1-nivo konsolidacija`,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = `1-nivo konsolidacija`)

level2 <- raw |>
  mutate(level2 = as.numeric(substr(`2-nivo konsolidacija`,1,2))) |>
  group_by(level2 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(level2 %in% c(70, 71, 72, 73, 74, 78, 40, 41,42,43, 45)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)|>
  rename(konto = level2) |>
  mutate(konto = as.character(konto))


level3 <- raw |>
  mutate(level3 = as.numeric(substr(`3-nivo konsolidacija`,1,3))) |>
  group_by(level3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(level3 %in% c(700,701, 702,703,704,705,706:741, 780:800, 400, 401, 402, 403, 404, 409,
                       410, 411, 412, 413, 414)) |>
  pivot_wider(names_from = Mesec, values_from = znesek)|>
  rename(konto = level3) |>
  mutate(konto = as.character(konto))

level4 <- raw |>
  mutate(level4 = as.numeric(substr(`4-nivo konoslidacija`,1,4))) |>
  select(Mesec, level4, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(level4 %in% c(4000:4132, 4134:4323, 4500:4506, 7000:7404)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = level4) |>
  mutate(konto = as.character(konto))

place <-  raw |>
  dplyr::filter(!grepl("^[0-9]", `3-nivo konsolidacija`)) |>
  dplyr::mutate(konto = stringr::str_extract(`4-nivo konoslidacija`, "^[0-9]+")) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  select(Mesec, konto, znesek) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  mutate(konto = as.character(konto))

output <- bind_rows( level2, level3, level4, place) |>
  arrange(konto)

openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("level1") |>
  openxlsx2::wb_add_data(x = level1) |>
  openxlsx2::wb_add_worksheet("output") |>
  openxlsx2::wb_add_data(x = output) |>
  openxlsx2::wb_save("O:\\Users\\LJuznikR\\JAVNE_FINANCE\\Konsolidirana bilanca javnega financiranja\\EKONOMSKO_OGLEDALO_prispevek_JF\\2025\\EO - avgust 2025\\out2.xlsx")
