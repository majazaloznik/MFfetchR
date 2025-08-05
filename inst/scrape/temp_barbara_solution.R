library(dplyr)
library(tidyr)
raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\kbjf data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))

level3 <- raw |>
  mutate(level3 = as.numeric(substr(`3-nivo konsolidacija`,1,3))) |>
  group_by(level3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(level3 %in% c(411, 412, 413, 414)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = level3) |>
  mutate(konto = as.character(konto))

level4 <- raw |>
  mutate(level4 = as.numeric(substr(`4-nivo konoslidacija`,1,4))) |>
  select(Mesec, level4, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(level4 %in% c(4110:4132, 4134:4143)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = level4) |>
  mutate(konto = as.character(konto))

kbjf <- bind_rows(level3, level4) |>
  arrange(konto)


raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\dp data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))

level3 <- raw |>
  group_by(K3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K3 %in% c(411)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K3)

level4 <- raw |>
  group_by(K4 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K4 %in% c(4110:4119)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K4)

level6 <- raw |>
  select(Mesec, K6, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(K6 %in% c(411000:411999)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K6)

dp <- bind_rows(level3, level4, level6) |>
  arrange(konto)




raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\zpiz data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))

level3 <- raw |>
  group_by(K3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K3 %in% c(411)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K3)

level4 <- raw |>
  group_by(K4 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K4 %in% c(4112:4119)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K4)

level6 <- raw |>
  select(Mesec, K6, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(K6 %in% c(411206:411599)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K6)

zpiz <- bind_rows(level3, level4, level6) |>
  arrange(konto)



raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\obcine data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))

level3 <- raw |>
  group_by(K3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K3 %in% c(411)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K3)

level4 <- raw |>
  group_by(K4 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K4 %in% c(4110:4119)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K4)

level6 <- raw |>
  select(Mesec, K6, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(K6 %in% c(411900:411999)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K6)

obcine <- bind_rows(level3, level4, level6) |>
  arrange(konto)


raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\zzzs data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))

level3 <- raw |>
  group_by(K3 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K3 %in% c(411)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K3)

level4 <- raw |>
  group_by(K4 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K4 %in% c(4116:4119)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K4)

level6 <- raw |>
  select(Mesec, K6, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(K6 %in% c(411600:411699, 411910:411999)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K6)

zzzs <- bind_rows(level3, level4, level6) |>
  arrange(konto)





raw <- openxlsx2::read_xlsx("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\zpiz data.xlsx")
raw <- raw |>
  filter(!is.na(Leto))


level2 <- raw |>
  group_by(K2 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K2 %in% c(70, 71, 72, 73, 74, 78, 40, 41,42,43)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K2)


level3 <- raw |>
  group_by(K3 ,Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K3 %in% c(400:414, 700:741, 784, 787)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K3)

level4 <- raw |>
  group_by(K4 ,  Mesec) |>
  summarise(znesek = sum(`Vrednost (v EUR)`)) |>
  filter(K4 %in% c(4112:4143, 7010:7416, 7842:7870)) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K4)

level6 <- raw |>
  select(Mesec, K6, `Vrednost (v EUR)`) |>
  rename(znesek = `Vrednost (v EUR)`) |>
  filter(K6 %in% c(411206:413102,701004:741600, 784204:787000 )) |>
  pivot_wider(names_from = Mesec, values_from = znesek) |>
  rename(konto = K6)

zpiz_cela <- bind_rows(level2, level3, level4, level6) |>
  arrange(konto)




openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet("kbjf") |>
  openxlsx2::wb_add_data(x = kbjf) |>
  openxlsx2::wb_add_worksheet("dp") |>
  openxlsx2::wb_add_data(x = dp) |>
  openxlsx2::wb_add_worksheet("zpiz") |>
  openxlsx2::wb_add_data(x = zpiz) |>
  openxlsx2::wb_add_worksheet("obcine") |>
  openxlsx2::wb_add_data(x = obcine) |>
  openxlsx2::wb_add_worksheet("zzzs") |>
  openxlsx2::wb_add_data(x = zzzs) |>
  openxlsx2::wb_add_worksheet("zpiz_cela") |>
  openxlsx2::wb_add_data(x = zpiz_cela) |>
  openxlsx2::wb_save("C:\\osebno\\ZaloznikM37\\bekapiranje\\MFfetchR\\inst\\scrape\\MF bilance za Barbaro - jan-dec2024.xlsx")
