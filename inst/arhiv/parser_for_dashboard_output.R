
con <- make_connection()

xlsx_parser <-  function(table_name, year = 2025, file_path){

  files <- list.files(path = file_path, pattern = paste0("^", table_name, "-", year) , full.names = TRUE)
  target <- files[which.max(file.info(files)$mtime)]
  raw_data <- readxl::read_xlsx(target)
  # remove empty rows at the end of the table
  empty_rows <- which(rowSums(!is.na(raw_data)) == 0)
  if(length(empty_rows) > 0) {
    raw_data <- raw_data[1:(min(empty_rows) - 1), ]
  }

  level2 <- raw_data |>
    dplyr::group_by(K2 ,  Mesec) |>
    dplyr::summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    dplyr::mutate(period_id = paste0(year, "M", sprintf("%02d", Mesec)),
                  series_code = paste0("MF", "--", table_name, "--", K2, "--M")) |>
    dplyr::ungroup() |>
    dplyr::select(-K2, -Mesec)


  level3 <- raw_data |>
    dplyr::group_by(K3 ,  Mesec) |>
    dplyr::summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    dplyr::mutate(period_id = paste0(year, "M", sprintf("%02d", Mesec)),
                  series_code = paste0("MF", "--", table_name, "--", K3, "--M")) |>
    dplyr::ungroup() |>
    dplyr::select(-K3, -Mesec)


  level4 <- raw_data |>
    dplyr::group_by(K4 ,  Mesec) |>
    dplyr::summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    dplyr::mutate(period_id = paste0(year, "M", sprintf("%02d", Mesec)),
                  series_code = paste0("MF", "--", table_name, "--", K4, "--M")) |>
    dplyr::ungroup() |>
    dplyr::select(-K4, -Mesec)

  level6 <- raw_data |>
    dplyr::group_by(K6 ,  Mesec) |>
    dplyr::summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    dplyr::mutate(period_id = paste0(year, "M", sprintf("%02d", Mesec)),
                  series_code = paste0("MF", "--", table_name, "--", K6, "--M")) |>
    dplyr::ungroup() |>
    dplyr::select(-K6, -Mesec)

  final <- level2 |>
    dplyr::bind_rows(level3) |>
    dplyr::bind_rows(level4) |>
    dplyr::bind_rows(level6)
}


xlsx_parser_kb <-  function( year = 2025, file_path){

  files <- list.files(path = file_path, pattern = paste0("^KBJF-", year) , full.names = TRUE)
  target <- files[which.max(file.info(files)$mtime)]
  raw_data <- readxl::read_xlsx(target)
  # remove empty rows at the end of the table
  empty_rows <- which(rowSums(!is.na(raw_data)) == 0)
  if(length(empty_rows) > 0) {
    raw_data <- raw_data[1:(min(empty_rows) - 1), ]
  }

  level1 <- raw_data |>
    dplyr::group_by(`1-nivo konsolidacija`,  Mesec) |>
    dplyr::summarise(znesek = sum(`Vrednost (v EUR)`))

  level2 <- raw_data |>
    mutate(level2 = as.numeric(substr(`2-nivo konsolidacija`,1,2))) |>
    group_by(level2 ,  Mesec) |>
    summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    filter(level2 %in% c(70, 71, 72, 73, 74, 78, 40, 41,42,43, 45)) |>
    pivot_wider(names_from = Mesec, values_from = znesek)


  level3 <- raw_data |>
    mutate(level3 = as.numeric(substr(`3-nivo konsolidacija`,1,3))) |>
    group_by(level3 ,  Mesec) |>
    summarise(znesek = sum(`Vrednost (v EUR)`)) |>
    filter(level3 %in% c(700,701, 702,703,704,705,706, 400, 401, 402, 403, 404, 409,
                         410, 411, 412, 413, 414)) |>
    pivot_wider(names_from = Mesec, values_from = znesek)

  level4 <- raw_data |>
    mutate(level4 = as.numeric(substr(`4-nivo konoslidacija`,1,4))) |>
    select(Mesec, level4, `Vrednost (v EUR)`) |>
    rename(znesek = `Vrednost (v EUR)`) |>
    filter(level4 %in% c(7000, 7001, 7030, 7040, 7042, 7102)) |>
    pivot_wider(names_from = Mesec, values_from = znesek)

  place <-  raw_data |>
    dplyr::filter(!grepl("^[0-9]", `3-nivo konsolidacija`)) |>
    dplyr::mutate(konto = stringr::str_extract(`4-nivo konoslidacija`, "^[0-9]+")) |>
    rename(znesek = `Vrednost (v EUR)`) |>
    select(Mesec, konto, znesek) |>
    pivot_wider(names_from = Mesec, values_from = znesek)


  final <- level2 |>
    dplyr::bind_rows(level3) |>
    dplyr::bind_rows(level4) |>
    dplyr::bind_rows(level6)

}

ob <- xlsx_parser("OB", 2025, testthat::test_path("testdata/nove/"))
dp <- xlsx_parser("DP", 2025, testthat::test_path("testdata/nove/"))
zpiz <- xlsx_parser("ZPIZ", 2025, testthat::test_path("testdata/nove/"))
zzzs <- xlsx_parser("ZZZS", 2025, testthat::test_path("testdata/nove/"))
