#' MF csv table parser
#'
#' This function reads the data from the CSV file and processes it.
#' It filters out rows with MESEC == 0, modifies BLG_ID, and creates new columns K1 to K4.
#' It also creates a period_id column and checks for duplications in the data.
#' Finally, it aggregates the data at different levels (K1, K2, K3, K4) and returns a final dataframe.
#' In addition it calculates all the 9XX transformations.

#' @param file_path path to csv file
#'
#' @return list of three tables: annual and monthly series and the series codelist
#' @export
mf_csv_parser_new <- function(file_path) {
  message("Reading csv file.")
  data_raw <- readr::read_delim(file_path,
                                delim = "\t",
                                locale = readr::locale(encoding = "UTF-8", decimal_mark = ","),
                                show_col_types = FALSE)
  if (nrow(data_raw) == 0) stop("There was no data read.")
  required_cols <- c("BLG_ID", "LETO", "MESEC", "K6_ID", "VALUE" )
  missing_cols <- setdiff(required_cols, names(data_raw))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  message("Read ", nrow(data_raw),  " rows of data.")
  message("Preparing aggregations.")
  # Base data preparation
  data <- data_raw |>
    dplyr::group_by(BLG_ID, LETO, MESEC, K6_ID) |>
    dplyr::summarise(VALUE = sum(VALUE)) |> # aggregates over duplicates
    dplyr::filter(MESEC != 0) |>
    dplyr::mutate(
      BLG_ID = dplyr::case_when(
        BLG_ID == "KB-JS" ~ "KBJF",
        BLG_ID == "KB-LS" ~ "OB",
        TRUE ~ BLG_ID
      ),
      K1 = K6_ID %/% 100000,
      K2 = K6_ID %/% 10000,
      K3 = K6_ID %/% 1000,
      K4 = K6_ID %/% 100,
      period_id = paste0(LETO, "M", sprintf("%02d", MESEC))
    ) |> dplyr::ungroup()
  mesec_zero_count <- sum(data_raw$MESEC == 0)
  message("Rows with MESEC == 0: ", mesec_zero_count)

  # Expected row count after filter
  expected_rows <- nrow(data_raw) - mesec_zero_count
  message("Expected rows after MESEC filter: ", expected_rows)
  message("Actual rows after processing: ", nrow(data))
  if (expected_rows != nrow(data)) message("Check for duplicates, there are ", nrow(data), " unique BLG_ID, period id and K6 combos.")
  # Level 4 (6-digit accounts) - no filtering needed
  level4 <- data |>
    dplyr::group_by(BLG_ID, K4, period_id, LETO) |>
    dplyr::summarise(value = sum(VALUE), .groups = "drop") |>
    dplyr::rename(konto = K4)

  # Level 3 (3-digit accounts)
  level3 <- data |>
    dplyr::filter(
      # For KBJF: exclude 4133xx accounts from 413
      !(BLG_ID == "KBJF" & K4 == 4133)
    ) |>
    dplyr::group_by(BLG_ID, K3, period_id, LETO) |>
    dplyr::summarise(value = sum(VALUE), .groups = "drop") |>
    dplyr::rename(konto = K3)

  # Level 2 (2-digit accounts)
  level2 <- data |>
    dplyr::mutate(
      # For KBJF: move 4133xx accounts from K2=41 to K2=40
      K2 = dplyr::case_when(
        BLG_ID == "KBJF" & K4 == 4133 ~ 40L,
        TRUE ~ K2
      )
    ) |>
    dplyr::group_by(BLG_ID, K2, period_id, LETO) |>
    dplyr::summarise(value = sum(VALUE), .groups = "drop") |>
    dplyr::rename(konto = K2)

  # Level 1 (1-digit accounts)
  level1 <- data |>
    dplyr::filter(
      !(K2 %in% c(44, 75, 50, 55))) |>
    dplyr::group_by(BLG_ID, K1, period_id, LETO) |>
    dplyr::summarise(value = sum(VALUE), .groups = "drop") |>
    dplyr::rename(konto = K1)

  monthly <- data |>
    dplyr::rename(konto = K6_ID, value = VALUE) |>
    dplyr::select(-LETO, -MESEC) |>
    dplyr::bind_rows(level1) |>
    dplyr::bind_rows(level2) |>
    dplyr::bind_rows(level3) |>
    dplyr::bind_rows(level4) |>
    dplyr::select(-K1, -K2, -K3, -K4) |>
    dplyr::mutate(konto = sprintf("%.0f", konto)) |>
    dplyr::mutate(code = paste("MF", BLG_ID, konto, "M", sep = "--")) |>
    dplyr::select(period_id, code, value)

  # expand table for missing months
  # Extract all unique periods that exist in your data
  # this is to make sure all series have the same length, even if the value was 0
  all_periods <- monthly |>
    dplyr::pull(period_id) |>
    unique() |>
    sort()

  # Complete the grid
  monthly <- monthly |>
    tidyr::complete(
      code,
      period_id = all_periods,
      fill = list(value = 0)
    )

  # helper function for transformations
  # Create transform function
  calc_transform <- function(data, add_codes, subtract_codes) {
    add_values <- if(length(add_codes) > 0) {
      data |> dplyr::filter(code %in% add_codes) |> dplyr::pull(value) |> sum()
    } else 0

    subtract_values <- if(length(subtract_codes) > 0) {
      data |> dplyr::filter(code %in% subtract_codes) |> dplyr::pull(value) |> sum()
    } else 0

    add_values - subtract_values
  }
  message("Calculating new transformed series.")
  # Apply transformations - select only the columns you need
  new_series <- transforms |>
    dplyr::filter(last == "M") |>
    dplyr::select(target_code, add_codes, subtract_codes) |>
    purrr::pmap_dfr(\(target_code, add_codes, subtract_codes) {
      all_codes <- c(add_codes, subtract_codes)

      monthly |>
        dplyr::filter(code %in% all_codes) |>
        dplyr::summarise(
          value = calc_transform(dplyr::pick(dplyr::everything()), add_codes, subtract_codes),
          .by = period_id
        ) |>
        dplyr::mutate(code = target_code)
    })

  monthly <- dplyr::bind_rows(monthly, new_series)
  message("Aggregating annual data and preparing series list.")
  annual <-  monthly |>
    dplyr::mutate(LETO = stringr::str_extract(period_id, "\\d{4}")) |>
    dplyr::group_by(code, LETO) |>
    dplyr::summarise(value = sum(value),
              count = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(count == 12) |>
    dplyr::rename(period_id = LETO) |>
    dplyr::mutate(code = sub("M$", "A", code))|>
    dplyr::select(period_id, code, value)

  konto_lookup <- get_konto_list_full("\\\\192.168.38.7\\public$\\Avtomatizacija\\umar-automation-scripts\\data\\mf_bilance\\new_data\\")
  series <- monthly |>
    dplyr::select(code) |>
    dplyr::distinct() |>
    dplyr::mutate(konto = stringr::str_extract(code, "(?<=--)[0-9]+(?=--)"),
                  blg =stringr::str_extract(code, "(?<=MF--)[^-]+(?=--)")) |>
    dplyr::left_join(konto_lookup) |>
    dplyr::select(-code) |>
    dplyr::filter(!is.na(description))
message("Writing list with ", nrow(monthly), " rows of monthly data, ", nrow(annual),
        " rows of annual data, and ", nrow(series), " series.")
  mget(c("monthly", "annual", "series"))
}
