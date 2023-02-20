
#' MF Excel table parser
#'
#' Parsing function to extract and clean up the bundget timeseries data in
#' the Ministry of Finance excel spreasheets usually published [here](https://www.gov.si/teme/fiskalna-in-javnofinancna-politika/).
#' The parser works on 5 tables with untidy budget account data, furthermore
#' the tables have errors and inconsistencies. The parser first extracts the header
#' because it has 2-3 rows, then the data. Cleaning up the empty rows, adding
#' missing codes, transposing and renaming the columns.
#'
#' Hardcoded failure points:
#' * there is a single duplicated row, if more turn up, the funciton will fail
#' * the first row of the header is 8 rows above the first data row. If that
#' chages or the code of the first data row changes, this will fail.
#' * one of the tables has a six-month column instead of months and years. It
#' is removed, and presumably nothing similar can ever happen again.
#'
#' @param file_path path to excel file
#' @param table_name code of table to be used in series codes
#' @param sheet_name the name of the sheet where the table is
#'
#' @return list of three tables: annual and monthly series and the series codelist
#' @export

mf_excel_parser <- function(file_path, table_name, sheet_name){

  # get head to extract header
  data_head <- readxl::read_excel(file_path, sheet_name, n_max = 25, col_names = FALSE)

  # find first row - hardcoded that header is 8 rows before the foirst row.
  first_row <- which(data_head[,1] == "7")
  first_row_header <- first_row-8

  header <- data_head[first_row_header:(first_row_header+1), ]

  # because years are sometimes in one row and sometimes in another
  # make sure they are all in the same, get month codes and clean up
  # all period codes.
  header <- apply(header,2,  function(x) if(is.na(x[2])) x[2:1] else x)[2:1,] %>%
    apply(2, function(x) if(is.na(x[2])) x else c(x[1], month_codes(x[2]))) %>%
    as.data.frame() %>%
    dplyr::summarise(dplyr::across(everything(), ~ paste(.x[!is.na(.x)], collapse ="" ))) %>%
    dplyr::summarise(dplyr::across(everything(), ~ trim_inter(.x)))

  # read all the data in without the header
  data_raw <- readxl::read_excel(file_path, sheet_name,
                                 skip = first_row - 1, col_names = FALSE)
  # remove empty rows
  data_clean <- data_raw[complete.cases(data_raw[,c(3,5)]),]
  # add header
  colnames(data_clean) <- header
  colnames(data_clean)[1:4] <- c("code", "delete", "description", "description_eng")
  # hardcode: remove 6-month data column from OBCINE if it exists
  try(data_clean <- data_clean %>%  dplyr::select(-JUNIJH01))

  # add missing konto codes and clean up
  suppressWarnings(data_clean %>%
    dplyr::mutate(across(c(4:ncol(data_clean)), as.numeric)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(code = ifelse(!is.na(match(description, konto_codes$description)),
                                 konto_codes$code[match(description, konto_codes$description)], code)) %>%
    dplyr::select(-delete, -description_eng)  -> data_clean)

  # harcode: remove second konto code 7505 if it exists (zzzs), the one where
  # all the values are either zero or NA.
  if(nrow(dplyr::filter(data_clean, code == 7505)) == 2){
    data_clean <- data_clean %>%
      dplyr::filter(!(code == 7505  & dplyr::if_all(c(-code, -description), ~ .x == 0 | is.na(.x)  )))
  }

  series <- data_clean %>% dplyr::select(code, description)
  # transpose
  df <- as.data.frame(t(data_clean[,-2]))
  colnames(df) <- paste0("MF--", table_name, "--", trim_leading(df[1,]))
  df <- df[-1,]

  df$period_id <- row.names(df)

  df <- df %>% dplyr::relocate(period_id)

  # split into annual and monthly datasets.
  monthly <- df[grep("\\d+M\\d+", df$period_id),]
  names(monthly) <- c("period_id", paste0(names(monthly)[-1], "--M"))
  annual <- df[grep("^\\d{4}$", df$period_id),]
  names(annual) <- c("period_id", paste0(names(annual)[-1], "--A"))

  # lengthen tables
  monthly <- tidyr::pivot_longer(monthly, !period_id, names_to = "code", values_to = "value") %>%
    dplyr::arrange(code, period_id)
  annual <- tidyr::pivot_longer(annual, !period_id, names_to = "code", values_to = "value") %>%
    dplyr::arrange(code, period_id)
  mget(c("monthly", "annual", "series"))
}
