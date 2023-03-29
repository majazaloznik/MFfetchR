
#' MF Excel table parser
#'
#' Parsing function to extract and clean up the budget timeseries data in
#' the Ministry of Finance excel spreadsheets usually published [here](https://www.gov.si/teme/fiskalna-in-javnofinancna-politika/).
#' The parser works on 5 tables with untidy budget account data, furthermore
#' the tables have errors and inconsistencies. The parser first extracts the header
#' because it has 2-3 rows, then the data. Cleaning up the empty rows, adding
#' missing codes, transposing and renaming the columns.
#'
#' Hardcoded failure points:
#' * there is a single duplicated row, if more turn up, the function will fail
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

  # find first row - hardcoded that header is 8 rows before the first row.
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
    dplyr::summarise(dplyr::across(everything(), ~  trim_inter(.x)))

  # read all the data in without the header
  data_raw <- readxl::read_excel(file_path, sheet_name,
                                 skip = first_row - 1, col_names = FALSE)
  # remove empty rows
  data_clean <- data_raw[complete.cases(data_raw[,c(3,5)]),]

  # add header
  colnames(data_clean) <- header
  colnames(data_clean)[1:4] <- c("code", "delete", "description", "description_eng")
  # remove zero columns
  data_clean <- data_clean %>% dplyr::select_if(~ !all(. == 0))

  # hardcode: remove 6-month data column from OBCINE if it exists
  try(data_clean <- data_clean %>%  dplyr::select(-JUNIJH01))

  # add missing konto codes and clean up
  suppressWarnings(data_clean %>%
    dplyr::mutate(across(c(5:ncol(data_clean)), as.numeric)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(code = ifelse(!is.na(match(description, konto_codes$description)) & is.na(code),
                                 konto_codes$code[match(description, konto_codes$description)], code)) %>%
      dplyr::ungroup() %>%
    dplyr::select(-delete, -description_eng) -> data_clean)

  # harcode: remove second konto code 7505 if it exists (zzzs), the one where
  # all the values are either zero or NA.
  if(nrow(dplyr::filter(data_clean, code == 7505)) == 2){
    data_clean <- data_clean %>%
      dplyr::filter(!(code == 7505  & dplyr::if_all(c( -code, -description), ~ .x == 0 | is.na(.x)  )))
  }
  data_clean <- data_clean %>%
    dplyr::mutate(order =  dplyr::row_number()) %>%
  dplyr::mutate(code = paste0("MF--", table_name, "--",sprintf("%03d",as.integer(order)),
                              "--", trim_leading(format(code, scientific = FALSE)))) %>%
    dplyr::relocate(order)

  series <- data_clean %>% dplyr::select(code, description)
  # transpose
  df <- as.data.frame(t(data_clean[,-3]))
  colnames(df) <- df[2,]
  df <- df[-c(1, 2),]

  df$period_id <- row.names(df)

  df <- df %>% dplyr::relocate(period_id) %>%
    dplyr::mutate(dplyr::across(!period_id, as.numeric))

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



#' Write EO tables to Excel
#'
#' Writes and formats the quarterly and annual and original raw data in a way
#' suitable for the EO publication.
#'
#' @param quarterly_list list output of \link[MFfetchR]{prepare_quarterly_eo}
#' @param annual_list list output of \link[MFfetchR]{prepare_annual_eo}
#' @param data_frame list output of \link[MFfetchR]{transform_series_eo}
#' @param outfile path of file to create
#'
#' @return nothing, creates excel file
#' @export
write_excel_kbjf_eo <- function(quarterly_list, annual_list, data_frame, outfile){
  # unpack data
  list2env(quarterly_list, envir = environment())
  list2env(annual_list, envir = environment())

  # create excel.
  wb <- openxlsx::createWorkbook()
  all_sheets <- c("\\u010detrtletni", "letni","originalni")
  purrr::walk(all_sheets, ~ openxlsx::addWorksheet(wb, .x))

  main_sheets <- list("\\u010detrtletni", "letni")
  main_list <- list(quarterly_list, annual_list)

  # write data and headers
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::writeData(wb, .x, .y[[1]], startRow = 2, startCol = 1))
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::writeData(wb, .x, .y[[2]], startRow = 1, startCol = 1))
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::writeData(wb, .x, .y[[3]], startRow = 2, startCol = 1))
  openxlsx::writeData(wb, "originalni", data_frame, startRow = 1, startCol = 1)
  # merge header cells :)
  purrr::walk2(main_sheets, main_list, function(sheet, cols){
    for (i in seq_along(cols[[2]][-c(1,2)])) {
      openxlsx::mergeCells(wb, sheet, cols = 2+(3 * (i - 1) + 1):(3 * i), rows = 1)
    }
  })
  # set column widths
  purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 1, widths = 45))
  purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 2, widths = 28))
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::setColWidths(wb, .x,
                                        cols = (ncol(.y[[1]])-5):ncol(.y[[1]]), widths = 10))
  # hide columns
  n <- c(6,9)
  purrr::pwalk(list(x = main_sheets, y = main_list, z = n), function(x,y,z){
    openxlsx::setColWidths(wb, x, cols = 3:(ncol(y[[1]])-z),
                           widths = 10, hidden = rep(TRUE, length(3:(ncol(y[[1]])-z))))})
  # numeric format
  style_num = openxlsx::createStyle(numFmt="0.00")
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::addStyle(wb, sheet = .x, style_num, cols = 3:ncol(.y[[1]]),
                                    rows = 1:24, gridExpand = TRUE, stack = TRUE))
  # bolding
  style_bold = openxlsx::createStyle(textDecoration ="bold")
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::addStyle(wb, sheet = .x, style_bold, cols = 1:ncol(.y[[1]]),
                                    rows = 1,gridExpand = TRUE, stack = TRUE))
  purrr::walk(main_sheets,
              ~ openxlsx::addStyle(wb, sheet = .x,style_bold, cols = 1, rows = 1:24,
                                   gridExpand = TRUE, stack = TRUE))
  # wrap and center
  center_wrap_style <- openxlsx::createStyle(halign = "center", valign = "center",  wrapText = TRUE)
  purrr::walk2(main_sheets, main_list,
               ~ openxlsx::addStyle(wb, sheet = .x, center_wrap_style, cols = 1:ncol(.y[[1]]),
                                    rows = 1:2, gridExpand = TRUE, stack = TRUE))
  # freeze
  purrr::walk(main_sheets, ~ openxlsx::freezePane(wb, .x, firstActiveRow = 2))
  purrr::walk(main_sheets, ~ openxlsx::freezePane(wb, .x,  firstActiveCol = 3))

  openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)
}

