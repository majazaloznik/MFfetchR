
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
    dplyr::mutate(code = paste0("MF--", table_name, "--",
                                trim_leading(format(code, scientific = FALSE)))) |>
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

#'
#'
#' #' Write EO tables to Excel
#' #'
#' #' Writes and formats the quarterly and annual and original raw data in a way
#' #' suitable for the EO publication.
#' #'
#' #' @param quarterly_list list output of \link[MFfetchR]{prepare_quarterly_eo}
#' #' @param annual_list list output of \link[MFfetchR]{prepare_annual_eo}
#' #' @param data_frame list output of \link[MFfetchR]{transform_series_eo}
#' #' @param outfile path of file to create
#' #'
#' #' @return nothing, creates excel file
#' #' @export
#' write_excel_kbjf_eo <- function(quarterly_list, annual_list, data_frame, outfile){
#'   # unpack data
#'   list2env(quarterly_list, envir = environment())
#'   list2env(annual_list, envir = environment())
#'
#'   # create excel.
#'   wb <- openxlsx::createWorkbook()
#'   all_sheets <- c("\u010detrtletni", "letni","originalni")
#'   purrr::walk(all_sheets, ~ openxlsx::addWorksheet(wb, .x))
#'
#'   main_sheets <- list("\u010detrtletni", "letni")
#'   main_list <- list(quarterly_list, annual_list)
#'
#'   # write data and headers
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[1]], startRow = 2, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[2]], startRow = 1, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[3]], startRow = 2, startCol = 1))
#'   openxlsx::writeData(wb, "originalni", data_frame, startRow = 1, startCol = 1)
#'   # merge header cells :)
#'   purrr::walk2(main_sheets, main_list, function(sheet, cols){
#'     for (i in seq_along(cols[[2]][-c(1,2)])) {
#'       openxlsx::mergeCells(wb, sheet, cols = 2+(3 * (i - 1) + 1):(3 * i), rows = 1)
#'     }
#'   })
#'   # set column widths
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 1, widths = 45))
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 2, widths = 28))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::setColWidths(wb, .x,
#'                                         cols = (ncol(.y[[1]])-5):ncol(.y[[1]]), widths = 10))
#'   # hide columns
#'   n <- c(6,9)
#'   purrr::pwalk(list(x = main_sheets, y = main_list, z = n), function(x,y,z){
#'     openxlsx::setColWidths(wb, x, cols = 3:(ncol(y[[1]])-z),
#'                            widths = 10, hidden = rep(TRUE, length(3:(ncol(y[[1]])-z))))})
#'   # numeric format
#'   style_num = openxlsx::createStyle(numFmt="0.00")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_num, cols = 3:ncol(.y[[1]]),
#'                                     rows = 1:24, gridExpand = TRUE, stack = TRUE))
#'   # bolding
#'   style_bold = openxlsx::createStyle(textDecoration ="bold")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_bold, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1,gridExpand = TRUE, stack = TRUE))
#'   purrr::walk(main_sheets,
#'               ~ openxlsx::addStyle(wb, sheet = .x,style_bold, cols = 1, rows = 1:24,
#'                                    gridExpand = TRUE, stack = TRUE))
#'   # wrap and center
#'   center_wrap_style <- openxlsx::createStyle(halign = "center", valign = "center",  wrapText = TRUE)
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, center_wrap_style, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:2, gridExpand = TRUE, stack = TRUE))
#'   # freeze
#'   purrr::walk(main_sheets, ~ openxlsx::freezePane(wb, .x, firstActiveRow = 3, firstActiveCol = 3))
#'   openxlsx::freezePane(wb, "originalni", firstActiveRow = 2, firstActiveCol =6)
#'   purrr::walk(all_sheets, ~openxlsx::protectWorksheet(wb, sheet = .x, password = "umar",
#'                                                       lockObjects = FALSE,
#'                                                       lockScenarios = FALSE,
#'                                                       lockFormattingRows = FALSE,
#'                                                       lockFormattingCells = FALSE,
#'                                                       lockFormattingColumns = FALSE))
#'
#'   openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)
#' }
#'
#'
#' #' Write 12mK to Excel
#' #'
#' #' Writes and formats the in a suitable format. Potentially lets you extend
#' #' a plot.
#' #'
#' #' @param monthly_list list output of \link[MFfetchR]{prepare_monthly_12mK}
#' #' @param data_frame list output of \link[MFfetchR]{transform_series_eo}
#' #' @param outfile path of file to create
#' #' @param update logical whether file exists and should be loaded first and then
#' #' updated.
#' #'
#' #' @return nothing, creates excel file
#' #' @export
#' write_excel_kbjf_12mK <- function(monthly_list, data_frame, outfile, update = TRUE){
#'   # unpack data
#'   list2env(monthly_list, envir = environment())
#'
#'   all_sheets <- c("12-mese\u010dne kumulative","originalni")
#'   main_sheets <- list("12-mese\u010dne kumulative")
#'   main_list <- list(monthly_list)
#'   # create excel or load existing
#'   if (update) {
#'     wb <- openxlsx::loadWorkbook(outfile)
#'     purrr::walk2(main_sheets, main_list,
#'                  ~ openxlsx::removeCellMerge(wb, .x,cols = 1:ncol( .y[[1]]), rows = 1))
#'   } else {
#'     wb <- openxlsx::createWorkbook()
#'     purrr::walk(all_sheets, ~ openxlsx::addWorksheet(wb, .x))}
#'
#'   main_sheets <- list("12-mese\u010dne kumulative")
#'   main_list <- list(monthly_list)
#'
#'   # write data and headers
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[1]], startRow = 3, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[2]], startRow = 1, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[3]], startRow = 2, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[4]], startRow = 3, startCol = 1))
#'   openxlsx::writeData(wb, "originalni", data_frame, startRow = 1, startCol = 1)
#'
#'   # create named regions
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::createNamedRegion(wb, .x, cols = c(3:ncol(.y[[1]])),
#'                                              rows = 4, name = "prihodki",
#'                                              overwrite = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::createNamedRegion(wb, .x, cols = c(3:ncol(.y[[1]])),
#'                                              rows = 19, name = "odhodki",
#'                                              overwrite = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::createNamedRegion(wb, .x, cols = c(3:ncol(.y[[1]])),
#'                                              rows = 33, name = "saldo",
#'                                              overwrite = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::createNamedRegion(wb, .x, cols = c(3:ncol(.y[[1]])),
#'                                              rows = 34, name = "primarni_saldo",
#'                                              overwrite = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::createNamedRegion(wb, .x, cols = c(3:ncol(.y[[1]])),
#'                                              rows = 3, name = "meseci",
#'                                              overwrite = TRUE))
#'   # merge header cells :)
#'   purrr::walk2(main_sheets, main_list, function(sheet, cols){
#'     for (i in seq_along(1:(ceiling((ncol(cols[[2]])-2)/12)))) {
#'       openxlsx::mergeCells(wb, sheet, cols = 2+(12 * (i - 1) + 1):(12 * i), rows = 1)
#'     }
#'   })
#'   # set column widths
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 1, widths = 45))
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 2, widths = 28))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::setColWidths(wb, .x,
#'                                         cols = (ncol(.y[[1]])-5):ncol(.y[[1]]), widths = 10))
#'   # hide columns
#'   n <- c(6)
#'   purrr::pwalk(list(x = main_sheets, y = main_list, z = n), function(x,y,z){
#'     openxlsx::setColWidths(wb, x, cols = 3:(ncol(y[[1]])-z),
#'                            widths = 10, hidden = rep(TRUE, length(3:(ncol(y[[1]])-z))))})
#'   # numeric format
#'   style_num = openxlsx::createStyle(numFmt="0.00")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_num, cols = 3:ncol(.y[[1]]),
#'                                     rows = 1:33, gridExpand = TRUE, stack = TRUE))
#'   # bolding
#'   style_bold = openxlsx::createStyle(textDecoration ="bold")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_bold, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:3,gridExpand = TRUE, stack = TRUE))
#'   # font
#'   style_font = openxlsx::createStyle(fontName = "Calibri")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_font, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:33,gridExpand = TRUE, stack = TRUE))
#'   purrr::walk2(list("originalni"), list(data_frame),
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_font, cols = 1:ncol(.y),
#'                                     rows = 1:nrow(.y),gridExpand = TRUE, stack = TRUE))
#'   # highlight
#'   style_yello <- openxlsx::createStyle( fgFill = "#fdf113", bgFill = "#fdf113")
#'
#'   style_lilac <- openxlsx::createStyle( fgFill = "#e38dfc", bgFill = "#e38dfc")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_yello, cols = 1:ncol(.y[[1]]),
#'                                     rows = c(4, 19, 34),  gridExpand = TRUE, stack = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_lilac, cols = 1:ncol(.y[[1]]),
#'                                     rows = c(33),  gridExpand = TRUE, stack = TRUE))
#'
#'   # wrap and center
#'   center_wrap_style <- openxlsx::createStyle(halign = "center", valign = "center",  wrapText = TRUE)
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, center_wrap_style, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:3, gridExpand = TRUE, stack = TRUE))
#'   # freeze
#'   purrr::walk(main_sheets, ~ openxlsx::freezePane(wb, .x, firstActiveRow = 4, firstActiveCol = 3))
#'   openxlsx::freezePane(wb, "originalni", firstActiveRow = 2, firstActiveCol =5)
#'
#'   purrr::walk(all_sheets, ~openxlsx::protectWorksheet(wb, sheet = .x, password = "umar",
#'                                                       lockObjects = FALSE,
#'                                                       lockScenarios = FALSE,
#'                                                       lockFormattingRows = FALSE,
#'                                                       lockFormattingCells = FALSE,
#'                                                       lockFormattingColumns = FALSE))
#'   openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)
#' }
#'
#'
#'
#' #' Write stats appendix to Excel
#' #'
#' #' Writes and formats the in a suitable format.
#' #'
#' #' @param stats_appendix_list list output of \link[MFfetchR]{prepare_stats_appendix}
#' #' @param data_frame list output of \link[MFfetchR]{transform_series_12mK}
#' #' @param outfile path of file to create
#' #'
#' #' @return nothing, creates excel file
#' #' @export
#' write_excel_stats_appendix <- function(stats_appendix_list, data_frame, outfile){
#'   # unpack data
#'   list2env(stats_appendix_list, envir = environment())
#'
#'   all_sheets <- c("statisti\u010dna priloga","originalni")
#'   main_sheets <- list("statisti\u010dna priloga")
#'   main_list <- list(stats_appendix_list)
#'   # create excel
#'   wb <- openxlsx::createWorkbook()
#'   purrr::walk(all_sheets, ~ openxlsx::addWorksheet(wb, .x))
#'
#'   # write data and headers
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[1]], startRow = 2, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[2]], startRow = 1, startCol = 1))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::writeData(wb, .x, .y[[3]], startRow = 2, startCol = 1))
#'   openxlsx::writeData(wb, "originalni", data_frame, startRow = 1, startCol = 1)
#'
#'
#'   merged_indices <- rle(colnames(stats_appendix_list[[2]])[-c(1,2)]) %>%
#'     with(data.frame(values, lengths)) %>%
#'     dplyr::mutate(start = 2 + cumsum(lengths) - lengths + 1,
#'                   end = 2 + cumsum(lengths)) %>%
#'     dplyr:: select(start, end)
#'
#'   # merge header cells :)
#'   purrr::walk2(main_sheets, main_list, function(sheet, cols){
#'     for (i in 4:nrow(merged_indices)) {
#'       openxlsx::mergeCells(wb, sheet, cols = merged_indices[i,1]:merged_indices[i,2],
#'                            rows = 1)
#'     }
#'   })
#'   openxlsx::mergeCells(wb, "statisti\u010dna priloga", cols = 3, rows = 1:2)
#'   openxlsx::mergeCells(wb, "statisti\u010dna priloga", cols = 4, rows = 1:2)
#'   openxlsx::mergeCells(wb, "statisti\u010dna priloga", cols = 5, rows = 1:2)
#'
#'   # set column widths
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 1, widths = 45))
#'   purrr::walk(main_sheets, ~ openxlsx::setColWidths(wb, .x, cols = 2, widths = 28))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::setColWidths(wb, .x,
#'                                         cols = 3:ncol(.y[[1]]), widths = 10))
#'
#'   # numeric format
#'   style_num = openxlsx::createStyle(numFmt="0.00")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_num, cols = 3:ncol(.y[[1]]),
#'                                     rows = 1:33, gridExpand = TRUE, stack = TRUE))
#'   # bolding
#'   style_bold = openxlsx::createStyle(textDecoration ="bold")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_bold, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:2,gridExpand = TRUE, stack = TRUE))
#'   # font
#'   style_font = openxlsx::createStyle(fontName = "Calibri")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_font, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:33,gridExpand = TRUE, stack = TRUE))
#'   purrr::walk2(list("originalni"), list(data_frame),
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_font, cols = 1:ncol(.y),
#'                                     rows = 1:nrow(.y),gridExpand = TRUE, stack = TRUE))
#'   # highlight
#'   style_yello <- openxlsx::createStyle( fgFill = "#fdf113", bgFill = "#fdf113")
#'
#'   style_lilac <- openxlsx::createStyle( fgFill = "#e38dfc", bgFill = "#e38dfc")
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_yello, cols = 1:ncol(.y[[1]]),
#'                                     rows = c(3, 18, 33),  gridExpand = TRUE, stack = TRUE))
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, style_lilac, cols = 1:ncol(.y[[1]]),
#'                                     rows = c(32),  gridExpand = TRUE, stack = TRUE))
#'
#'   # wrap and center
#'   center_wrap_style <- openxlsx::createStyle(halign = "center", valign = "center",  wrapText = TRUE)
#'   purrr::walk2(main_sheets, main_list,
#'                ~ openxlsx::addStyle(wb, sheet = .x, center_wrap_style, cols = 1:ncol(.y[[1]]),
#'                                     rows = 1:3, gridExpand = TRUE, stack = TRUE))
#'   # freeze
#'   purrr::walk(main_sheets, ~ openxlsx::freezePane(wb, .x, firstActiveRow = 3, firstActiveCol = 3))
#'   openxlsx::freezePane(wb, "originalni", firstActiveRow = 2, firstActiveCol =5)
#'
#'   purrr::walk(all_sheets, ~openxlsx::protectWorksheet(wb, sheet = .x, password = "umar",
#'                                                       lockObjects = FALSE,
#'                                                       lockScenarios = FALSE,
#'                                                       lockFormattingRows = FALSE,
#'                                                       lockFormattingCells = FALSE,
#'                                                       lockFormattingColumns = FALSE))
#'   openxlsx::saveWorkbook(wb, file = outfile, overwrite = TRUE)
#' }


