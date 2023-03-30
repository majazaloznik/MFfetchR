#' Helper to rename value column into seires name
#'
#' @param series_name character
#' @param df data frame with a column named `value`
#'
#' @return same dataframe, with renamed column
#' @keywords internal
rename_value_column <- function(series_name, df) {
  df %>%
    dplyr::rename(!!series_name := value) -> df
  df
}

#' Loading function
#'
#' Loads all the series from the list of series codes, by first getting the list
#' of the newest vintage numbers. Theg gets the data, renames the value column
#' into the series code
#'
#' @param series_list list of character objects of series codes
#' @param con connection to database
#'
#' @return dataframe of all series in the list
#' @export
load_series_as_df <- function(series_list, con) {
  vintage_list <- purrr::map(series_list, UMARaccessR::get_vintage_from_series_code, con)
  vintage_list <- purrr::map(vintage_list, `[[`, 1)
  data_list <- purrr::map(vintage_list, get_data_points_from_vintage, con)
  data_list_final <- purrr::map2(series_list, data_list, rename_value_column)
  data_frame <- Reduce(function(...) merge(..., all=T), data_list_final)
  data_frame <- data_frame %>% select(period_id, sort(colnames(data_frame)[-1]))
  data_frame
}


#' Transform series needed for the EO tables
#'
#' From the raw_data_frame output of load_series_as_df using kbjf_series_list_oe as
#' the series list, this function calculates the necessary series and transforms
#' the values into millions.
#'
#' @param raw_data_frame dataframe with series from the kbjf_series_list_oe list
#'
#' @return dataframe with some additional series calculated
#' @export
#'
transform_series_eo <- function(raw_data_frame){
  raw_data_frame %>%
    dplyr::mutate(`MF_UMAR--KBJF--003--700--M` = `MF--KBJF--003--70--M` - `MF--KBJF--007--701--M`,
                  `MF_UMAR--KBJF--050--700--M` = `MF--KBJF--050--72--M`+ `MF--KBJF--063--73--M` + `MF--KBJF--073--74--M`,
                  `MF_UMAR--KBJF--092--400--M` = `MF--KBJF--094--400--M` + `MF--KBJF--095--413300--M`+
                    `MF--KBJF--097--401--M` + `MF--KBJF--098--413301--M` + `MF--KBJF--099--413310--M`,
                  `MF_UMAR--KBJF--101--400--M` = `MF--KBJF--101--402--M` + `MF--KBJF--102--413302--M`,
                  `MF_UMAR--KBJF--103--400--M` = `MF--KBJF--103--403--M` + `MF--KBJF--110--404--M`,
                  `MF_UMAR--KBJF--119--400--M` = `MF--KBJF--119--41--M` - `MF--KBJF--124--411--M`,
                  `MF_UMAR--KBJF--148--400--M` = `MF--KBJF--148--42--M` + `MF--KBJF--160--43--M`) %>%
    dplyr::select(-c(`MF--KBJF--003--70--M`,`MF--KBJF--050--72--M`, `MF--KBJF--063--73--M`, `MF--KBJF--073--74--M`,
              `MF--KBJF--094--400--M`, `MF--KBJF--097--401--M`,`MF--KBJF--103--403--M`, `MF--KBJF--110--404--M`,
              `MF--KBJF--119--41--M`, `MF--KBJF--148--42--M`, `MF--KBJF--160--43--M`,
              `MF--KBJF--095--413300--M`, `MF--KBJF--098--413301--M`, `MF--KBJF--099--413310--M`,
              `MF--KBJF--102--413302--M`, `MF--KBJF--101--402--M`)) %>%
    dplyr::mutate(year = as.numeric(substr(period_id, 1,4)),
           month = substr(period_id, 6,7),
           month_roman = as.roman(month),
           quarter = paste0(year,"Q",lubridate::quarter(as.numeric(month)))) %>%
    dplyr::relocate(period_id, year, month, month_roman, quarter) %>%
    dplyr::relocate(`MF_UMAR--KBJF--003--700--M`, .after = `MF--KBJF--001--7--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--050--700--M`, .before = `MF--KBJF--091--4--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--092--400--M`, .after = `MF--KBJF--091--4--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--119--400--M`, .after = `MF--KBJF--124--411--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--148--400--M`, .before = `MF--KBJF--173--45--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--101--400--M`, .after = `MF_UMAR--KBJF--092--400--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--103--400--M`, .after = `MF_UMAR--KBJF--101--400--M`) %>%
    dplyr::relocate(`MF--KBJF--007--701--M`, .before = `MF--KBJF--035--71--M`) %>%
    dplyr::mutate(dplyr::across(-c(1:5), ~ .x/1000000))
}


#' Transform series needed for the 12month cumulative tables
#'
#' From the raw_data_frame output of load_series_as_df using kbjf_series_list_12mK as
#' the series list, this function calculates the necessary series and transforms
#' the values into millions.
#'
#' @param raw_data_frame dataframe with series from the kbjf_series_list_12mK list
#'
#' @return dataframe with some additional series calculated
#' @export
#'
transform_series_12mK <- function(raw_data_frame){
  raw_data_frame %>%
    dplyr::mutate(`MF_UMAR--KBJF--093--400--M` = `MF--KBJF--093--911--M` +  `MF--KBJF--096--912--M`,
                  `MF_UMAR--KBJF--103--400--M` = `MF--KBJF--103--403--M` + `MF--KBJF--110--404--M`,
                  `MF_UMAR--KBJF--133--400--M` = `MF--KBJF--134--412--M` +`MF--KBJF--136--413--M`,) %>%
    dplyr::select(-c(`MF--KBJF--093--911--M`, `MF--KBJF--096--912--M`,
                     `MF--KBJF--103--403--M`, `MF--KBJF--110--404--M`,
                     `MF--KBJF--134--412--M`, `MF--KBJF--136--413--M`)) %>%
    dplyr::mutate(year = as.numeric(substr(period_id, 1,4)),
                  month = substr(period_id, 6,7),
                  date = lubridate::ym(period_id)) %>%
    dplyr::relocate(period_id, year, month, date) %>%
    dplyr::relocate(`MF_UMAR--KBJF--093--400--M`, .after = `MF--KBJF--092--40--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--103--400--M`, .after = `MF--KBJF--100--913--M`) %>%
    dplyr::relocate(`MF_UMAR--KBJF--133--400--M`, .after = `MF--KBJF--124--411--M`) %>%
    dplyr::mutate(dplyr::across(-c(1:4), ~ .x/1000000))
}

#' Calculate and prepare the table for quarterly data for EO
#'
#' This function aggregates the amounts over quarters (only for complete quarters)
#' then calculates the year-on-year growth rates, and the relative contributions
#' for each account and formats the table appropriately. It also prepares the
#' two header rows required for a pretty (but def not tidy) Excel presentation.
#'
#' @param data_frame output from transform_series_eo
#'
#' @return list of three objects, the main table and two header rows.
#' @export
prepare_quarterly_eo <- function(data_frame) {
  # summarise to quarterly and calculate rates of change and contributions.
  data_frame %>%
    dplyr::group_by(quarter) %>%
    dplyr::mutate(no_months = n()) %>%
    dplyr::filter(no_months == 3) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric),~ sum(.x, na.rm = TRUE))) %>%
    dplyr::select(-month_roman, -no_months, -year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--[47]+[^-]*--M$", perl = TRUE),
                  ~ .x/lag(.x, 4)*100-100,
                  .names = "{.col}___st_rasti")) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--7[^-]*--M$", perl = TRUE),
                  ~ lag(.x, 4)/lag(`MF--KBJF--001--7--M`,4) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                  .names = "{.col}___prispevek"))  %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--4[^-]*--M$", perl = TRUE),
                  ~ lag(.x, 4)/lag(`MF--KBJF--091--4--M`, 4) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                  .names = "{.col}___prispevek")) %>%
    tidyr::pivot_longer(!quarter) %>%
    tidyr::pivot_wider(names_from = quarter, values_from = value) -> intermediate_quarterly

  intermediate_quarterly %>%
    tidyr::separate(name, into = c("name", "suffix"), sep ="__", fill = "right") %>%
    dplyr::mutate(suffix = ifelse(is.na(suffix), "", suffix)) %>%
    tidyr::pivot_wider(names_from = suffix, values_from = colnames(intermediate_quarterly)[-1],
                       names_sep = "") %>%
    dplyr::mutate(kazalnik = kbjf_row_names_eo) %>%
    dplyr::rename(koda = name) %>%
    dplyr::relocate(kazalnik) -> quarterly
  # prepare headers
  header_year_quarterly <- regmatches(colnames(quarterly)[-c(1:2)],
                                      regexpr("^([^_]+)", colnames(quarterly)[-c(1:2)]))
  header_year_quarterly <- c("", "", header_year_quarterly)
  header_year_quarterly_df <- data.frame(matrix(ncol = length(header_year_quarterly), nrow = 0))
  colnames(header_year_quarterly_df) <- header_year_quarterly

  header_indicator_quarterly <- rep_len(c("Znesek", "Medletna rast, v %", "Prispevek k rasti, v o.t."),
                                        length(header_year_quarterly)-2)
  header_indicator_quarterly <- c("Kazalnik", "Koda", header_indicator_quarterly)
  header_indicator_quarterly_df <- data.frame(matrix(ncol = length(header_indicator_quarterly), nrow = 0))
  colnames(header_indicator_quarterly_df) <- header_indicator_quarterly
  # return list
  mget(c("quarterly", "header_year_quarterly_df", "header_indicator_quarterly_df"))
}

#' Calculate and prepare the table for annual data for EO
#'
#' This function aggregates the amounts over years (also for the last incomplete year)
#' then calculates the year-on-year growth rates, and the relative contributions
#' for each account and formats the table appropriately. It also prepares the
#' two header rows required for a pretty (but def not tidy) Excel presentation.
#'
#' @param data_frame output from  \link[MFfetchR]{transform_series_eo}
#'
#' @return list of three objects, the main table and two header rows.
#' @export
prepare_annual_eo <- function(data_frame) {
  # summarise to annual and calculate rates of change and contributions.
  data_frame %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(no_months = n()) %>%
    dplyr::filter(no_months == 12) %>%
    dplyr::mutate(full_year = paste0(first(month_roman), "-", last(month_roman), " ", year))  %>%
    dplyr::ungroup() %>%
    dplyr::group_by(full_year) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric),~ sum(.x, na.rm = TRUE))) %>%
    dplyr::select(-month_roman, -no_months, -year) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--[47]+[^-]*--M$", perl = TRUE),
                  ~ .x/lag(.x, 1)*100-100,
                  .names = "{.col}___st_rasti")) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--7[^-]*--M$", perl = TRUE),
                  ~ lag(.x,1)/lag(`MF--KBJF--001--7--M`,1) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                  .names = "{.col}___prispevek"))  %>%
    dplyr::mutate(dplyr::across(dplyr::matches("--4[^-]*--M$", perl = TRUE),
                  ~ lag(.x, 1)/lag(`MF--KBJF--091--4--M`, 1) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                  .names = "{.col}___prispevek")) %>%
    tidyr::pivot_longer(!full_year) %>%
    tidyr::pivot_wider(names_from = full_year, values_from = value) -> intermediate_annual

  intermediate_annual %>%
    tidyr::separate(name, into = c("name", "suffix"), sep ="__", fill = "right") %>%
    dplyr::mutate(suffix = ifelse(is.na(suffix), "", suffix)) %>%
    tidyr::pivot_wider(names_from = suffix, values_from = colnames(intermediate_annual)[-1],
                       names_sep = "")  %>%
    dplyr::mutate(kazalnik = kbjf_row_names_eo) %>%
    dplyr::rename(koda = name) %>%
    dplyr::relocate(kazalnik) -> annual

  if(!nrow(data_frame) %% 12 == 0){
    n <- nrow(data_frame) %% 12
    max_year <- max(data_frame$year)

    data_frame %>%
      dplyr::filter(year >= max_year-3,
             as.integer(month_roman) <= n) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(full_year = paste0(first(month_roman), "-", last(month_roman), " ", year))  %>%
      dplyr::ungroup() %>%
      dplyr::group_by(full_year) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric),~ sum(.x, na.rm = TRUE))) %>%
      dplyr::select(-month_roman, -year) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(dplyr::matches("--[47]+[^-]*--M$", perl = TRUE),
                    ~ .x/lag(.x, 1)*100-100,
                    .names = "{.col}___st_rasti")) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("--7[^-]*--M$", perl = TRUE),
                    ~ lag(.x,1)/lag(`MF--KBJF--001--7--M`,1) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                    .names = "{.col}___prispevek"))  %>%
      dplyr::mutate(dplyr::across(dplyr::matches("--4[^-]*--M$", perl = TRUE),
                    ~ lag(.x, 1)/lag(`MF--KBJF--091--4--M`, 1) * get(paste0(dplyr::cur_column(), "___st_rasti")),
                    .names = "{.col}___prispevek")) %>%
      tidyr::pivot_longer(!full_year) %>%
      tidyr::pivot_wider(names_from = full_year, values_from = value) -> intermediate_partial

    intermediate_partial %>%
      tidyr::separate(name, into = c("name", "suffix"), sep ="__", fill = "right") %>%
      dplyr::mutate(suffix = ifelse(is.na(suffix), "", suffix)) %>%
      tidyr::pivot_wider(names_from = suffix, values_from = colnames(intermediate_partial)[-1],
                         names_sep = "")  %>%
      dplyr::mutate(kazalnik = kbjf_row_names_eo) %>%
      dplyr::rename(koda = name) %>%
      dplyr::relocate(kazalnik) -> partial
    annual <- dplyr::bind_cols(annual, partial[,6:14])
  }
  # prepare double headers
  header_year_annual <- regmatches(colnames(annual)[-c(1:2)], regexpr("^([^_]+)", colnames(annual)[-c(1:2)]))
  header_year_annual <- c("", "", header_year_annual)
  header_year_annual_df <- data.frame(matrix(ncol = length(header_year_annual), nrow = 0))
  colnames(header_year_annual_df) <- header_year_annual


  header_indicator_annual <- rep_len(c("Znesek", "Medletna rast, v %", "Prispevek k rasti, v o.t."),
                                     length(header_year_annual)-2)
  header_indicator_annual <- c("Kazalnik", "Koda", header_indicator_annual)
  header_indicator_annual_df <- data.frame(matrix(ncol = length(header_indicator_annual), nrow = 0))
  colnames(header_indicator_annual_df) <- header_indicator_annual
  # return list
  mget(c("annual", "header_year_annual_df", "header_indicator_annual_df"))
}

#' Prepare data for the 12 month cumulative table
#'
#' This function calculates the 12 month cumulative sums and the preps the
#' table and headers for exporting into excel
#'
#' @param data_frame output from  \link[MFfetchR]{transform_series_12mK}
#'
#' @return list of four tables: the first with the monthly data, the other three
#' are headers for the fancy excel three-row heaedr
#' @export
prepare_monthly_12mK <- function(data_frame){
  monthly <- data_frame %>%
    dplyr::mutate(dplyr::across(dplyr::matches(".*--M$"), ~ zoo::rollapplyr(
      .x, width = 12, FUN = sum, fill = NA, partial = FALSE))) %>%
    dplyr::select(-c(year, month, date)) %>%
    tidyr::pivot_longer(!c(period_id)) %>%
    tidyr::pivot_wider(names_from = period_id, values_from = value) %>%
    dplyr::mutate(kazalnik = kbjf_row_names_12Mk) %>%
    dplyr::rename(koda = name) %>%
    dplyr::relocate(kazalnik)
  header_year_df <- data.frame(matrix(ncol = 2+length(data_frame$year), nrow = 0))
  colnames(header_year_df) <- c("", "", data_frame$year)
  header_month_df <- data.frame(matrix(ncol = 2+length(data_frame$year), nrow = 0))
  colnames(header_month_df) <- c("", "", data_frame$month)
  header_date_df <- data.frame(matrix(ncol = 2+length(data_frame$year), nrow = 0))
  colnames(header_date_df) <- c("Kazalnik", "Koda", format(data_frame$date, format = "%b %Y"))
  # return list
  mget(c("monthly", "header_year_df", "header_month_df", "header_date_df"))
}
