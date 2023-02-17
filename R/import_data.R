#' Prepare table to insert into `vintage` table
#'
#' Helper function that prepares the vintage table with the new vintages.
#' First checks if new vintages are even necessary, by parsing the excel file
#' to get the most recent month, (and year?) and then checking with the most
#' recent period in the database. If there is new data,
#' Prepare a dataframe with the `series_id`s of the table and the current time
#' as their publication date - since we don't have anything better.
#'
#' Returns table ready to insert into the `vintage`table with the
#' db_writing family of functions.
#'
#'
#'
#' @return a dataframe with the `series_id` and `published` columns
#' for all the series in this table.
#' @export
#'
#
# prepare_vintage_table <- function(file_path, table_name, sheet_name, con){
#   get_table_id(code_no, con) -> tbl_id
#   get_px_metadata(code_no)$updated -> published
#   get_last_publication_date(tbl_id, con) -> last_published
#   if(identical(published, last_published)) {
#     stop(paste0("These vintages for table ", code_no,
#                 "are not new, they will not be inserted again."))
#   } else {
#     get_time_dimension(code_no, con) -> time_dimension
#     get_interval_id(time_dimension) -> interval_id
#     expand_to_level_codes(code_no, con) -> expanded_level_codes
#     expanded_level_codes %>%
#       tidyr::unite("series_code", dplyr::starts_with("Var"), sep = "--") %>%
#       dplyr::mutate(series_code = paste0("SURS--", code_no, "--",
#                                          series_code, "--",interval_id)) -> x
#     get_series_id(x$series_code, con) -> series_ids
#     get_series_id_from_table(tbl_id, con) -> double_check
#     if(isTRUE(all.equal(sort(series_ids), sort(double_check)))){
#       data.frame(series_id = series_ids,
#                  published = published) } else {
#                    warning(paste("The newly published data in table", code_no,
#                                  "seems to have a different structure to the series already",
#                                  "in the database. The vintages were not imported, update",
#                                  "the series table first."))}
#   }
# }
#
