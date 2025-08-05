#' this is a one off script to preapre a lookup table for the konto values
#' which takes the unique konto values along with their descriptions
#' and assigns them each the grouping konto (4, 7 or NA) within which
#' their contributions can be calculated.
#'
#' it is then saved to the database to the views schema with the name JF_konti_lookup
#'
con <- make_connection()
file_path <- "C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv"

data_raw <- mf_csv_parser_new(file_path)

konti <- data_raw$series  |>
  dplyr::group_by(konto) |>
  dplyr::summarise(konto = unique(konto),
                   description = unique(description))

codes_to_7 <- c("901", "914", "915")
codes_to_4 <- c("911", "912", "913", "916", "917", "918", "919", "920", "921")
codes_to_na <- c("902", "903", "904", "905", "906", "907", "908")

konti <- konti %>%
  dplyr::mutate(group_code = case_when(
    stringr::str_starts(konto, "44") ~ NA,
    stringr::str_starts(konto, "75") ~ NA,
    stringr::str_starts(konto, "50") ~ NA,
    stringr::str_starts(konto, "55") ~ NA,
    konto %in% codes_to_7 ~ "7",
    konto %in% codes_to_4 ~ "4",
    konto %in% codes_to_na ~ NA,
    TRUE ~ stringr::str_sub(konto, 1, 1)
  ))

# Write to database
DBI::dbWriteTable(con,
                  name = DBI::Id(schema = "views", table = "JF_konto_lookup"),
                  value = konti,
                  overwrite = TRUE
)
