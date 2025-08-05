con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "platform",
                      host = "192.168.38.21",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"),
                      client_encoding = "utf8")

library(MFfetchR)
meta <- MFfetchR:::meta
# For structure import
struc_results <- purrr::map2(
  meta$table_name,
  meta$file_path,
  ~ MF_import_structure_old(
    .x,
    con,
    schema = "platform",
    keep_vintage = FALSE,
    file.path("tests/testthat/testdata/zadnje_stare"))
)


# For data import
data_results <- purrr::pmap(
  list(meta$file_path, meta$table_name, meta$sheet_name),
  ~ MF_import_data_points_old(
    testthat::test_path(file.path("testdata/zadnje_stare", paste0(..1, "_1992-2025.xlsx"))),
    ..2,
    ..3,
    con,
    schema = "platform"
  )
)

# Name the results for easier access
names(struc_results) <- meta$table_name
names(data_results) <- meta$table_name
