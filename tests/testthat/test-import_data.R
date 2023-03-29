dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("vintage tables", {
    kbjf <- prepare_vintage_table(test_path("testdata", "kbjf_test.xlsx"), "KBJF", "GLOBALNA", con )
    expect_equal(length(kbjf), 3)
    expect_equal(dim(kbjf[[1]]), c(256,2))
    expect_equal(names(kbjf), c("monthly_vintages", "annual_vintages", "parsed_data"))
    parsed <- readRDS(test_path("testdata", "parsed.rds"))
    out <- prepare_data_table(parsed, con)
    expect_equal(dim(out), c(200,4))
  })
})
