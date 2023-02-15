dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("preparing tables", {
    df <- prepare_table_table(con)
    expect_equal(dim(df), c(5,5))
    df <- prepare_category_table(con)
    expect_equal(dim(df), c(6,3))
    df <- prepare_category_relationship_table(con)
    expect_equal(dim(df), c(5,3))
    df <- prepare_category_table_table(con)
    expect_equal(dim(df), c(5,3))
    expect_equal(df$table_id[1], 24)
    df <- prepare_table_dimensions_table(con)
    expect_equal(dim(df), c(10,3))
    df <- prepare_unit_table(con)
    expect_equal(dim(df), c(1,1))
    df <- prepare_series_table("ZPIZ", con)
    expect_equal(dim(df), c(380,5))
    df <- prepare_series_levels_table("ZPIZ", con)
    expect_equal(dim(df), c(760,3))
  })
})





