test_that("prepare dimension levels table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    on.exit(DBI::dbDisconnect(con_test), add = TRUE)
    out <- prepare_dimension_levels_table_new(test_path("testdata/test001.csv"),
                                              "KBJF", con_test, schema = "platform")
    expect_equal(nrow(out), 42)
    expect_equal(ncol(out), 3)
    expect_true(all(c("level_value", "level_text" , "tab_dim_id" ) %in% colnames(out)))
    out <- prepare_series_table_new(test_path("testdata/test001.csv"),
                                    "KBJF", con_test)
    expect_equal(dim(out), c(80,5))
    expect_true(all(c("table_id", "name_long" , "code", "unit_id", "code",
                      "interval_id" ) %in% colnames(out)))
    out <- prepare_series_levels_table_new("KBJF", con_test, schema = "platform")
    expect_equal(nrow(out), 4760)
    expect_equal(ncol(out), 3)
    expect_true(all(c("series_id", "tab_dim_id" , "level_value" ) %in% colnames(out)))
  })
})
