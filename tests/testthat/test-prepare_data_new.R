test_that("prepare vintage and data table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    on.exit(DBI::dbDisconnect(con_test), add = TRUE)
    xx <- prepare_vintage_table_and_merge_data_points(test_path("testdata/"), "/test006.csv",
                                                "test_mf", con_test, schema = "platform")
    expect_true(is.list(xx))
    expect_equal(length(xx), 3)
    expect_equal(nrow(xx$monthly_vintages), 5)
    expect_equal(nrow(xx$annual_vintages), 5)
    expect_equal(nrow(xx$final), 65)

  })
})
