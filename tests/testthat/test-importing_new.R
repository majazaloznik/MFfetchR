test_that("importing structure from test table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    on.exit(DBI::dbDisconnect(con_test), add = TRUE)
    xx <- MF_import_structure_new(test_path("testdata/test006.csv"), "UTF-8",
                                  "test_mf", con_test, schema = "platform")
    expect_true(is.list(xx))
    expect_equal(length(xx), 3)
    expect_true(all(sapply(xx, function(x) is.numeric(x))))
    expect_true(all(sapply(xx, function(x) x > 0)))
    expect_equal(xx$dimension_levels, 5)
    expect_equal(xx$series,10)
    expect_equal(xx$series_levels, 20)
  })
})

test_that("importing data from test table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    on.exit(DBI::dbDisconnect(con_test), add = TRUE)
    xx <- MF_import_data_points_new(test_path("testdata/test006.csv"), "UTF-8",
                              "test_mf", con_test, schema = "platform")
    expect_true(is.list(xx))
    expect_equal(length(xx), 2)
    expect_equal(xx[[1]], 5)
    expect_equal(xx[[2]], 5)
  })
})


