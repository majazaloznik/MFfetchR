test_that("full konto list", {
  konto_list <- get_konto_list_full(test_path("testdata/"), paste0(test_path("testdata/"), "/Export_EK_2025-08-29_10-11-35.csv"))
  expect_equal(nrow(konto_list), 1826)
  konto_list <- get_konto_list_data(test_path("testdata/"), paste0(test_path("testdata/"), "/test002.csv"))
  expect_equal(nrow(konto_list), 24)
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    levels <- get_db_konto_list(con_test)
    expect_equal(nrow(levels), 1677)
    check <- check_for_extra_kontos(test_path("testdata/"), paste0(test_path("testdata/"),
                                                          "/test002.csv"), con_test)
    expect_equal(check, FALSE)
  })
})
