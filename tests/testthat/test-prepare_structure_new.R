test_that("multiplication works", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
  xx <- prepare_dimension_levels_table_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                                           "KBJF", con_test)
  expect_equal(nrow(xx), 2177)
  expect_equal(ncol(xx), 3)
  expect_true(all(c("level_value", "level_text" , "tab_dim_id" ) %in% colnames(xx)))
  xx <- prepare_series_table_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                                 "KBJF", con_test)
  expect_equal(dim(xx), c(4350,5))
  expect_true(all(c("table_id", "name_long" , "code", "unit_id", "code", "interval_id" ) %in% colnames(xx)))
  })
})
