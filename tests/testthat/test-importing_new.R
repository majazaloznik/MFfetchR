test_that("multiplication works", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    xx <- MF_import_structure_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv",
                                  "OB", con_test, schema = "platform", keep_vintage = TRUE)
    expect_true(is.list(xx))
    expect_equal(length(xx), 3)
    expect_true(all(sapply(xx, function(x) is.numeric(x))))
    expect_true(all(sapply(xx, function(x) x > 0)))
    expect_true(xx$dimension_levels == 553)
    expect_true(xx$series == 1106)
    expect_true(xx$series_levels == 2212)
  })
})
