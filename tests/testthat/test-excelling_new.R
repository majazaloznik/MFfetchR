test_that("csv parsing works", {

  l <- mf_csv_parser_new("C:/osebno/ZaloznikM37/bekapiranje/MFfetchR/inst/nove_tabele/Table_4BJF_BI_DATA/Table_4BJF_BI_DATA_1.csv")
  expect_true(length(l) == 3)
  expect_true(all(c("period_id", "code", "value") %in% names(l$monthly)))
  expect_true(all(c("period_id", "code", "value") %in% names(l$annual)))
  expect_true(all(c("konto", "blg", "description") %in% names(l$series)))
  expect_equal(nrow(l$series), 8342)
})
