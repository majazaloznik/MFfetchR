
test_that("parsing works", {
  l <- mf_excel_parser(test_path("testdata", "dp_test.xlsx"), "DP", "MESPROR")
  expect_equal(nrow(l[[2]]), 0)
  expect_equal(nrow(l[[3]]), 49)
  l <- mf_excel_parser(test_path("testdata", "kbjf_test.xlsx"), "KBJF", "GLOBALNA")
  expect_equal(ncol(l[[3]]), 2)
  expect_equal(nrow(l[[3]]), 256)
  expect_equal(ncol(l[[1]]), 3)
})
