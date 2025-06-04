
test_that("parsing works", {
  l <- mf_excel_parser(testthat::test_path("testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
                       "OB", "OBCINE")
  expect_equal(nrow(l[[1]]), 83739)
  expect_equal(nrow(l[[2]]), 9214)
  expect_equal(nrow(l[[3]]), 271)

})
