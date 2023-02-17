test_that("trims work", {
  expect_equal(trim_leading(" kj"), "kj")
  expect_equal(trim_leading("kj "), "kj ")
  expect_equal(trim_inter("2 0 0 1"), "2001")
  expect_equal(trim_inter("2 0 0 1 "), "2001")
  expect_equal(month_codes("MAJ"), "M05")
  expect_error(month_codes("MAY"))
})
