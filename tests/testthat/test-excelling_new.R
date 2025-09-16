test_that("csv parsing works", {

  l <- mf_csv_parser_new(test_path("testdata/"), "/test001.csv")
  expect_true(length(l) == 3)
  expect_true(all(c("period_id", "code", "value") %in% names(l$monthly)))
  expect_true(all(c("period_id", "code", "value") %in% names(l$annual)))
  expect_true(all(c("konto", "blg", "description") %in% names(l$series)))
  expect_equal(nrow(l$monthly), 40)
  expect_equal(nrow(l$annual), 0)
  expect_equal(nrow(l$series), 40)
  l <- mf_csv_parser_new(test_path("testdata/"), "/test002.csv")
  expect_true(length(l) == 3)
  expect_equal(nrow(l$monthly), 132)
  expect_equal(nrow(l$annual), 11)
  expect_equal(nrow(l$series), 11)
})

test_that("function stops with empty data", {
  empty_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(), empty_file, row.names = FALSE)
  expect_error(
    mf_csv_parser_new("", empty_file),
    "There was no data read\\."
  )
  unlink(empty_file)

})
test_that("Missing column", {
  expect_error(
    mf_csv_parser_new(test_path("testdata/"), "/test005.csv"),
    "Missing required columns"
  )
})
