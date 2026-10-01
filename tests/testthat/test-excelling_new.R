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

test_that("UTF-16LE exports parse identically to UTF-8", {
  utf8  <- read_mf_csv(test_path("testdata/test002.csv"), decimal_mark = ",")
  expect_message(
    utf16 <- read_mf_csv(test_path("testdata/test002_utf16le.csv"), decimal_mark = ","),
    "UTF-16LE")
  expect_identical(names(utf16), names(utf8))
  expect_equal(as.data.frame(utf16), as.data.frame(utf8))
  l8  <- mf_csv_parser_new(test_path("testdata/"), "/test002.csv")
  l16 <- suppressMessages(mf_csv_parser_new(test_path("testdata/"), "/test002_utf16le.csv"))
  expect_equal(l16$monthly, l8$monthly)
  expect_equal(l16$annual, l8$annual)
})

test_that("file patterns ignore ad-hoc exports", {
  f <- c("Export_4BJF_2026-09-30_11-44-39.csv", "Export_4BJF_ZZZZ_2026-09-30_11-45-56.csv",
         "Export_EK_2026-09-30_11-44-42.csv")
  expect_equal(grep(mf_file_patterns[["bjf"]], f, value = TRUE), f[1])
  expect_equal(grep(mf_file_patterns[["ek"]], f, value = TRUE), f[3])
})
