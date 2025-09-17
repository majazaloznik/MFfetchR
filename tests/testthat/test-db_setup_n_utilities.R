test_that("get most recent file", {
  # Create temporary files with known timestamps
  temp_dir <- tempdir()
  file1 <- file.path(temp_dir, "test001.csv")
  file2 <- file.path(temp_dir, "test002.csv")
  file3 <- file.path(temp_dir, "test_9xx.csv")

  # Create files
  writeLines("data", file1)
  writeLines("data", file2)
  writeLines("data", file3)

  # Set specific timestamps
  Sys.setFileTime(file1, Sys.time() - 3600)  # 1 hour ago
  Sys.setFileTime(file2, Sys.time())         # Now (most recent)
  Sys.setFileTime(file3, Sys.time() - 7200)  # 2 hours ago

  x <- get_most_recent_file_from_pattern(temp_dir, "^test.*\\.csv$")
  expect_equal(basename(x), "test002.csv")

  # Clean up
  unlink(c(file1, file2, file3))
})


test_that("update_JF_lookup_table_on_db", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()

    local_mock <- mockery::mock(1677L)
    mockery::stub(update_JF_lookup_table_on_db, 'DBI::dbAppendTable', local_mock)

    out <- update_JF_lookup_table_on_db(
      "O:/Avtomatizacija/umar-automation-scripts/data/mf_bilance/new_data/",
      con_test
    )

    expect_equal(out, 1677)
  })
})
