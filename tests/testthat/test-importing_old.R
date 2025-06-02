test_that("insert strucutre and data points", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    results <- MF_import_structure_old("OB", con_test, schema = "platform",
                                       keep_vintage = FALSE,
                                       testthat::test_path("../../tests/testthat/testdata/zadnje_stare/"))

    expect_equal(length(results), 9)
    expect_equal(results$table$count, 1)
    expect_equal(dim(results$category), c(2,1))
    expect_equal(dim(results$category_relationship), c(1,1))
    expect_equal(dim(results$category_table), c(1,1))
    expect_equal(dim(results$table_dimensions), c(2,1))
    expect_equal(dim(results$dimension_levels), c(273,1))
    expect_equal(dim(results$units), c(1,1))
    expect_equal(dim(results$series), c(542,1))
    expect_equal(dim(results$series_levels), c(1084,1))

    results <- MF_import_data_points_old(testthat::test_path("../../tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
                    "OB", "OBCINE", con_test, schema = "platform")
    expect_equal(length(results), 2)
    expect_equal(results[[1]], 271)
    expect_equal(results[[2]], 271)

  })
})
