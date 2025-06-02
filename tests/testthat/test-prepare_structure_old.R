test_that("prepare source table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    expect_message(prepare_source_table(con_test, "platform"))
    df <- prepare_table_table("DP", FALSE, con_test, "platform")
    expect_equal(dim(df), c(1,6))
    df <- prepare_category_table("OB", con_test, "platform")
    expect_equal(dim(df), c(2,3))
    expect_equal(df[2,1], 5)
    df <- prepare_category_relationship_table("OB", con_test, "platform")
    expect_equal(dim(df), c(1,3))
    df <- prepare_category_table_table("OB", con_test, "platform")
    expect_equal(dim(df), c(1,3))
    expect_equal(df$category_id, 5)
    df <- prepare_table_dimensions_table("OB", con_test, "platform")
    expect_equal(dim(df), c(2,3))
    df <- prepare_dimension_levels_table(testthat::test_path("../../tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
                                          "OB", "OBCINE", con_test)
    expect_equal(dim(df), c(273,3))
    df <- prepare_unit_table(con_test)
    expect_equal(dim(df), c(1,1))
    df <- prepare_series_table(testthat::test_path("../../tests/testthat/testdata/zadnje_stare/Bilance_proracunov_obcin_1992-2025.xlsx"),
                               "OB", "OBCINE", con_test)
    expect_equal(dim(df), c(542,5))
    df <- prepare_series_levels_table("OB", con_test, "platform")
    expect_equal(dim(df), c(1084,3))
  })
})







