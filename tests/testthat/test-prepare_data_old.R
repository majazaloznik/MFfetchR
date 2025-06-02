test_that("preparing vintages and data", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    df <- prepare_vintage_table(testthat::test_path("testdata/zadnje_stare/Drzavni_proracun_1992-2025.xlsx"),
                                "DP", "MESPROR", con_test, "platform")
    expect_equal(dim(df$monthly_vintages), c(1574,2))
    expect_equal(dim(df$annual_vintages), c(1574,2))
    expect_equal(dim(df$parsed_data$monthly), c(514698,3))
    expect_equal(dim(df$parsed_data$annual), c(53516,3))
    expect_equal(dim(df$parsed_data$series), c(1574,2))
    expect_equal(df$parsed_data$series$code[1], "MF--DP--7")
    DBI::dbExecute(con_test, paste("set search_path to", "platform"))
    out <- MFfetchR:::vintage_table("M", 164, con_test)
    expect_s3_class(out$published, "POSIXct")})
    expect_equal(dim(out), c(271,2))

})


