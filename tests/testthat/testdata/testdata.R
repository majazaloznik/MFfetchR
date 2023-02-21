file_path <- meta[3,1]
table_name <- meta[3,2]
sheet_name <- meta[3,3]
l <- prepare_vintage_table(file_path, table_name, sheet_name, con)
parsed <- lapply(l$parsed[1:2], `[`,1:100, 1:3)
saveRDS(parsed, "tests/testthat/testdata/parsed.rds")

