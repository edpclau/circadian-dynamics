test_that("read_csv_data nests one tibble per measurement column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  dt <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = 3600, length.out = 5)
  readr::write_csv(data.frame(datetime = dt, IND_1 = 1:5, IND_2 = 6:10), path)

  out <- read_csv_data(path)

  expect_named(out, c("IND_1", "IND_2"))
  expect_s3_class(out$IND_1, "data.frame")
  expect_named(out$IND_1, c("datetime", "value"))
  expect_equal(out$IND_2$value, 6:10)
})

test_that("deprecated reader aliases warn and delegate", {
  expect_warning(
    try(read_trikinetics_long("/no/such/file.txt"), silent = TRUE),
    "deprecated"
  )
})
