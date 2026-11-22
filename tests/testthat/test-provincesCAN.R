## provincesCAN(): GADM province boundaries through prepInputs(), the same object as
## geodata::gadm("CAN", level = 1) returned before.

test_that("same provinces as geodata::gadm()", {
  skip_on_cran()
  skip_if_not_installed("geodata")
  skip_if_offline("geodata.ucdavis.edu")
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)

  old <- geodata::gadm("CAN", level = 1, path = withr::local_tempdir())
  new <- provincesCAN(withr::local_tempdir())

  expect_s4_class(new, "SpatVector")
  expect_identical(terra::crs(new), terra::crs(old))
  expect_identical(terra::geom(new), terra::geom(old))
  expect_identical(terra::values(new), terra::values(old))
  expect_true(all(c("British Columbia", "Alberta") %in% new$NAME_1))
})

test_that("once downloaded, the file is reused without the server", {
  skip_on_cran()
  skip_if_offline("geodata.ucdavis.edu")
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)

  dPath <- withr::local_tempdir()
  first <- provincesCAN(dPath)
  expect_true(file.exists(file.path(dPath, "gadm", "CHECKSUMS.txt")))
  again <- provincesCAN(dPath, url = "https://unreachable.invalid/gadm41_CAN_1_pk.rds")
  expect_identical(terra::geom(again), terra::geom(first))
})

test_that("an unreachable server is an error, not NULL", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  expect_error(provincesCAN(withr::local_tempdir(),
                            url = "https://unreachable.invalid/gadm41_CAN_1_pk.rds"))
})
