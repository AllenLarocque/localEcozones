## provincesCAN(): GADM province boundaries through prepInputs(), the same object as
## geodata::gadm("CAN", level = 1) returned before.

## A small packed SpatVector saved under GADM's file name, served as a file:// URL, so the
## read and reuse tests need neither the network nor the GADM server.
localGADM <- function(dir) {
  v <- terra::vect(c("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))"),
                   crs = "EPSG:4326")
  v$NAME_1 <- c("British Columbia", "Alberta")
  f <- file.path(dir, "gadm41_CAN_1_pk.rds")
  saveRDS(terra::wrap(v), f)
  list(v = v, url = paste0("file://", normalizePath(f, winslash = "/")))
}

noSharedPaths <- function(env = parent.frame()) {
  withr::local_options(list(reproducible.destinationPathShared = NULL,
                            reproducible.inputPaths = NULL), .local_envir = env)
}

test_that("reads the packed SpatVector, and reuses it once the source is gone", {
  noSharedPaths()
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  src <- localGADM(withr::local_tempdir())
  dPath <- withr::local_tempdir()

  first <- provincesCAN(dPath, url = src$url)
  expect_s4_class(first, "SpatVector")
  expect_identical(terra::geom(first), terra::geom(src$v))
  expect_identical(first$NAME_1, c("British Columbia", "Alberta"))
  expect_true(file.exists(file.path(dPath, "CHECKSUMS.txt")))

  unlink(sub("^file://", "", src$url))
  again <- provincesCAN(dPath, url = src$url)
  expect_identical(terra::geom(again), terra::geom(first))
})

test_that("an unreachable server is a download error naming the URL, not NULL", {
  skip_on_cran() # reproducible turns download failures into skips on CRAN
  noSharedPaths()
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  expect_error(provincesCAN(withr::local_tempdir(),
                            url = "https://unreachable.invalid/gadm41_CAN_1_pk.rds"),
               "unreachable.invalid/gadm41_CAN_1_pk.rds")
})

test_that("same provinces as geodata::gadm() (needs geodata and the GADM server)", {
  skip_on_cran()
  skip_if_not_installed("geodata")
  skip_if_offline("geodata.ucdavis.edu")
  noSharedPaths()
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)

  old <- geodata::gadm("CAN", level = 1, path = withr::local_tempdir())
  new <- provincesCAN(withr::local_tempdir())

  expect_s4_class(new, "SpatVector")
  expect_identical(terra::crs(new), terra::crs(old))
  expect_identical(terra::geom(new), terra::geom(old))
  expect_identical(terra::values(new), terra::values(old))
  expect_true(all(c("British Columbia", "Alberta") %in% new$NAME_1))
})
