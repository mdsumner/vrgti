test_that("validate_vrt passes for simple mosaic", {
  vrt_file <- system.file("extdata/test_mosaic.vrt", package = "vrgti")
  info <- parse_vrt(vrt_file)
  expect_true(validate_vrt(info))
})

test_that("validate_vrt passes for ComplexSource (nodata mosaic)", {
  vrt_file <- system.file("extdata/test_complex.vrt", package = "vrgti")
  info <- parse_vrt(vrt_file)
  # ComplexSource is allowed — it's what gdalbuildvrt emits with nodata
  expect_true(validate_vrt(info))
})

test_that("validate_vrt rejects AveragedSource", {
  info <- list(
    bands = list(list(
      band = 1L,
      subClass = NULL,
      sources = list(list(
        source_type = "AveragedSource",
        filename = "test.tif",
        relative_to_vrt = FALSE,
        source_band = "1",
        src_rect = c(xOff = 0, yOff = 0, xSize = 100, ySize = 100),
        dst_rect = c(xOff = 0, yOff = 0, xSize = 100, ySize = 100),
        extra_elements = character(0)
      ))
    ))
  )
  expect_error(validate_vrt(info), "AveragedSource.*not supported")
})

test_that("validate_vrt rejects mismatched SrcRect/DstRect sizes", {
  # Build a synthetic vrt_info with mismatched dimensions
  info <- list(
    bands = list(list(
      band = 1L,
      subClass = NULL,
      sources = list(list(
        source_type = "SimpleSource",
        filename = "test.tif",
        relative_to_vrt = FALSE,
        source_band = "1",
        src_rect = c(xOff = 0, yOff = 0, xSize = 100, ySize = 100),
        dst_rect = c(xOff = 0, yOff = 0, xSize = 50, ySize = 50),
        extra_elements = character(0)
      ))
    ))
  )
  expect_error(validate_vrt(info), "SrcRect size.*DstRect size")
})
