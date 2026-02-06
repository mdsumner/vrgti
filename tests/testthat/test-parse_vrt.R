test_that("parse_vrt reads test mosaic correctly", {
  vrt_file <- system.file("extdata/test_mosaic.vrt", package = "vrgti")
  info <- parse_vrt(vrt_file)

  expect_equal(info$rasterXSize, 200L)
  expect_equal(info$rasterYSize, 200L)
  expect_equal(info$geotransform, c(-100000, 100, 0, 100000, 0, -100))
  expect_equal(info$band_count, 1L)
  expect_equal(info$data_type, "Float32")
  expect_equal(info$nodata, "-9999")

  # Should have 4 sources in band 1
  expect_equal(length(info$bands[[1]]$sources), 4L)

  # Check first source
  s1 <- info$bands[[1]]$sources[[1]]
  expect_equal(s1$source_type, "SimpleSource")
  expect_equal(s1$filename, "/data/tile_a.tif")
  expect_false(s1$relative_to_vrt)
  expect_equal(s1$dst_rect[["xOff"]], 0)
  expect_equal(s1$dst_rect[["yOff"]], 0)
  expect_equal(s1$dst_rect[["xSize"]], 100)
  expect_equal(s1$dst_rect[["ySize"]], 100)
})

test_that("parse_vrt detects ComplexSource extras", {
  vrt_file <- system.file("extdata/test_complex.vrt", package = "vrgti")
  info <- parse_vrt(vrt_file)

  s1 <- info$bands[[1]]$sources[[1]]
  expect_equal(s1$source_type, "ComplexSource")
  expect_true("ScaleOffset" %in% s1$extra_elements)
})
