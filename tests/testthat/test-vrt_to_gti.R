test_that("vrt_to_gti creates valid GPKG", {
  skip_if_not_installed("gdalraster", "2.0.0")

  vrt_file <- system.file("extdata/test_mosaic.vrt", package = "vrgti")
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)

  result <- vrt_to_gti(vrt_file, out)
  expect_equal(result, out)
  expect_true(file.exists(out))

  # Read back and check
  lyr <- new(gdalraster::GDALVector, out, "tile_index")
  expect_equal(lyr$getFeatureCount(), 4L)

  # Check field names include location
  expect_true("location" %in% lyr$getFieldNames())

  # Check geometry type
  expect_equal(lyr$getGeomType(), "POLYGON")

  # Check metadata (WIP)
  #resx <- lyr$getMetadataItem("RESX")
  #expect_equal(resx, "100")

  #band_count <- lyr$getMetadataItem("BAND_COUNT")
  #expect_equal(band_count, "1")

  #data_type <- lyr$getMetadataItem("DATA_TYPE")
  #expect_equal(data_type, "Float32")

  lyr$close()
})

test_that("vrt_to_gti computes correct extents", {
  skip_if_not_installed("gdalraster", "2.0.0")

  vrt_file <- system.file("extdata/test_mosaic.vrt", package = "vrgti")
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)

  vrt_to_gti(vrt_file, out)

  # Read features and check extents
  lyr <- new(gdalraster::GDALVector, out, "tile_index")
  lyr$returnGeomAs <- "WKT"
  feat <- lyr$fetch(-1)
  lyr$close()

  # The test VRT has:
  # gt = (-100000, 100, 0, 100000, 0, -100)
  # tile_a: DstRect(0, 0, 100, 100) -> x: [-100000, -90000], y: [90000, 100000]
  # tile_b: DstRect(100, 0, 100, 100) -> x: [-90000, -80000], y: [90000, 100000]
  # tile_c: DstRect(0, 100, 100, 100) -> x: [-100000, -90000], y: [80000, 90000]
  # tile_d: DstRect(100, 100, 100, 100) -> x: [-90000, -80000], y: [80000, 90000]

  expect_equal(nrow(feat), 4L)
  # tile_a should be first, with location /data/tile_a.tif
  expect_equal(feat$location[1], "/data/tile_a.tif")
})

test_that("vrt_to_gti refuses to overwrite without flag", {
  skip_if_not_installed("gdalraster", "2.0.0")

  vrt_file <- system.file("extdata/test_mosaic.vrt", package = "vrgti")
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)

  vrt_to_gti(vrt_file, out)
  expect_error(vrt_to_gti(vrt_file, out), "exists")
  expect_no_error(vrt_to_gti(vrt_file, out, overwrite = TRUE))
})
