
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vrgti

<!-- badges: start -->

[![R-CMD-check](https://github.com/mdsumner/vrgti/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mdsumner/vrgti/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# vrgti

Convert GDAL VRT mosaics to the GDAL Tile Index (GTI) format.

## Installation

``` r
# install.packages("pak")
pak::pak("hypertidy/vrgti")
```

## Usage

``` r
library(vrgti)

# Convert a VRT mosaic to a GTI-compatible GeoPackage
vrt_to_gti(
  dsn = "/vsicurl/https://raw.githubusercontent.com/mdsumner/rema-ovr/main/REMA-2m_dem_ovr.vrt",
  output = "rema_tiles.gpkg"
)

# Open the result as a raster dataset
library(gdalraster)
ds <- new(GDALRaster, "GTI:rema_tiles.gpkg")
ds$dim()
ds$res()
ds$close()
```

Let’s try that example output directly:

``` r
dsn <- "/vsicurl/https://github.com/mdsumner/vrgti/releases/download/latest/rema_v2_tiles.gti.gpkg"
library(gdalraster)
#> GDAL 3.13.0dev-7924dc998e (released 2026-01-27), GEOS 3.12.1, PROJ 9.7.0
ds <- new(GDALRaster, dsn)
ds$getDriverLongName()
#> [1] "GDAL Raster Tile Index"
ds$dim()
#> [1] 2725100 2921100       1
ds$res()
#> [1] 2 2
ds$bbox() ## xmin,ymin,xmax,ymax
#> [1] -2700100 -2500100  2750100  3342100
ds$close()
```

## What it does

1.  **Parses** the VRT XML (via xml2) to extract tile source paths and
    pixel-space layout
2.  **Validates** that the VRT is a simple mosaic — only `SimpleSource`
    elements, consistent across bands, no resampling
3.  **Computes** geographic tile footprints from DstRect pixel
    coordinates + GeoTransform
4.  **Writes** a GTI-compatible GeoPackage via gdalraster’s
    `GDALVector`, with polygon footprints, a `location` field, and layer
    metadata (RESX, RESY, extent, band count, data type, nodata, SRS)

## Why GTI over VRT?

GTI is an improved mosaic format over VRT for large tile collections:

- **Spatial indexing** — GeoPackage/FlatGeoBuf backends provide
  efficient spatial queries, avoiding the need to scan all source
  entries
- **Scale** — handles hundreds of thousands of tiles where VRT’s
  XML-in-memory approach struggles
- **Flexibility** — tiles can have different SRS (on-the-fly
  reprojection), z-order control, alpha band handling
- **Compact** — smaller index files than equivalent VRT XML

## Non-simple VRTs

The function will stop with a diagnostic message if the VRT contains:

- `AveragedSource`, `KernelFilteredSource`, or other non-simple source
  types (note: `ComplexSource` is allowed — it’s the standard
  nodata-aware mosaic source)
- `VRTWarpedDataset` or `VRTDerivedRasterBand` subclasses
- Sources where SrcRect and DstRect dimensions differ (within-VRT
  resampling)
- Bands with inconsistent source file/layout across bands

## Dependencies

- [xml2](https://xml2.r-lib.org/) for VRT XML parsing
- [gdalraster](https://github.com/firelab/gdalraster) (\>= 2.0.0) for
  GeoPackage writing via `GDALVector` and layer metadata

## Code of Conduct

Please note that the vrgti project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
