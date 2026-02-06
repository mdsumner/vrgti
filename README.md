# vrgti

Convert GDAL VRT mosaics to the GDAL Tile Index (GTI) format.

The shared **T** in VR**T** → G**T**I.

## Installation

```r
# install.packages("pak")
pak::pak("hypertidy/vrgti")
```

## Usage

```r
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

## What it does

1. **Parses** the VRT XML (via xml2) to extract tile source paths and pixel-space layout
2. **Validates** that the VRT is a simple mosaic — only `SimpleSource` elements, consistent across bands, no resampling
3. **Computes** geographic tile footprints from DstRect pixel coordinates + GeoTransform
4. **Writes** a GTI-compatible GeoPackage via gdalraster's `GDALVector`, with polygon footprints, a `location` field, and layer metadata (RESX, RESY, extent, band count, data type, nodata, SRS)

## Why GTI over VRT?

GTI is an improved mosaic format over VRT for large tile collections:

- **Spatial indexing** — GeoPackage/FlatGeoBuf backends provide efficient spatial queries, avoiding the need to scan all source entries
- **Scale** — handles hundreds of thousands of tiles where VRT's XML-in-memory approach struggles
- **Flexibility** — tiles can have different SRS (on-the-fly reprojection), z-order control, alpha band handling
- **Compact** — smaller index files than equivalent VRT XML

## Non-simple VRTs

The function will stop with a diagnostic message if the VRT contains:

- `ComplexSource`, `AveragedSource`, `KernelFilteredSource`, or other non-simple source types
- `VRTWarpedDataset` or `VRTDerivedRasterBand` subclasses
- Sources where SrcRect and DstRect dimensions differ (within-VRT resampling)
- Bands with inconsistent source file/layout across bands

## Dependencies

- [xml2](https://xml2.r-lib.org/) for VRT XML parsing
- [gdalraster](https://github.com/USDAForestService/gdalraster) (>= 2.0.0) for GeoPackage writing via `GDALVector` and layer metadata
