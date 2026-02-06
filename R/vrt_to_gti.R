#' Convert a VRT mosaic to a GTI tile index
#'
#' Parses a GDAL VRT file, validates that it is a simple mosaic, computes
#' tile footprint polygons from the DstRect pixel coordinates and GeoTransform,
#' and writes a GTI-compatible vector dataset (GeoPackage by default) with
#' appropriate layer metadata.
#'
#' @param dsn Character string. Path or URL to a VRT file. Supports
#'   `/vsicurl/` prefixed URLs.
#' @param output Character string. Output file path. The format is inferred
#'   from the extension: `.gpkg` for GeoPackage (default recommendation),
#'   `.fgb` for FlatGeoBuf. These formats support layer metadata required
#'   by GTI.
#' @param layer Character string. Layer name in the output dataset.
#'   Defaults to `"tile_index"`.
#' @param location_field Character string. Name of the field that stores
#'   tile file paths. Defaults to `"location"` (the GTI default).
#' @param relative_paths Logical. If `TRUE`, store paths relative to the
#'   output file location. If `FALSE` (default), store absolute/resolved paths.
#' @param overwrite Logical. If `TRUE`, delete existing output file before
#'   writing.
#'
#' @return Invisibly, the output file path. The file can be opened as a GTI
#'   raster datasource, e.g. `GTI:<output>`.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. **Parse**: Read the VRT XML and extract source information.
#' 2. **Validate**: Ensure the VRT is a simple mosaic (SimpleSource only,
#'    consistent bands, no resampling).
#' 3. **Compute extents**: Convert DstRect pixel coordinates to geographic
#'    extents using the VRT's GeoTransform. Extents are pixel-edge aligned.
#' 4. **Resolve paths**: Handle `relativeToVRT` attributes, resolving source
#'    file paths against the VRT location.
#' 5. **Write**: Create a GeoPackage (or FlatGeoBuf) with polygon footprints
#'    and a `location` field, using [gdalraster::GDALVector].
#' 6. **Set metadata**: Write GTI layer metadata items (RESX, RESY, MINX,
#'    MINY, MAXX, MAXY, BAND_COUNT, DATA_TYPE, NODATA, SRS).
#'
#' @export
#' @examples
#' \dontrun{
#' vrt_to_gti(
#'   dsn = "/vsicurl/https://example.com/REMA-2m_dem_ovr.vrt",
#'   output = "rema_tiles.gpkg"
#' )
#'
#' # Open with GDAL
#' library(gdalraster)
#' ds <- new(GDALRaster, "GTI:rema_tiles.gpkg")
#' }
vrt_to_gti <- function(dsn,
                        output,
                        layer = "tile_index",
                        location_field = "location",
                        relative_paths = FALSE,
                        overwrite = FALSE) {

  # Parse and validate
  message("Parsing VRT: ", dsn)
  vrt_info <- parse_vrt(dsn)
  validate_vrt(vrt_info)
  message("  ", length(vrt_info$bands[[1]]$sources), " sources across ",
          vrt_info$band_count, " band(s)")

  # Compute tile extents from DstRect + GeoTransform
  gt <- vrt_info$geotransform
  sources <- vrt_info$bands[[1]]$sources
  n <- length(sources)

  tile_data <- .compute_tile_data(sources, gt, dsn, relative_paths, output)

  # Compute overall mosaic extent from the GeoTransform and raster dimensions
  mosaic_xmin <- gt[1]
  mosaic_xmax <- gt[1] + vrt_info$rasterXSize * gt[2]
  mosaic_ymax <- gt[4]
  mosaic_ymin <- gt[4] + vrt_info$rasterYSize * gt[6]

  # Resolution
  res_x <- abs(gt[2])
  res_y <- abs(gt[6])

  # Handle output
  if (file.exists(output)) {
    if (overwrite) {
      gdalraster::deleteDataset(output)
    } else {
      stop("Output file exists: ", output, ". Use overwrite = TRUE.", call. = FALSE)
    }
  }

  # Determine output format from extension
  fmt <- .format_from_ext(output)

  # Write the tile index
  message("Writing GTI to: ", output, " (", fmt, ")")
  .write_gti(
    output = output,
    format = fmt,
    layer = layer,
    location_field = location_field,
    tile_data = tile_data,
    srs = vrt_info$srs,
    res_x = res_x,
    res_y = res_y,
    mosaic_extent = c(mosaic_xmin, mosaic_ymin, mosaic_xmax, mosaic_ymax),
    band_count = vrt_info$band_count,
    data_type = vrt_info$data_type,
    nodata = vrt_info$nodata
  )

  message("Done. Open raster index with: GTI:", output)
  invisible(output)
}


# Internal: compute tile extents and resolved paths
.compute_tile_data <- function(sources, gt, vrt_dsn, relative_paths, output) {
  n <- length(sources)
  vrt_base <- .vrt_base_dir(vrt_dsn)

  locations <- character(n)
  xmin <- numeric(n)
  xmax <- numeric(n)
  ymin <- numeric(n)
  ymax <- numeric(n)

  for (i in seq_len(n)) {
    s <- sources[[i]]

    # Resolve file path
    if (s$relative_to_vrt && nzchar(vrt_base)) {
      resolved <- paste0(vrt_base, "/", s$filename)
    } else {
      resolved <- s$filename
    }

    if (relative_paths) {
      out_dir <- normalizePath(dirname(output), mustWork = FALSE)
      resolved <- .make_relative(resolved, out_dir)
    }
    locations[i] <- resolved

    # Compute geo extent from DstRect pixel coords
    dr <- s$dst_rect
    if (is.null(dr)) {
      stop("Source ", i, " (", s$filename, ") has no DstRect", call. = FALSE)
    }
    # Pixel-edge extents (no half-pixel shift needed — DstRect gives
    # the pixel offset and size, and geotransform maps pixel edges)
    xmin[i] <- gt[1] + dr["xOff"] * gt[2]
    xmax[i] <- gt[1] + (dr["xOff"] + dr["xSize"]) * gt[2]
    ymax[i] <- gt[4] + dr["yOff"] * gt[6]
    ymin[i] <- gt[4] + (dr["yOff"] + dr["ySize"]) * gt[6]
  }

  data.frame(
    location = locations,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    stringsAsFactors = FALSE
  )
}


# Internal: write the GTI GeoPackage/FlatGeoBuf via gdalraster
.write_gti <- function(output, format, layer, location_field, tile_data,
                        srs, res_x, res_y, mosaic_extent, band_count,
                        data_type, nodata) {

  n <- nrow(tile_data)

  # Convert SRS to WKT if needed (gdalraster::srs_to_wkt handles EPSG, PROJ, WKT)
  srs_wkt <- tryCatch(
    gdalraster::srs_to_wkt(srs),
    error = function(e) srs
  )

  # Build layer definition
  defn <- gdalraster::ogr_def_layer("Polygon", srs = srs_wkt)
  defn[[location_field]] <- gdalraster::ogr_def_field("OFTString")

  # Create dataset and layer
  gdalraster::ogr_ds_create(format = format, dsn = output)
  lyr <- gdalraster::ogr_layer_create(
    dsn = output,
    layer = layer,
    layer_defn = defn,
    return_obj = TRUE
  )

  # Get the geometry column name from the layer
  geom_col <- lyr$getGeometryColumn()
  if (!nzchar(geom_col)) geom_col <- "geom"

  # Write features
  lyr$startTransaction()
  for (i in seq_len(n)) {
    wkt <- .bbox_to_wkt(tile_data$xmin[i], tile_data$ymin[i],
                         tile_data$xmax[i], tile_data$ymax[i])

    feat <- list()
    feat[[location_field]] <- tile_data$location[i]
    feat[[geom_col]] <- wkt

    lyr$createFeature(feat)
  }
  lyr$commitTransaction()

  # Set layer metadata for GTI
  lyr$setMetadata(sprintf("RESX%s", as.character(res_x)))
  lyr$setMetadata(sprintf("RESY%s", as.character(res_y)))
  lyr$setMetadata(sprintf("MINX%s", as.character(mosaic_extent[1])))
  lyr$setMetadata(sprintf("MINY%s", as.character(mosaic_extent[2])))
  lyr$setMetadata(sprintf("MAXX%s", as.character(mosaic_extent[3])))
  lyr$setMetadata(sprintf("MAXY%s", as.character(mosaic_extent[4])))
  lyr$setMetadata(sprintf("BAND_COUNT%s", as.character(band_count)))
  lyr$setMetadata(sprintf("DATA_TYPE%s", data_type))
  lyr$setMetadata(sprintf("LOCATION_FIELD%s", location_field))
  if (!is.null(nodata)) {
    lyr$setMetadata(sprintf("NODATA%s", nodata))
  }

  # Also set the SRS metadata explicitly
  lyr$setMetadata(sprintf("SRS%s", srs_wkt))

  lyr$close()
  invisible(output)
}


# Internal: bounding box to WKT polygon
.bbox_to_wkt <- function(xmin, ymin, xmax, ymax) {
  # Use high precision, no scientific notation, trimmed whitespace
  fmt <- function(x) formatC(x, format = "f", digits = 10, flag = "")
  sprintf("POLYGON ((%s %s,%s %s,%s %s,%s %s,%s %s))",
          fmt(xmin), fmt(ymin),
          fmt(xmax), fmt(ymin),
          fmt(xmax), fmt(ymax),
          fmt(xmin), fmt(ymax),
          fmt(xmin), fmt(ymin))
}


# Internal: determine the base directory of a VRT for resolving relative paths
.vrt_base_dir <- function(dsn) {
  if (grepl("^/vsicurl/", dsn)) {
    # For /vsicurl/ URLs, the base is the URL directory
    url <- sub("^/vsicurl/", "", dsn)
    base_url <- sub("/[^/]*$", "", url)
    paste0("/vsicurl/", base_url)
  } else if (grepl("^https?://", dsn)) {
    sub("/[^/]*$", "", dsn)
  } else {
    normalizePath(dirname(dsn), mustWork = FALSE)
  }
}


# Internal: make a path relative to a base directory
.make_relative <- function(path, base_dir) {
  # Simple relative path computation for local files
  # For /vsi paths, just return as-is

  if (grepl("^/vsi", path)) return(path)
  if (grepl("^https?://", path)) return(path)

  tryCatch({
    path_norm <- normalizePath(path, mustWork = FALSE)
    base_norm <- normalizePath(base_dir, mustWork = FALSE)
    # Use R's relative path computation
    if (startsWith(path_norm, base_norm)) {
      sub(paste0("^", gsub("([.\\\\+*?\\[\\](){}|^$])", "\\\\\\1", base_norm), "/?"),
          "", path_norm)
    } else {
      path
    }
  }, error = function(e) path)
}


# Internal: determine output format from file extension
.format_from_ext <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    gpkg = "GPKG",
    fgb  = "FlatGeobuf",
    stop("Unsupported output format extension: .", ext,
         ". Use .gpkg or .fgb", call. = FALSE)
  )
}
