#' Parse a VRT file into a structured list
#'
#' Reads a VRT file and extracts the dataset-level metadata (dimensions,
#' geotransform, SRS, data type, nodata) and per-band source information
#' (file paths, source types, SrcRect, DstRect).
#'
#' @param dsn Character string. Path or URL to a VRT file. Supports
#'   `/vsicurl/` prefixed URLs — the VRT XML will be read via
#'   [gdalraster::VSIFile] for virtual filesystem paths,
#'   or [xml2::read_xml()] for plain paths/URLs.
#'
#' @return A list with components:
#' \describe{
#'   \item{rasterXSize}{Integer. Width of the VRT in pixels.}
#'   \item{rasterYSize}{Integer. Height of the VRT in pixels.}
#'   \item{geotransform}{Numeric vector of length 6. The affine geotransform.}
#'   \item{srs}{Character string. The spatial reference system (WKT or other).}
#'   \item{bands}{A list of per-band information, each containing a list of sources.}
#'   \item{data_type}{Character. The data type of band 1 (e.g. "Float32").}
#'   \item{nodata}{Character or NULL. The nodata value of band 1, if any.}
#'   \item{band_count}{Integer. Number of bands.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' info <- parse_vrt("/vsicurl/https://example.com/mosaic.vrt")
#' str(info)
#' }
parse_vrt <- function(dsn) {
  doc <- .read_vrt_xml(dsn)
  root <- xml2::xml_find_first(doc, "/VRTDataset")
  if (is.na(root)) {
    stop("Not a valid VRT file: missing VRTDataset root element", call. = FALSE)
  }

  rasterXSize <- as.integer(xml2::xml_attr(root, "rasterXSize"))
  rasterYSize <- as.integer(xml2::xml_attr(root, "rasterYSize"))

  # GeoTransform
  gt_node <- xml2::xml_find_first(root, ".//GeoTransform")
  if (is.na(gt_node)) {
    stop("VRT has no GeoTransform element", call. = FALSE)
  }
  geotransform <- as.numeric(strsplit(trimws(xml2::xml_text(gt_node)), ",\\s*")[[1]])
  if (length(geotransform) != 6) {
    stop("GeoTransform must have 6 values, got ", length(geotransform), call. = FALSE)
  }

  # SRS
  srs_node <- xml2::xml_find_first(root, ".//SRS")
  srs <- if (!is.na(srs_node)) xml2::xml_text(srs_node) else ""

  # Bands
  band_nodes <- xml2::xml_find_all(root, ".//VRTRasterBand")
  if (length(band_nodes) == 0) {
    stop("VRT has no VRTRasterBand elements", call. = FALSE)
  }

  bands <- lapply(band_nodes, .parse_band)

  # Get data_type and nodata from first band
  data_type <- xml2::xml_attr(band_nodes[[1]], "dataType")
  if (is.na(data_type)) data_type <- "Byte"

  nodata_node <- xml2::xml_find_first(band_nodes[[1]], ".//NoDataValue")
  nodata <- if (!is.na(nodata_node)) xml2::xml_text(nodata_node) else NULL

  list(
    rasterXSize = rasterXSize,
    rasterYSize = rasterYSize,
    geotransform = geotransform,
    srs = srs,
    bands = bands,
    data_type = data_type,
    nodata = nodata,
    band_count = length(band_nodes)
  )
}


# Internal: read VRT XML, handling /vsi paths
#' @importFrom methods new
#' @noRd
.read_vrt_xml <- function(dsn) {
  if (grepl("^/vsicurl/", dsn)) {
    # Extract the URL from /vsicurl/ prefix and let xml2 read it
    url <- sub("^/vsicurl/", "", dsn)
    xml2::read_xml(url)
  } else if (grepl("^/vsi", dsn)) {
    ## this branch is untested, as is the next one 2026-02-06
    # Other /vsi paths (e.g. /vsigzip/, /vsimem/) — read via gdalraster
    v <- methods::new(gdalraster::VSIFile, dsn)
    bytes <- v$ingest(-1)
    v$close()
    xml2::read_xml(bytes)
  } else {
    # Local file or plain URL
    xml2::read_xml(dsn)
  }
}

# Internal: parse a single VRTRasterBand node
.parse_band <- function(band_node) {
  band_num <- xml2::xml_attr(band_node, "band")
  subclass <- xml2::xml_attr(band_node, "subClass")
  data_type <- xml2::xml_attr(band_node, "dataType")

  # Find all source elements (children of the band)
  children <- xml2::xml_children(band_node)
  source_names <- xml2::xml_name(children)
  source_types <- c("SimpleSource", "ComplexSource", "AveragedSource",
                     "KernelFilteredSource", "ArraySource",
                     "NoDataFromMaskSource")
  is_source <- source_names %in% source_types
  source_nodes <- children[is_source]

  sources <- lapply(source_nodes, .parse_source)

  list(
    band = as.integer(band_num),
    subClass = if (is.na(subclass)) NULL else subclass,
    data_type = if (is.na(data_type)) "Byte" else data_type,
    sources = sources
  )
}

# Internal: parse a single source element
.parse_source <- function(source_node) {
  source_type <- xml2::xml_name(source_node)

  # SourceFilename
  fn_node <- xml2::xml_find_first(source_node, ".//SourceFilename")
  filename <- if (!is.na(fn_node)) xml2::xml_text(fn_node) else NA_character_
  relative_to_vrt <- xml2::xml_attr(fn_node, "relativeToVRT")
  relative_to_vrt <- if (!is.na(relative_to_vrt)) as.integer(relative_to_vrt) == 1L else FALSE

  # SourceBand
  sb_node <- xml2::xml_find_first(source_node, ".//SourceBand")
  source_band <- if (!is.na(sb_node)) xml2::xml_text(sb_node) else "1"

  # DstRect
  dst_node <- xml2::xml_find_first(source_node, ".//DstRect")
  dst_rect <- if (!is.na(dst_node)) {
    c(xOff  = as.numeric(xml2::xml_attr(dst_node, "xOff")),
      yOff  = as.numeric(xml2::xml_attr(dst_node, "yOff")),
      xSize = as.numeric(xml2::xml_attr(dst_node, "xSize")),
      ySize = as.numeric(xml2::xml_attr(dst_node, "ySize")))
  } else {
    NULL
  }

  # SrcRect
  src_node <- xml2::xml_find_first(source_node, ".//SrcRect")
  src_rect <- if (!is.na(src_node)) {
    c(xOff  = as.numeric(xml2::xml_attr(src_node, "xOff")),
      yOff  = as.numeric(xml2::xml_attr(src_node, "yOff")),
      xSize = as.numeric(xml2::xml_attr(src_node, "xSize")),
      ySize = as.numeric(xml2::xml_attr(src_node, "ySize")))
  } else {
    NULL
  }

  # Detect ComplexSource extras (for validation)
  extra_elements <- character(0)
  if (source_type == "ComplexSource") {
    children <- xml2::xml_children(source_node)
    child_names <- xml2::xml_name(children)
    base_names <- c("SourceFilename", "SourceBand", "SourceProperties",
                     "SrcRect", "DstRect", "OpenOptions")
    extra_elements <- setdiff(child_names, base_names)
  }

  list(
    source_type = source_type,
    filename = filename,
    relative_to_vrt = relative_to_vrt,
    source_band = source_band,
    dst_rect = dst_rect,
    src_rect = src_rect,
    extra_elements = extra_elements
  )
}
