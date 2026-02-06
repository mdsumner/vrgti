#' Validate that a VRT represents a simple mosaic
#'
#' Checks whether a VRT file is suitable for conversion to a GTI tile index.
#' A "simple" VRT mosaic must satisfy:
#'
#' - All sources are `SimpleSource` or `ComplexSource` (no `AveragedSource`,
#'   `KernelFilteredSource`, etc.)
#' - No `subClass` on bands (no `VRTWarpedDataset`, `VRTDerivedRasterBand`)
#' - All bands reference the same set of source files with the same DstRect
#'   layout (standard multi-band mosaic)
#' - SrcRect and DstRect dimensions match (no within-VRT resampling)
#'
#' @param vrt_info A list as returned by [parse_vrt()].
#'
#' @return Invisibly, `TRUE` if valid. Throws an error with a diagnostic
#'   message if validation fails.
#'
#' @export
#' @examples
#' \dontrun{
#' info <- parse_vrt("mosaic.vrt")
#' validate_vrt(info)
#' }
validate_vrt <- function(vrt_info) {
  errors <- character(0)

  # Check 1: No band subclasses
  for (b in vrt_info$bands) {
    if (!is.null(b$subClass)) {
      errors <- c(errors, sprintf(
        "Band %d has subClass '%s'; only standard VRTRasterBand is supported",
        b$band, b$subClass))
    }
  }

  # Check 2: All sources must be SimpleSource or ComplexSource
  # ComplexSource is used by gdalbuildvrt whenever nodata is present —

  # structurally identical to SimpleSource for mosaic layout purposes.
  allowed_types <- c("SimpleSource", "ComplexSource")
  for (bi in seq_along(vrt_info$bands)) {
    b <- vrt_info$bands[[bi]]
    for (si in seq_along(b$sources)) {
      s <- b$sources[[si]]
      if (!s$source_type %in% allowed_types) {
        errors <- c(errors, sprintf(
          "Band %d, source %d: type '%s' is not supported (allowed: %s)",
          b$band, si, s$source_type, paste(allowed_types, collapse = ", ")))
      }
    }
  }

  # Check 3: SrcRect and DstRect dimensions must match (no resampling)
  for (bi in seq_along(vrt_info$bands)) {
    b <- vrt_info$bands[[bi]]
    for (si in seq_along(b$sources)) {
      s <- b$sources[[si]]
      if (!is.null(s$src_rect) && !is.null(s$dst_rect)) {
        if (s$src_rect["xSize"] != s$dst_rect["xSize"] ||
            s$src_rect["ySize"] != s$dst_rect["ySize"]) {
          errors <- c(errors, sprintf(
            "Band %d, source %d (%s): SrcRect size (%g x %g) != DstRect size (%g x %g); resampling within VRT is not supported",
            b$band, si, basename(s$filename),
            s$src_rect["xSize"], s$src_rect["ySize"],
            s$dst_rect["xSize"], s$dst_rect["ySize"]))
        }
      }
    }
  }

  # Check 4: All bands have consistent source files and DstRects
  if (length(vrt_info$bands) > 1) {
    ref_sources <- .source_fingerprint(vrt_info$bands[[1]])
    for (bi in 2:length(vrt_info$bands)) {
      other_sources <- .source_fingerprint(vrt_info$bands[[bi]])
      if (!identical(ref_sources, other_sources)) {
        errors <- c(errors, sprintf(
          "Band %d has different source file/DstRect layout than band 1; multi-band mosaics must have matching sources across bands",
          vrt_info$bands[[bi]]$band))
      }
    }
  }

  # Check 5: Must have at least one source
  n_sources <- length(vrt_info$bands[[1]]$sources)
  if (n_sources == 0) {
    errors <- c(errors, "Band 1 has no sources")
  }

  if (length(errors) > 0) {
    stop("VRT is not a simple mosaic:\n",
         paste("  -", errors, collapse = "\n"),
         call. = FALSE)
  }

  invisible(TRUE)
}


# Internal: create a fingerprint of source layout for cross-band comparison
# Returns a data.frame of filename + DstRect for ordering comparison
.source_fingerprint <- function(band) {
  if (length(band$sources) == 0) return(data.frame())
  data.frame(
    filename = vapply(band$sources, function(s) s$filename, character(1)),
    dst_xOff  = vapply(band$sources, function(s) s$dst_rect["xOff"], numeric(1)),
    dst_yOff  = vapply(band$sources, function(s) s$dst_rect["yOff"], numeric(1)),
    dst_xSize = vapply(band$sources, function(s) s$dst_rect["xSize"], numeric(1)),
    dst_ySize = vapply(band$sources, function(s) s$dst_rect["ySize"], numeric(1)),
    stringsAsFactors = FALSE
  )
}
