#' @title readLPJmL
#'
#' @description Read in LPJmL outputs
#'
#' @param subtype Switch between different inputs
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Felicitas Beier, Sebastian Ostberg, Michael Crawford
#'
#' @seealso
#' [readLPJ()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = FALSE)
#' }

# nolint start
### This function should be adjusted during the LPJmL - MAgPIE - Hackathon ###
# Testing locations:
# /p/projects/rd3mod/inputdata/sources/LPJmL
# example subtype: lpjml5.9.5.mag1.MRI.ESM2.0.ssp370.crop.sdate

# Testing data:
# subtype <- "lpjml5.9.5.mag1.MRI.ESM2.0.ssp370.crop.sdate"
# setwd("/p/projects/rd3mod/inputdata/sources/LPJmL/lpjml5.9.5.mag1.MRI.ESM2.0.ssp370.pnv.mrunoff")
# setwd("/p/projects/rd3mod/inputdata/sources/LPJmL/lpjml5.9.5.mag1.MRI.ESM2.0.ssp370.crop.sdate")
# setwd("/p/projects/rd3mod/inputdata/sources/LPJmL/")
# nolint end

readLPJmL <- function(subtype = "lpjml5.9.5-m1:MRI-ESM2-0:ssp370:crops:sdate") {

  # filenames for dataset and grid
  files <- list.files(path = ".", pattern = "\\.bin\\.json$", full.names = TRUE)
  gridname <- lpjmlkit::find_varfile(".", variable = "grid")
  dataname <- grep("grid", files, invert = TRUE, value = TRUE)
  if (length(dataname) != 1) {
    stop("More than one data file is present in the LPJmL source directory.")
  }

  # read in LPJmL dataset
  x <- lpjmlkit::read_io(dataname)

  # generate a mapping from LPJmL lon-lat to MAgPIE coord
  # extract grid information
  x$add_grid(gridname, silent = TRUE)
  grid <- x$grid$data

  # transform to format of magpie object while maintaining cell order
  lon <- gsub("\\.", "p", grid[, "lon"])
  lat <- gsub("\\.", "p", grid[, "lat"])
  coordsLPJmL <- paste(lon, lat, sep = ".")

  # sort mapping according to provided grid to ensure consistency
  mapping <- mstools::toolGetMappingCoord2Country(pretty = FALSE, extended = FALSE)
  matches <- match(x = coordsLPJmL, table = mapping$coords)
  if (any(is.na(matches))) {
    stop("Discrepancy between spatial extent of LPJmL coords and MAgPIE coords")
  }
  mapping <- mapping[matches, ]

  # transform time dimension
  x$transform(to = "year_month_day")
  if ("month" %in% names(dimnames(x))) {
    x <- aperm(x$data, c("cell", "year", "month", "band"))
  } else {
    x <- x$data
  }
  x <- drop(x)

  # transform x into a MAgPIE object
  x <- magclass::as.magpie(x, spatial = 1)

  lpj2mag <- madrat::toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mrlandcore")
  # TO BE DISCUSSED, this again makes it very unflexible to read in new crops from LPJmL, as LN:102
  # will fail if this mapping is not accordingly updated to new crop types. Why not use the long names?
  meta    <- lpjmlkit::read_meta(dataname)

  hasCrops <- any(sub("^(rainfed|irrigated)\\s+", "", meta$band_names) %in% lpj2mag$LPJmL5)
  if (hasCrops) {

    hasIrrigationStatus <- grepl("^(rainfed|irrigated)\\b", meta$band_names)
    if (any(hasIrrigationStatus == TRUE) && any(hasIrrigationStatus == FALSE)) {
      stop("Mixed irrigation status within LPJmL crop data is an unsupported case")
    }

    if (all(hasIrrigationStatus)) {

      # differentiate between rainfed and irrigated crops in the case of crop data
      irrigation <- sub(" .*", "", magclass::getNames(x)) # select first word (of, e.g. "rainfed temperature cereals")
      x <- magclass::add_dimension(x, dim = 3.1, add = "irrigation", nm = "dummy")
      magclass::getNames(x, dim = "irrigation") <- irrigation

      magclass::getSets(x)["d3.2"] <- "crop"
      crop <- sub("^[^ ]+\\s+", "",  magclass::getNames(x)) # select everything after first word
      magclass::getNames(x, dim = "crop") <- crop

    } else {
      magclass::getSets(x)["d3.1"] <- "crop"
    }

    # transform LPJmL5 to LPJmL-internal names as of mapping
    x <- madrat::toolAggregate(x, rel = lpj2mag, from = "LPJmL5", to = "LPJmL", dim = "crop", partrel = TRUE)
  }

  # use coordinate mapping to assign MAgPIE coords and iso
  x <- magclass::add_dimension(x, dim = 1.1, add = "x", nm = "dummy")
  x <- magclass::add_dimension(x, dim = 1.2, add = "y", nm = "dummy")
  magclass::getItems(x, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")
  magclass::getSets(x)[c("d1.1", "d1.2", "d1.3")] <- c("x", "y", "iso")

  return(x)
}
