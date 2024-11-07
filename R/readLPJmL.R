#' @title readLPJmL
#'
#' @description Read in LPJmL outputs
#'
#' @param subtype Switch between different inputs
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Felicitas Beier, Sebastian Ostberg, Michael Crawford, Kristine Karstens
#'
#' @seealso
#' [readLPJ()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = FALSE)
#' }

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

  # extract meta data from file
  meta        <- lpjmlkit::read_meta(dataname)
  # extract unit from meta data
  unit        <- meta$unit
  # check units
  mapUnitsExp <- toolGetMapping("lpjmlUnits.csv", where = "mrlandcore")
  expUnit     <- mapUnitsExp[mapUnitsExp$variable == gsub(".bin.json", "",
                                                          gsub("./", "", dataname)), 2]
  toolExpectTrue(unit == expUnit, "LPJmL unit is as expected",
                 level = 0, falseStatus = "warn")

  # extract grid information
  x$add_grid(gridname, silent = TRUE)
  grid <- x$grid$data

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

  # check whether crops from our mapping exist
  lpj2mag <- madrat::toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mrlandcore")
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

  }

  # transform to format of magpie object while maintaining cell order
  lon <- gsub("\\.", "p", grid[, "lon"])
  lat <- gsub("\\.", "p", grid[, "lat"])
  cell <- magclass::getItems(x, dim = 1)

  # use coordinate mapping to assign MAgPIE coords and iso
  x <- magclass::add_dimension(x, dim = 1.1, add = "x", nm = "dummy")
  x <- magclass::add_dimension(x, dim = 1.2, add = "y", nm = "dummy")
  magclass::getItems(x, dim = 1, raw = TRUE) <- paste(lon, lat, cell, sep = ".")

  x <- mstools::toolCoord2Isocoord(x)

  return(list(x = x,
              unit = unit))
}
