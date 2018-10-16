#' Similar to vein::emis_grid but it returns an array with dimensions of ppchem::read_edgar
#'
#' @description \code{\link{emis_grid_array}} return array or sf object of gridded emisions.
#'
#' @param spobj A spatial dataframe of class "sp" or "sf". When class is "sp"
#' it is transformed to "sf". It can be points or lines.
#' @param month Integer, which month? from 1 to 12.
#' @param sf Logical Do you want an 'sf' instead an array?
#' @param sr Integer, Spatial reference e.g: 31983. It is required if spobj and g are
#' not projected. Please, see http://spatialreference.org/.
#' @param type type of geometry: "lines" or "points".
#' @importFrom vein emis_grid
#' @export
#' @note When spobj is a 'Spatial' object (class of sp), they are converted
#'  into 'sf'. Also, The aggregation of data ise done with data.table functions.
#' @examples \dontrun{
#' # do not run
#' library(vein)
#' data(net)
#' net$emission <- 1:length(net)
#' ene <- emis_grid_array(spobj = net[,"emission"])
#' }
emis_grid_array <- function(spobj, month, sf = FALSE, sr = 4326, type = "lines",
                            verbose = TRUE){
  g <- sysdata$g
  cat(names(spobj))
  g$id <- 1:nrow(g)

  netg <- vein::emis_grid(spobj = spobj, g = g, sr = sr, type = type)
  if(sf){
    return(netg)
  } else {
    a <- array(0, dim = c(1,1, 1800, 3600))
    if(verbose) cat("Dimensions of array: ", dim(m), '\n')
    m <- t(matrix(netg$emission, ncol = 1800, nrow = 3600, byrow = TRUE))
    if(verbose) cat("Dimensions of emissions: ", dim(m), '\n')
    a[month, 1, , ] <- m
    return(a)
  }
}

