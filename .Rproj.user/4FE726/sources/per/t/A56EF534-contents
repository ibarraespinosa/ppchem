#' Average daily traffic (ADT) from hourly traffic data.
#'
#' @description \code{\link{read_edgar}} calculates ADT based on hourly traffic data. The
#' input traffic data is usually for morning rush hours.
#'
#' @param x numeric vector for passenger cars
#' @return numeric vector of total volume of traffic per link, or data-frames
#' of expanded traffic
#' @import h5
#' @export
#' @examples \dontrun{
#' # do not run
#' }
library(h5)
path  = "~/Downloads/EDGAR-HTAP_TRANSPORT_2010.h5"
mode ="r"
verbose = TRUE
readAttribute(.Object = file)
(a <- openAttribute(file, "NAME"))
readAttribute(a)

(a <- openAttribute(file, "SIZE X"))
readAttribute(a)

(a <- openAttribute(file, "SIZE Y"))
readAttribute(a)

(a <- openAttribute(file, "SIZE Z"))
readAttribute(a)

a <- h5::openDataSet(.Object = file, datasetname = "PM2.5", type = "")
class(a)
dim(a)
read_edgar <- function(path,
                       mode ="r",
                       verbose = TRUE,
                       dataset,
                       attribute,
                       month = 1) {
  file <- h5::h5file(name = path,
                     mode = mode)
  aa <- capture.output(file)
  aa <- aa[2:length(aa)]
  ds <- list.datasets(file, recursive = TRUE)
  if(verbose) print(file)
  if(verbose) cat("Datasets:\n",
                  ds, "\n")
  if(verbose) cat("Attributes:\n",
                  list.attributes(file), "\n")

  types <-   c("double", "integer", "logical", "boolean")

    if(missing(dataset)){
    choice <- utils::menu(ds, title="Choose dataset")
    nds <- ds[choice]
    }

  a <- file[ds][month, 1, , ]

  }
  if(missing(attribute)){
    choice <- utils::menu(ds, title="Choose dataset")
    nds <- ds[choice]
    at <- h5::openAttribute(file, nds)
    bt <- h5::readAttribute(at)
    if(verbose) cat("Attribute ", at, ": ", bt, "\n")
  } else {
    at <- h5::openAttribute(file, attribute)
    bt <- h5::readAttribute(at)
    if(verbose) cat("Attribute ", at, ": ", bt, "\n")

  }

  return(a)
}
read_edgar()
