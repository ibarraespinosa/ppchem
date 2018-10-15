#' Read HDF5 EDGAR emission files.
#'
#' @description \code{\link{read_edgar}} calculates ADT based on hourly traffic data. The
#' input traffic data is usually for morning rush hours.
#'
#' @param path Character; Where is your HDF5 file.
#' @param dataset Character; What data do you want. If you dont know,
#' i will show you that it is inside so that you can choose.
#' @param show.attribute Logical; Do you wna to see attirbutes?. If you dont know which,
#' i will show you that it is inside so that you can choose.
#' @param month Integer; which month from 1 to 12.
#' @param verbose Logical; Do you want more information?
#' @return array of EDGAR emissions by month.
#' @importFrom  h5 h5file list.datasets list.attributes openAttribute readAttribute
#' @importFrom utils menu object.size
#' @export
#' @examples \dontrun{
#' # do not run
#' path  = "~/Downloads/EDGAR-HTAP_TRANSPORT_2010.h5"
#' e <- read_edgar(path)
#' }
read_edgar <- function(path,
                       dataset,
                       show.attribute = FALSE,
                       month,
                       verbose = TRUE){
  file <- h5::h5file(name = path, mode = 'r')
  if(missing(month)){
    choice <- utils::menu(1:12, title="Choose month")
    month <- (1:12)[choice]
  }
  ds <- list.datasets(file, recursive = TRUE)
  ds <- substr(x = ds, start = 2, nchar(ds))

  if(verbose) cat(file, "\n")
  if(verbose) cat("Datasets:\n",
                  ds, "\n")
  if(verbose) cat("Attributes:\n",
                  list.attributes(file), "\n")

  if(missing(dataset)){
    choice <- utils::menu(ds, title="Choose dataset")
    nds <- ds[choice]
  }
  if(verbose) cat( "Reading... \n")
  a <- file[nds][month, 1800:1, , ]
  if(verbose) cat("dimensions: ", dim(a), '\n')

  if(show.attribute){
    latr <- list.attributes(file)
    choice <- utils::menu(latr, title="Choose attribute")
    nds <- latr[choice]
    at <- h5::openAttribute(.Object = file, attributename = nds)
    bt <- h5::readAttribute(at)
    if(verbose) cat("Attribute Name: ", at@name,  "\nValue: ",bt, "\n")
  }
  return(a)
  h5::h5close(file)

}
