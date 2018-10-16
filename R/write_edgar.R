#' Read HDF5 EDGAR emission files.
#'
#' @description \code{\link{read_edgar}} calculates ADT based on hourly traffic data. The
#' input traffic data is usually for morning rush hours.
#'
#' @param path Character; Where is your HDF5 file.
#' @param dataset Character; What data do you want. If you dont know,
#' i will show you that it is inside so that you can choose.
#' @param m Array; Array of monhly emissions, dim = c(month, 1, , )
#' @param month Integer; which month from 1 to 12. If missing, cover whole year.
#' @param verbose Logical; Do you want more information?
#' @importFrom  h5 h5file list.datasets list.attributes openAttribute readAttribute writeDataSet h5close
#' @importFrom utils menu object.size
#' @export
#' @examples \dontrun{
#' # do not run
#' path  = "~/Downloads/EDGAR-HTAP_TRANSPORT_2010.h5"
#' e <- read_edgar(path, month = 1, dataset = "PM2.5")
#' e <- e* #units in kg/m2/s
#' write_edgar(path = path, dataset = "PM2.5", month = 1)
#' }
write_edgar <- function(path,
                       dataset,
                       m,
                       month,
                       verbose = TRUE){
  file <- h5::h5file(name = path,
                     mode = 'w')
  ds <- h5::list.datasets(file, recursive = TRUE)
  ds <- substr(x = ds, start = 2, nchar(ds))

  if(verbose) print(file)
  if(verbose) cat("Datasets:\n",
                  ds, "\n")
  if(verbose) cat("Attributes:\n",
                  h5::list.attributes(file), "\n")

  if(missing(dataset)){
    choice <- utils::menu(ds, title="Choose dataset")
    dataset <- ds[choice]
  }
  name_ds <- h5::openDataSet(.Object = file, datasetname = dataset)
  dset <- h5::readDataSet(.Object = name_ds)
  if(verbose) cat("dimensions: ", dim(dset), '\n')
  if(!missing(month)){
    if(verbose) cat("Month: ", month, '\n')
    m[is.na(m)] <- 0
    dset[is.na(dset)] <- 0
    dset[month, 1, 1800:1, ] <- m + dset[month, 1, 1800:1, ]
  } else {
    if(verbose) cat("Assuming m is an array with 12 months\n with dimensions\n")
    m[is.na(m)] <- 0
    dset[is.na(dset)] <- 0
    dset[, 1, 1800:1, ] <- m + dset[, 1, 1800:1, ]

  }
  #write
  if(verbose) cat("Writing... \n")
  h5::writeDataSet(.Object = ds, data = dset)

  h5close(file)

}
