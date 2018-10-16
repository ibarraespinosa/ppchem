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
  file <- h5::h5file(name = path, mode = 'r+')
  ds <- h5::list.datasets(file, recursive = TRUE)
  ds <- substr(x = ds, start = 2, nchar(ds))

  if(missing(dataset)){
    choice <- utils::menu(ds, title="Choose dataset")
    dataset <- ds[choice]
  }
  name_ds <- h5::openDataSet(.Object = file, datasetname = dataset)
  dset <- file[dataset]
  if(verbose) cat("dimensions dataset: ", dim(dset), '\n')
  if(verbose) cat("class datset: ", class(dset), '\n')
  if(!missing(month)){
    if(verbose) cat("Month: ", month, '\n')
    m[is.na(m)] <- 0
    dset[is.na(dset)] <- 0
    dset[month, 1, , ] <- m + dset[month, 1, , ]
  } else {
    if(verbose) cat("Assuming m is an array with 12 months\n with dimensions\n")
    class(m) <- class(dset)
    # dset[, , , ] <- m

  }

  #write
  if(verbose) cat("Writing...", dataset,"\n")
  h5::writeDataSet(.Object = name_ds, data = m)

  h5::h5close(file)

}
