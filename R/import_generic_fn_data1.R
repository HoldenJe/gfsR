#' import.generic.fn.data1
#' @description A generic import function.  Project file path build is omitted so it 
#' must be provided as part of the argument.
#' @param generic File path to FN DATA.ZIP folder
#' @return a list of FN2 tables
#' @export
#' @examples 
#' \dontrun{
#' fn <- import.generic.fn.data("~/FNData/NS1/IA02_NS1/DATA.ZIP")
#' fn$FN011
#' lapply(fn, head)
#'}

import.generic.fn.data1<-function(generic_datazip) {
  # check that only 1 file is provided
  if(length(generic_datazip)!=1) {
    usethis::ui_stop('This function can only import a single file')
    break
  }
  
  # check that the file exists
  if(file.exists(generic_datazip)) {
    usethis::ui_done("File exists")
  } else {
    usethis::ui_stop("File not found. Check file path.")
  }
  
  # check that a DATA.ZIP file has been provided
  has_zip <- grep(generic_datazip, pattern = "DATA\\.ZIP$")
  if(length(has_zip) == 1) {
    usethis::ui_done("DATA.ZIP input accepted")
  } else {
    usethis::ui_stop("Input file expects a DATA.ZIP file")
  }
  
  AllTables <- All_FN_Tables()
  
  # Create and unzip data to a temp file
  mytemp <- tempdir()
  unzip(generic_datazip, exdir = mytemp)
  
  # check each table exists
  check_table <- function(fntable){
    mydbf <- paste(fntable, ".DBF", sep = "")
    table_exists <- mydbf %in% dir(mytemp)
    table_exists
  }
  
  import_table <- function(fntable) {
    table_exists <- check_table(fntable)
    if(table_exists) {
      usethis::ui_done(paste0(fntable, " available for import"))
      mydbf <- paste(fntable, ".DBF", sep = "")
      mypaths <- file.path(mytemp, mydbf)
      rawdata <- foreign::read.dbf(mypaths, as.is = T)
      return(rawdata)
    } else {
      usethis::ui_oops(paste0(fntable, " is not available for import"))
    }
  }
  
  alldata <- lapply(AllTables, import_table)
  
  names(alldata) <- AllTables
  
  usethis::ui_done("Data has been imported with each table as a list.")
  
  # clean up temp files
  unlink(mytemp)
  
  # return final data
  alldata
  
}
