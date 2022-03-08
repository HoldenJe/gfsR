#' import_fn_creel
#' @description A generic import function similar to open.fn.data1 but for creel files.  
#' Project path build is omitted so it must be provided in file path
#' @param generic File path to FN DATA.ZIP folder
#' @export

import_fn_creel <- function(generic_datazip) {
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
  
  AllTables <- c("FN011", "FN022", "FN023", "FN024", "FN025", "FN026", "FN028", "FN111", 
                  "FN112", "FN121", "FN123", "FN125", "FN126", "FN127")
  
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
