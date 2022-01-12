#' import_fn_index_net
#' @description A generic import function similar to open.fn.data1 but without interactivity.. 
#' Project path build is omitted so it must be provided in file path.
#' @param generic File path to FN DATA.ZIP folder
#' @export

import_fn_index_net<-function(generic_datazip) {
  # FN011 eventually needs LAKE and PROTOCOL
  
  
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
  
  # fix dates
  alldata <- lapply(alldata, FUN = function(x) {x |> purrr::map(fix_date) |> dplyr::bind_rows()})
  
  # fix SILOC
  if("SILOC" %in% names(alldata$FN121)) {
       alldata$FN121 <- SILOC2COORD(alldata$FN121, alldata$FN121$SILOC)
       alldata$FN121 <- alldata$FN121 |> dplyr::rename(DD_LAT0 = LAT, DD_LON0 = LON) |> 
         mutate(DD_LAT1 = NA, DD_LON1 = NA)
   } else if ("XSILOC1" %in% names(alldata$FN121)) {
    alldata$FN121 <- SILOC2COORD(alldata$FN121, alldata$FN121$XSILOC1)
    alldata$FN121 <- alldata$FN121 |> dplyr::rename(DD_LAT0 = LAT, DD_LON0 = LON)
   }
  
  if("XSILOC2" %in% names(alldata$FN121)) {
    alldata$FN121 <- SILOC2COORD(alldata$FN121, alldata$FN121$XSILOC2)
    alldata$FN121 <- alldata$FN121 |> dplyr::rename(DD_LAT1 = LAT, DD_LON1 = LON)
  }
  
  # change from character class to appropriate class
  alldata <- gl1.vartypes(alldata)
  
  # give the data to the user
  usethis::ui_done("Data has been imported with each table as a list.")
  alldata
  
}
