#' Import Generic FN DATA.ZIP
#' @description Provides a generic unzip function but with console prompts to add missing
#' FN011 fields.
#' @param generic_datazip 
#'
#' @return list of FN Tables
#' @export
#'
#' @examples
#' \dontrun{
#' fn <- import_raw_fn_data("~/FNData/NS1/IA02_NS1/DATA.ZIP")
#'}
import_raw_fn_data <- function(generic_datazip){
  # FN011 eventually needs LAKE and PROTOCOL
  get_lake <- function(){
    run_loop <- TRUE
    while(run_loop){
      question <- "Please select a lake:\n"
      options <- "1 = L. Ontario\n2 = L. Huron\n3= L. Erie\n4 = L. Superior\n"
      answer <-  "Selection:\n"
      cat(paste0(question, options, answer))
      lake <- readline(prompt = "")
      if(!(lake %in% 1:4)){
        usethis::ui_oops("That was not a valid selection")
      } else {run_loop <- FALSE}
    }
    
    lake_codes <- data.frame(code = c(1:4), lake_short = c("ON", "HU", "ER", "SU"))
    cat(paste0("you choose: "), lake_codes$lake_short[lake_codes$code == lake])
    cat("\n")
    lake_short_form = lake_codes$lake_short[lake_codes$code == lake]
    lake_short_form
  }
  
  mylake <- get_lake()
  
  get_protocol <- function(){
    run_protocol_loop <- TRUE
    while(run_protocol_loop){
      question <- "Please select a protocol:\n"
      options <- "1 = Gill net\n2 = Trawl\n3= Trap net\n4 = Other\n"
      answer <-  "Selection:\n"
      cat(paste0(question, options, answer))
      protocol <- readline(prompt = "")
      if(!(protocol %in% 1:4)){
        usethis::ui_oops("That was not a valid selection")
      } else {run_protocol_loop <- FALSE}
    }
    
    protocol_codes <- data.frame(code = c(1:4), protocol_short = c("GN", "TR", "TN", "OTH"))
    cat(paste0("you choose: "), protocol_codes$protocol_short[protocol_codes$code == protocol])
    cat("\n")
    protocol_short_form = protocol_codes$protocol_short[protocol_codes$code == protocol]
    protocol_short_form
  }
  
  myprotocol <- get_protocol()
  
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
  alldata$FN011$LAKE <- mylake 
  alldata$FN011$PROTOCOL <- myprotocol
  
  usethis::ui_done("Data has been imported with each table as a list.")
  
  # clean up temp files
  unlink(mytemp)
  
  # return final data
  alldata
  
  
}