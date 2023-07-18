#' import creel frtables
#'
#' @param generic_datazip a file path to a single FN2 DATA.ZIP creel project
#' @description
#' This function will import the FR report tables from a single FN2 creel project 
#' that has been analyzed in FN2. FN2 generates several FR* report/summary tables.
#' Tables will be returned as items within a list.
#' 
#' @seealso [import_creel_series_frtables()] for combining multiple projects
#' @return list a list with "FR712", "FR713", "FR714" and "FR715" tables
#' @export
#'
#' @examples
#' \dontrun{
#' cr1_frtable <- import_creel_frtables("SC97_WL1/DATA.ZIP")
#' lapply(cr1_frtable, head)
#' }
#' 
#' 
import_creel_frtables <- function(generic_datazip) {
  if (length(generic_datazip) != 1) {
    usethis::ui_stop("This function can only import a single file")
    break
  }
  if (file.exists(generic_datazip)) {
    usethis::ui_done("File exists")
  }
  else {
    usethis::ui_stop("File not found. Check file path.")
  }
  has_zip <- grep(generic_datazip, pattern = "DATA\\.ZIP$")
  if (length(has_zip) == 1) {
    usethis::ui_done("DATA.ZIP input accepted")
  }
  else {
    usethis::ui_stop("Input file expects a DATA.ZIP file")
  }
  AllTables <- c("FR712", "FR713", "FR714", "FR715")
  mytemp <- tempdir()
  unzip(generic_datazip, exdir = mytemp)
  check_table <- function(fntable) {
    mydbf <- paste(fntable, ".DBF", sep = "")
    table_exists <- mydbf %in% dir(mytemp)
    table_exists
  }
  import_table <- function(fntable) {
    table_exists <- check_table(fntable)
    if (table_exists) {
      usethis::ui_done(paste0(fntable, " available for import"))
      mydbf <- paste(fntable, ".DBF", sep = "")
      mypaths <- file.path(mytemp, mydbf)
      rawdata <- foreign::read.dbf(mypaths, as.is = T)
      return(rawdata)
    }
    else {
      usethis::ui_oops(paste0(fntable, " is not available for import"))
    }
  }
  alldata <- lapply(AllTables, import_table)
  names(alldata) <- AllTables
  usethis::ui_done("Data has been imported with each table as a list.")
  unlink(mytemp)
  alldata
}


#' import creel series frtables
#'
#' @param datazips a vector of file paths to FN2 DATA.ZIP creel projects 
#' @description
#' This function is a wrapper for `import_creel_frtables` that will import the FR 
#' report tables from multiple FN2 creel projects 
#' that have been analyzed in FN2. FN2 generates several FR* report/summary tables.
#' Tables will be returned as items within a list.
#' 
#' @seealso [import_creel_frtables()] for combining multiple projects
#' @return list a list with "FR712", "FR713", "FR714" and "FR715" tables
#' @export
#'
#' @examples
#' \dontrun{
#' cr1_frtable <- import_creel_series_frtables("SC97_WL1/DATA.ZIP", "SC96_WL11/DATA.ZIP")
#' lapply(cr1_frtable, head)
#' }

import_creel_series_frtables <- function(datazips) {
  fr712 <- data.frame()
  fr713 <- data.frame()
  fr714 <- data.frame()
  fr715 <- data.frame()
  
  suppressWarnings(suppressMessages(fndat <- lapply(datazips,
                                                    import_creel_frtables)))
  
  
  for (i in 1:length(fndat)) {
    fr712 <- dplyr::bind_rows(fr712, fndat[[i]]$FR712)
    fr713 <- dplyr::bind_rows(fr713, fndat[[i]]$FR713)
    fr714 <- dplyr::bind_rows(fr714, fndat[[i]]$FR714)
    fr715 <- dplyr::bind_rows(fr715, fndat[[i]]$FR715)
  }
  all_FN_Data <- list(FR712 = fr712, FR713 = fr713, FR714 = fr714, FR715 = fr715)
  
  lapply(names(all_FN_Data), FUN = function(x) {
    usethis::ui_done(paste0(x, " has been imported"))
  })
  return(all_FN_Data)
}