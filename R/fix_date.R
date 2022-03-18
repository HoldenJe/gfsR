#' fix_date
#' @description A generic function to fix dates in FN2 files. Dates after 2000 are recorded as 19XX.
#' @param fndt0 A date field in FN2 files
#' @return a formatted date field
#' @export
#' @examples fix_date(as.Date("1920-07-10")); fix_date(as.Date("1990-07-10")) 

fix_date <- function(fndt0) {
  if(lubridate::is.Date(fndt0)){
    dtfixed <- as.Date(ifelse(fndt0<"1940-01-01", 
                 format(fndt0,"20%y-%m-%d"),
                 format(fndt0)))
  dtfixed 
  } else {fndt0}
}

