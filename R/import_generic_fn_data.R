#' import.generic.fn.data
#' @description A generic import function will open and compile multiple FN projects.  
#' Project path build is omitted so it must be provided in file path.  
#' This function allows the import of projects other than 'GL1' and 'TW1' but doesn't not
#' format the outputs to the same extent.
#' @param generic File path to FN DATA.ZIP folder
#' @param mytables Specify which FN data tables are to be imported.
#' @export
#' @return a list of FN2 tables
#' @examples 
#' \dontrun{
#' prjlist <- c("~/FNData/NS1/IA02_NS1/DATA.ZIP", "~/FNData/GL1/IA15_GL1/DATA.ZIP")
#' fn <- import.generic.fn.data(prjlist)
#' fn$FN011
#' lapply(fn, head)
#'}

import.generic.fn.data<-function(generic, mytables=all.tables){
  all.tables<-c('FN011', 'FN121', 'FN122', 'FN123','FN124','FN125','FN126','FN127')
  mytables<-toupper(mytables)
  if (!all(file.exists(generic))) {cat('Error: An invalid file path exists'); break}
  if (!all(mytables%in%all.tables)) {cat("Error: FN table not recognized"); break} # this is a different approach than import.fn.data
  for (i in 1:length(generic)){
    if (i==1) {year1<-import.generic.fn.data1(generic[i])}
    else {newdata<-import.generic.fn.data1(generic[i])
        year1$FN011<-dplyr::bind_rows(year1$FN011, newdata$FN011)
        year1$FN121<-dplyr::bind_rows(year1$FN121, newdata$FN121)
        year1$FN122<-dplyr::bind_rows(year1$FN122, newdata$FN122)
        year1$FN123<-dplyr::bind_rows(year1$FN123, newdata$FN123)
        year1$FN124<-dplyr::bind_rows(year1$FN124, newdata$FN124)
        year1$FN125<-dplyr::bind_rows(year1$FN125, newdata$FN125)
        year1$FN126<-dplyr::bind_rows(year1$FN126, newdata$FN126)
        year1$FN127<-dplyr::bind_rows(year1$FN127, newdata$FN127)
    }
  }
  year1
}
