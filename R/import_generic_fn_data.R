#' import.generic.fn.data
#' @description A generic import function similar to open.fn.data to open and compile multiple FN projects.  Project path build is omitted so it must be provided in file path.  
#' @description This function allows the import of projects other than 'GL1' and 'TW1'.
#' @param generic File path to FN DATA.ZIP folder
#' @param mytables Specify which FN data tables are to be imported.
#' @export

import.generic.fn.data<-function(generic, mytables=all.tables){
  all.tables<-c('FN011', 'FN121', 'FN122', 'FN123','FN124','FN125','FN126','FN127')
  mytables<-toupper(mytables)
  if (!all(file.exists(generic))) {cat('Error: An invalid file path exists'); break}
  if (!all(mytables%in%all.tables)) {cat("Error: FN table not recognized"); break} # this is a different approach than import.fn.data
  for (i in 1:length(generic)){
    if (i==1) {year1<-import.generic.fn.data1(generic[i])}
    else {newdata<-import.generic.fn.data1(generic[i])
        year1$FN011<-plyr::rbind.fill(year1$FN011, newdata$FN011)
        year1$FN121<-plyr::rbind.fill(year1$FN121, newdata$FN121)
        year1$FN122<-plyr::rbind.fill(year1$FN122, newdata$FN122)
        year1$FN123<-plyr::rbind.fill(year1$FN123, newdata$FN123)
        year1$FN124<-plyr::rbind.fill(year1$FN124, newdata$FN124)
        year1$FN125<-plyr::rbind.fill(year1$FN125, newdata$FN125)
        year1$FN126<-plyr::rbind.fill(year1$FN126, newdata$FN126)
        year1$FN127<-plyr::rbind.fill(year1$FN127, newdata$FN127)
    }
  }
  year1
}
