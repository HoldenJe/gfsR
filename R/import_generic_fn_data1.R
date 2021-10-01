#' import.generic.fn.data1
#' @description A generic import function similar to open.fn.data1.  Project path build is omitted so it must be provided in file path
#' @param generic File path to FN DATA.ZIP folder
#' @param mytables Specify which FN data tables are to be imported.
#' @export

import.generic.fn.data1<-function(generic, mytables=all.tables) {
  if(length(generic)!=1) {cat('Error: This function can only import a single file'); break}
  all.tables<-c('FN011', 'FN121', 'FN122', 'FN123','FN124','FN125','FN126','FN127')
  mytables<-toupper(mytables)
  if(!all(mytables%in% all.tables)) {cat('Are you sure all the tables specified exist?')}
  mytemp <- tempdir()
  unzip(generic, exdir = mytemp)
  mydbf <- paste(mytables, ".DBF", sep = "")
  mypaths <- file.path(mytemp, mydbf)
  rawdata <- lapply(mypaths, foreign::read.dbf, as.is = T)
  names(rawdata) <- mytables
  rawdata
}
