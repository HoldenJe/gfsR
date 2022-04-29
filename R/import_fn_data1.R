#' Import a single FN project from a DATA.ZIP file
#'
#' @description This function will generally not be called directly. It is mainly intended to be called from with import.fn.data
#' @param mydir should specify the local data warehouse ('c:/FN_Data')
#' @param yr is a field season year (e.g. 2013), 
#' @param program is a character vector specifying which program to build a project code for ('GL1')
#' @param mytables is either a single FN2 table ('FN121') or a list of tables ('FN121', 'FN125').  If no value is supplied the default import routine is to import all supported tables.
#' @return returns a list that contains each FN2 table as an item in the list
#' @export
#' 

import.fn.data1<-function(mydir, yr, program, mytables=all.tables) {
  # check number of years - delete eventually
  if (length(yr)>1) {stop ('only works for 1 year') }
  
  # check program
  program.list<-c('TW1', 'GL1', 'NS1', '061', '062', 'TW2', 'TW3')
  if (length(program)>1) {stop ('select only 1 program')}
  if (!(program %in% program.list)) {stop (sprintf('%s can not use this function',program))}
  
  # check tables
  all.tables<-c('FN011', 'FN121', 'FN122', 'FN123','FN124','FN125','FN126','FN127')
  mytables<-toupper(mytables)
  for (i in mytables){
  if (!(i %in% all.tables)) {stop (sprintf('%s is not available with this function',i))}
  }
  
  # set up import directory path
  prj_cd<-make.project(yr,program)
  fnpath<-paste(mydir,program,prj_cd,sep="/")
  fnopen<- paste(fnpath,"DATA.ZIP", sep="/")
 
  # unzip to temp dir
  mytemp<-tempdir()
  if (!(file.exists(fnopen))) {stop (sprintf('check file path, %s not a valid directory', fnopen))}
  unzip(fnopen, exdir=mytemp)

  # make table names in to file paths and append ".DBF"
  mydbf<-paste(mytables, ".DBF", sep="")
  mypaths<-file.path(mytemp,mydbf)
  
  # read each dbf in to list
  rawdata<-lapply(mypaths, foreign::read.dbf, as.is=T)
  names(rawdata)<-mytables
  
  unlink(mytemp)
  rawdata
  
}
