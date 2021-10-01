#' Makes a FN2 project directory name
#' 
#' Takes a year and a program type and forms a project directory code. Generally only used within other functions.
#' @param  yr is a field season year (e.g. 2013) 
#' @param  program is a character vector specifying a program ('GL1')
#' @return returns a project directory code ('IA13_GL1') 
#' @export

make.project<-function(yr,program){
  # Convert a year (or vector of years)(yr) and a gear (e.g. TW1)
  # to project code (e.g. IA10_GL1)
  program.list<-c('TW1','GL1','NS1','GLR', '062', '061', 'TW2', 'TW3')
  if(is.numeric(yr)) {yr<-as.character(yr)}
  yr<-substr(yr,3,4)
  program2<-toupper(program)
  if (program2 %in% program.list){
    project<-paste("IA",yr,"_",program2,sep="")
    project
  } else {stop(paste(program, "is not a valid program"))}
}
