#' append.spc.names
#' @name append.spc.names
#' @title Append Species Names
#' @description function that appends species names and codes (SpcName) to a working data frame.
#' @param fntable a data table that has a column SPC indicating the species code (e.g. FN125)
#' @return returns a merged dataframe
#' @export
#' 

append.spc.names<-function(fntable){merge(fntable,SpcName,by="SPC", all.x=T)}
