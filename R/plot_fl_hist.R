#' fl.hist
#' 
#' @description Fork length histogram
#' @param species three digit species code (e.g. '081')
#' @param my125 the location of the FN125 data (e.g. fndata$FN125)
#' @export 
#' 

fl.hist<-function(species, my125){
  if (!('FLEN' %in% names(my125))) {stop ("No FLEN data found")}
  mydata2<-my125[my125$SPC==species,]
  hist(mydata2$FLEN, main="", col='grey', xlab="FLEN (mm)")
  if ("SPC_NM" %in% names(mydata2)){
    title(as.character(mydata2$SPC_NM[1]))
    }
}