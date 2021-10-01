#' fl.rwt
#' 
#' @details provides a quick diagnostic plot for looking for data errors
#' @param species three digit species code (e.g. '081')
#' @param my125 the location of the FN125 data (e.g. fndata$FN125)
#' @param logplot optional argument to plot a log-log plot, default = FALSE
#' @export

fl.rwt<-function(species, my125, logplot=FALSE){
  mydata2<-my125[my125$SPC==species,]
  if (!('RWT' %in% names(mydata2) & 'FLEN' %in% names(mydata2))) {stop ("Either FLEN or RWT data is missing")}
  if (logplot==FALSE){
    plot(RWT~FLEN, data=mydata2, pch=18, xlab="FLEN (mm)", ylab="RWT (g)")
  }		
  else {plot(log(RWT)~log(FLEN), data=mydata2, pch=18)
        abline(lm(log(RWT)~log(FLEN), data=mydata2))
  }
  if ("SPC_NM" %in% names(mydata2)){
    title(as.character(mydata2$SPC_NM[1]))
  }
}
