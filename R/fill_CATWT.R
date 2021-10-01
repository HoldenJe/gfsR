#' fill.CATWT
#' @description Computes CATWT based on SUBCNT, SUBWT and CATCNT
#' @export

fill.CATWT<-function(fn123) {
  fn123$CATWT<- ifelse(!is.na(fn123$CATWT), fn123$CATWT, 
                       ifelse(fn123$CATCNT == fn123$SUBCNT, fn123$SUBWT,
                              ifelse(fn123$SUBWT>0, fn123$CATCNT/fn123$SUBCNT*fn123$SUBWT, NA)))
  if(anyNA(fn123$CATWT)) {cat("Still NA's in CATWT\n")}
  fn123
}