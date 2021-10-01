#' fill.CATCNT
#' @description Computes CATCNT based on SUBCNT, SUBWT and CATWT
#' @export

fill.CATCNT<-function(fn123) {
  fn123$CATCNT<-ifelse(!is.na(fn123$CATCNT), fn123$CATCNT, fn123$SUBCNT/fn123$SUBWT*fn123$CATWT)
  if(anyNA(fn123$CATCNT)) {cat("Still NA's in CATCNT\n")}
  fn123
}
