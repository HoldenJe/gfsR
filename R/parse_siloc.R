#' Parses LAT from a 12 character SILOC
#' @description Helper function for SILOC2COORD to get LAT
#' @export

SILOCgetLAT12<-function(fn121column) {
  LATa<-substr(fn121column,1,6)
  LATdeg<-as.integer(substr(LATa, 1,2))
  LATmin<-as.integer(substr(LATa, 3,4))
  LATsec<-as.integer(substr(LATa, 5,6))
  LAT<-(LATdeg + (LATmin +(LATsec)/100)/60)
  LAT
}

#' Parses LON from a 12 character SILOC
#' @description Helper function for SILOC2COORD to get LON
#' @export
#'

SILOCgetLON12<-function(fn121column) {
  LATa<-substr(fn121column,7,12)
  LATdeg<-as.integer(substr(LATa, 1,2))
  LATmin<-as.integer(substr(LATa, 3,4))
  LATsec<-as.integer(substr(LATa, 5,6))
  LAT<-(LATdeg + (LATmin +(LATsec)/100)/60)*(-1)
  LAT
}

#' Parses LAT from a 10 character SILOC
#' @description Helper function for SILOC2COORD to get LAT
#' @export

SILOCgetLAT10<-function(fn121column) {
  LATa<-substr(fn121column,1,5)
  LATdeg<-as.integer(substr(LATa, 1, 2))
  LATmin<-as.integer(substr(LATa, 3, 4))
  LATsec<-as.integer(substr(LATa, 5, 5))
  LAT<-(LATdeg + (LATmin +(LATsec)/10)/60)
  LAT
}

#' Parses LON from a 10 characterSILOC
#' @description Helper function for SILOC2COORD to get LON
#' @export
#'

SILOCgetLON10<-function(fn121column) {
  LATa<-substr(fn121column,6,10)
  LATdeg<-as.integer(substr(LATa, 1, 2))
  LATmin<-as.integer(substr(LATa, 3, 4))
  LATsec<-as.integer(substr(LATa, 5, 5))
  LAT<-(LATdeg + (LATmin +(LATsec)/10)/60)*(-1)
  LAT
}


#' Convert XSILOC1 FN2 Field to LAT and LON fields
#' @description XSILOC1 is a 12 character field that records LAT and LON in the format
#' DDMMddDDMMdd (LAT then LON). This function uses two helper functions to parse the field and add a
#' LAT and LON field to the FN121 table provided.
#' @export

SILOC2COORD<-function(fn121, fn121column){
  fn121$LAT<-ifelse(nchar(fn121column) == 12,
                    SILOCgetLAT12(fn121column),
                    ifelse(nchar(fn121column) == 10,
                           SILOCgetLAT10(fn121column), NA))
  fn121$LON<-ifelse(nchar(fn121column) == 12,
                    SILOCgetLON12(fn121column),
                    ifelse(nchar(fn121column) == 10,
                           SILOCgetLON10(fn121column), NA))
  fn121
}
