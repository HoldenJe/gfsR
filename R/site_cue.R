#' site.cue
#' @description calculates CUE by area on CATCNT of CATWT
#' @export

site.cue<-function(NorW, fn121, fn123, psuedo=TRUE, ...) {
  sam.data<-net.total(NorW, fn121, fn123)
  if (psuedo==TRUE) {sam.data<-aggregate(CATSTAN~PROG+YEAR+PRJ_CD+DATE+AREA+SITE+SPC, data=sam.data, mean)}
    
  sam.data<-as.data.frame(as.list(
      aggregate(CATSTAN~PROG+YEAR+PRJ_CD+AREA+SITE+SPC, data=sam.data, 
              FUN=function(x){c(mn=mean(x), n=length(x), sdev=sd(x))})))
  
  sam.data$CATSTAN.mn<-round(sam.data$CATSTAN.mn, 2)
  
  if (NorW == "CATCNT") {names(sam.data)<-c("PROG", 'YEAR', "PRJ_CD", "AREA", "SITE", "SPC", "CUE", 'n', 'sdev')}
  if (NorW == "CATWT") {names(sam.data)<-c("PROG", 'YEAR', "PRJ_CD", "AREA", "SITE", "SPC", "CUEW", 'n', 'sdev')}
  
  sam.data$n<-as.integer(sam.data$n)
  sam.data$sdev<-round(sam.data$sdev, 2)
  
  s.a<-unique(sam.data[,c("AREA", "SITE")])
  PRJ<-unique(sam.data[,c("PRJ_CD", "YEAR")])
  site.area<-merge(s.a,PRJ)
  site.area<- merge(site.area, as.data.frame(levels(sam.data$SPC)))
  site.area$PROG<-levels(sam.data$PROG)
  mycols<-c('AREA', 'SITE', 'PRJ_CD', 'YEAR', 'SPC', 'PROG')
  names(site.area)<-mycols
  site.cue<-merge(sam.data, site.area, by=mycols, all.y=T)
  
  # return
  site.cue[order(site.cue$YEAR),]
}