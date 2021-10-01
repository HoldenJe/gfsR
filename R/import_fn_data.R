#' import.fn.data is the generic function used to load data
#' @description This is primary import function for loading data from archived FishNet2 projects.  It assumes that data is archived using the existing GFS data file index (e.g. "/TW1/LOA_IA13/DATA.ZIP")
#' @param mydir should specify the local data warehouse ('c:/FN_Data')
#' @param yr is a field season year (e.g. 2013), 
#' @param program is a character vector specifying which program to build a project code for ('GL1')
#' @param mytables is either a single FN2 table ('FN121') or a list of tables ('FN121', 'FN125').  If no value is supplied the default import routine is to import all supported tables.
#' @return returns a list that contains each FN2 table as an item in the list
#' @export
#' 
import.fn.data<-function(mydir, yr, program, mytables=all.tables) {
 all.tables<-c('FN011', 'FN121', 'FN122', 'FN123','FN124','FN125','FN126','FN127')
  if (length(yr)>1) {
  for (i in 1:length(yr)){
   if (i == 1) {year1<-import.fn.data1(mydir, yr[i],program, mytables)
   } else {
     year.next<- import.fn.data1(mydir, yr[i], program, mytables)
     if('FN011' %in% mytables){year1$FN011<-rbind.fill(year1$FN011, year.next$FN011)}
     if('FN121' %in% mytables){year1$FN121<-rbind.fill(year1$FN121, year.next$FN121)}
     if('FN122' %in% mytables){year1$FN122<-rbind.fill(year1$FN122, year.next$FN122)}
     if('FN123' %in% mytables){year1$FN123<-suppressWarnings(rbind.fill(year1$FN123, year.next$FN123))} #suppressWarnings needed for problems with xFRZN_DT and xFRSH_DT   
     if('FN124' %in% mytables){year1$FN124<-rbind.fill(year1$FN124, year.next$FN124)}    
     if('FN125' %in% mytables){year1$FN125<-rbind.fill(year1$FN125, year.next$FN125)}    
     if('FN126' %in% mytables){year1$FN126<-rbind.fill(year1$FN126, year.next$FN126)}    
     if('FN127' %in% mytables){year1$FN127<-rbind.fill(year1$FN127, year.next$FN127)}
     }
  }
 } else {year1<-import.fn.data1(mydir, yr,program, mytables)}
 
 
 # Fix variable types
 if(program == 'GL1') {all.fndata<-suppressWarnings(gl1.vartypes(year1))}
 if(program %in% c('TW1', 'TW2', 'TW3')) {all.fndata<-suppressWarnings(tw1.vartypes(year1))} # not sure what is causing the NAs introduced by coercion error here.
 if(program == 'NS1') {all.fndata<-suppressWarnings(ns1.vartypes(year1))}
 if(program == '061') {all.fndata<-suppressWarnings(tw061.vartypes(year1))}
 if(program == '062') {all.fndata<-suppressWarnings(tw062.vartypes(year1))}
 
 if(program == 'GL1' | program == 'NS1' & 'FN121' %in% mytables) {
    all.fndata$FN011$PRJ_DATE0<-as.Date(ifelse(all.fndata$FN011$PRJ_DATE0<"1940-01-01", format(all.fndata$FN011$PRJ_DATE0,"20%y-%m-%d"),format(all.fndata$FN011$PRJ_DATE0)))
    all.fndata$FN011$PRJ_DATE1<-as.Date(ifelse(all.fndata$FN011$PRJ_DATE1<"1940-01-01", format(all.fndata$FN011$PRJ_DATE1,"20%y-%m-%d"),format(all.fndata$FN011$PRJ_DATE1)))
    all.fndata$FN121$EFFDT0<-as.Date(ifelse(all.fndata$FN121$EFFDT0<"1940-01-01", format(all.fndata$FN121$EFFDT0,"20%y-%m-%d"),format(all.fndata$FN121$EFFDT0)))
    all.fndata$FN121$EFFDT1<-as.Date(ifelse(all.fndata$FN121$EFFDT1<"1940-01-01", format(all.fndata$FN121$EFFDT1,"20%y-%m-%d"),format(all.fndata$FN121$EFFDT1)))
    all.fndata$FN121$xEFFDUR<-all.fndata$FN121$EFFDT1 - all.fndata$FN121$EFFDT0
    all.fndata$FN121$YEAR<-year(all.fndata$FN121$EFFDT0)
 }
  if(program %in% c('TW1', 'TW2', 'TW3') | program == '061'| program == '062' & 'FN121' %in% mytables) {
    all.fndata$FN121$DATE<-as.Date(ifelse(all.fndata$FN121$DATE<"1940-01-01", format(all.fndata$FN121$DATE,"20%y-%m-%d"),format(all.fndata$FN121$DATE)))
    all.fndata$FN121$YEAR<-year(all.fndata$FN121$DATE)
    all.fndata$FN121$EFFTM0<-as.POSIXct(paste(all.fndata$FN121$DATE,as.character(all.fndata$FN121$EFFTM0)),format = '%Y-%m-%d %H:%M')
    all.fndata$FN121$EFFTM1<-as.POSIXct(paste(all.fndata$FN121$DATE,as.character(all.fndata$FN121$EFFTM1)),format = '%Y-%m-%d %H:%M')
    all.fndata$FN121$EFFDUR<-round(as.numeric(all.fndata$FN121$EFFTM1-all.fndata$FN121$EFFTM0)/60,2)
  }
  
 if ('FN121' %in% mytables) {all.fndata$FN121$PROG<-as.factor(program)}
 
 cat ('The following tables are available:\n')
 cat((names(all.fndata)))
 #cat('\nIndividual tables can be accessed using all.fndata$FN121\n')
 cat('\n')
 
 all.fndata
}