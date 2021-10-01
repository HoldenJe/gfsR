#' Assign GL1 FN121 variable types
#' @description Changes the default 'chr' class of each variable to the desired class type (e.g. 'int') for GL1 data.  Generally this function will not be called directly as it is called from with 'import.fn.data'
#' @param gl1table is a list containing each FN table as an item in the list
#' @return returns a list that contains each FN2 table as an item in the list
#' @export

gl1.vartypes<-function(gl1table){
  # FN121
  mytables<-names(gl1table)
  if('FN121' %in% mytables){
  if("F1" %in% names(gl1table$FN121)) {gl1table$FN121$F1 <-as.factor(gl1table$FN121$F1)} 
  if('SAM' %in% names(gl1table$FN121)) {gl1table$FN121$SAM<-as.integer(gl1table$FN121$SAM)}
  if('AREA' %in% names(gl1table$FN121)) {gl1table$FN121$AREA<-as.factor(gl1table$FN121$AREA)}
  if('SITE' %in% names(gl1table$FN121)) {gl1table$FN121$SITE<-as.factor(gl1table$FN121$SITE)}
  if('XSILOC1' %in% names(gl1table$FN121)) {gl1table$FN121$XSILOC1<-as.numeric(gl1table$FN121$XSILOC1)}
  if('XSILOC2' %in% names(gl1table$FN121)) {gl1table$FN121$XSILOC2<-as.numeric(gl1table$FN121$XSILOC2)}
  if('EFFDUR' %in% names(gl1table$FN121)) {gl1table$FN121$EFFDUR<-as.numeric(gl1table$FN121$EFFDUR)}
  if('GR' %in% names(gl1table$FN121)) {gl1table$FN121$GR<-as.factor(gl1table$FN121$GR)}
  if('SIDEP' %in% names(gl1table$FN121)) {gl1table$FN121$SIDEP<-as.numeric(gl1table$FN121$SIDEP)}
  if('GRDEP' %in% names(gl1table$FN121)) {gl1table$FN121$GRDEP<-as.numeric(gl1table$FN121$GRDEP)}
  if('GRDEPMIN' %in% names(gl1table$FN121)) {gl1table$FN121$GRDEPMIN<-as.numeric(gl1table$FN121$GRDEPMIN)}
  if('GRDEPMAX' %in% names(gl1table$FN121)) {gl1table$FN121$GRDEPMAX<-as.numeric(gl1table$FN121$GRDEPMAX)}
  if('SITEM' %in% names(gl1table$FN121)) {gl1table$FN121$SITEM<-as.numeric(gl1table$FN121$SITEM)}
  if('GRTEM' %in% names(gl1table$FN121)) {gl1table$FN121$GRTEM<-as.numeric(gl1table$FN121$GRTEM)}
  if('STRATUM' %in% names(gl1table$FN121)) {gl1table$FN121$STRATUM<-as.factor(gl1table$FN121$STRATUM)}
  if('PRJ_CD' %in% names(gl1table$FN121)) {gl1table$FN121$PRJ_CD<-as.factor(gl1table$FN121$PRJ_CD)}
  if('XFOULING' %in% names(gl1table$FN121)) {gl1table$FN121$XFOULING<-as.factor(gl1table$FN121$XFOULING)}
  if("ENTRY" %in% names(gl1table$FN121)) {gl1table$FN121$ENTRY <-as.integer(gl1table$FN121$ENTRY)}
  }
  
  #FN123
  if ('FN123' %in% mytables){
    if("SAM" %in% names(gl1table$FN123)) {gl1table$FN123$SAM <-as.integer(gl1table$FN123$SAM)} 
    if("EFF" %in% names(gl1table$FN123)) {gl1table$FN123$EFF <-as.factor(gl1table$FN123$EFF)} 
    if("F3" %in% names(gl1table$FN123)) {gl1table$FN123$F3 <-as.factor(gl1table$FN123$F3)} 
    if("SPC" %in% names(gl1table$FN123)) {gl1table$FN123$SPC <-as.factor(gl1table$FN123$SPC)} 
    if("CATCNT" %in% names(gl1table$FN123)) {gl1table$FN123$CATCNT <-as.integer(gl1table$FN123$CATCNT)} 
    if("SUBCNT" %in% names(gl1table$FN123)) {gl1table$FN123$SUBCNT <-as.integer(gl1table$FN123$SUBCNT)} 
    if("SUBWT" %in% names(gl1table$FN123)) {gl1table$FN123$SUBWT <-as.numeric(gl1table$FN123$SUBWT)} 
    if("BIOCNT" %in% names(gl1table$FN123)) {gl1table$FN123$BIOCNT <-as.integer(gl1table$FN123$BIOCNT)} 
    #if("XFRSH_DT" %in% names(gl1table$FN123)) {gl1table$FN123$XFRSH_DT <-as.integer(gl1table$FN123$XFRSH_DT)} - keep as date
    #if("XFRZN_DT" %in% names(gl1table$FN123)) {gl1table$FN123$XFRZN_DT <-as.integer(gl1table$FN123$XFRZN_DT)} - keep as date
    if("XSPECNT" %in% names(gl1table$FN123)) {gl1table$FN123$XSPECNT <-as.integer(gl1table$FN123$XSPECNT)} 
    if("PRJ_CD" %in% names(gl1table$FN123)) {gl1table$FN123$PRJ_CD <-as.factor(gl1table$FN123$PRJ_CD)} 
    #if("V0" %in% names(gl1table$FN123)) {gl1table$FN123$V0 <-as.integer(gl1table$FN123$V0)}
    if("ENTRY" %in% names(gl1table$FN123)) {gl1table$FN123$ENTRY <-as.integer(gl1table$FN123$ENTRY)} 
    if("CATWT" %in% names(gl1table$FN123)) {gl1table$FN123$CATWT <-as.numeric(gl1table$FN123$CATWT)} 
    }
# FN124
if('FN124' %in% mytables){
  if("SAM" %in% names(gl1table$FN124)) {gl1table$FN124$SAM <-as.integer(gl1table$FN124$SAM)} 
  if("EFF" %in% names(gl1table$FN124)) {gl1table$FN124$EFF <-as.factor(gl1table$FN124$EFF)} 
  if("SPC" %in% names(gl1table$FN124)) {gl1table$FN124$SPC <-as.factor(gl1table$FN124$SPC)} 
  if("F4" %in% names(gl1table$FN124)) {gl1table$FN124$F4 <-as.factor(gl1table$FN124$F4)} 
  if("SIZ" %in% names(gl1table$FN124)) {gl1table$FN124$SIZ <-as.integer(gl1table$FN124$SIZ)} 
  if("SIZCNT" %in% names(gl1table$FN124)) {gl1table$FN124$SIZCNT <-as.integer(gl1table$FN124$SIZCNT)} 
  if("PRJ_CD" %in% names(gl1table$FN124)) {gl1table$FN124$PRJ_CD <-as.factor(gl1table$FN124$PRJ_CD)} 
  #if("V0" %in% names(gl1table$FN124)) {gl1table$FN124$V0 <-as.integer(gl1table$FN124$V0)} 
  if("ENTRY" %in% names(gl1table$FN124)) {gl1table$FN124$ENTRY <-as.integer(gl1table$FN124$ENTRY)} 
}

#FN125
if ('FN125' %in% mytables){
  if("SAM" %in% names(gl1table$FN125)) {gl1table$FN125$SAM <-as.integer(gl1table$FN125$SAM)} 
  if("EFF" %in% names(gl1table$FN125)) {gl1table$FN125$EFF <-as.factor(gl1table$FN125$EFF)} 
  if("SPC" %in% names(gl1table$FN125)) {gl1table$FN125$SPC <-as.factor(gl1table$FN125$SPC)} 
  if("F5" %in% names(gl1table$FN125)) {gl1table$FN125$F5 <-as.factor(gl1table$FN125$F5)} 
  if("FISH" %in% names(gl1table$FN125)) {gl1table$FN125$FISH <-as.integer(gl1table$FN125$FISH)} 
  if("FLEN" %in% names(gl1table$FN125)) {gl1table$FN125$FLEN <-as.integer(gl1table$FN125$FLEN)} 
  if("TLEN" %in% names(gl1table$FN125)) {gl1table$FN125$TLEN <-as.integer(gl1table$FN125$TLEN)} 
  if("RWT" %in% names(gl1table$FN125)) {gl1table$FN125$RWT <-as.numeric(gl1table$FN125$RWT)} 
  if("XEVIS_WT" %in% names(gl1table$FN125)) {gl1table$FN125$XEVIS_WT <-as.integer(gl1table$FN125$XEVIS_WT)} 
  if("SEX" %in% names(gl1table$FN125)) {gl1table$FN125$SEX <-as.factor(gl1table$FN125$SEX)} 
  if("GON" %in% names(gl1table$FN125)) {gl1table$FN125$GON <-as.factor(gl1table$FN125$GON)} 
  if("GONWT" %in% names(gl1table$FN125)) {gl1table$FN125$GONWT <-as.integer(gl1table$FN125$GONWT)} 
  if("CLIPC" %in% names(gl1table$FN125)) {gl1table$FN125$CLIPC <-as.factor(gl1table$FN125$CLIPC)} 
  #if("NODC" %in% names(gl1table$FN125)) {gl1table$FN125$NODC <-as.integer(gl1table$FN125$NODC)} 
  if("TAGSTAT" %in% names(gl1table$FN125)) {gl1table$FN125$TAGSTAT <-as.factor(gl1table$FN125$TAGSTAT)} 
  #if("TAGID" %in% names(gl1table$FN125)) {gl1table$FN125$TAGID <-as.integer(gl1table$FN125$TAGID)} 
  #if("TAGDOC" %in% names(gl1table$FN125)) {gl1table$FN125$TAGDOC <-as.integer(gl1table$FN125$TAGDOC)} 
  #if("LAMIJC" %in% names(gl1table$FN125)) {gl1table$FN125$LAMIJC <-as.integer(gl1table$FN125$LAMIJC)} 
  #if("AGEST" %in% names(gl1table$FN125)) {gl1table$FN125$AGEST <-as.integer(gl1table$FN125$AGEST)} 
  #if("TISSUE" %in% names(gl1table$FN125)) {gl1table$FN125$TISSUE <-as.integer(gl1table$FN125$TISSUE)} 
  #if("XHEAD_BOX" %in% names(gl1table$FN125)) {gl1table$FN125$XHEAD_BOX <-as.integer(gl1table$FN125$XHEAD_BOX)} 
  if("XSTOM_WT" %in% names(gl1table$FN125)) {gl1table$FN125$XSTOM_WT <-as.integer(gl1table$FN125$XSTOM_WT)} 
  #if("COMMENT5" %in% names(gl1table$FN125)) {gl1table$FN125$COMMENT5 <-as.integer(gl1table$FN125$COMMENT5)} 
  if("PRJ_CD" %in% names(gl1table$FN125)) {gl1table$FN125$PRJ_CD <-as.factor(gl1table$FN125$PRJ_CD)} 
  #if("V0" %in% names(gl1table$FN125)) {gl1table$FN125$V0 <-as.integer(gl1table$FN125$V0)} 
  if("ENTRY" %in% names(gl1table$FN125)) {gl1table$FN125$ENTRY <-as.integer(gl1table$FN125$ENTRY)} 
  #if("XHETERO" %in% names(gl1table$FN125)) {gl1table$FN125$XHETERO <-as.integer(gl1table$FN125$XHETERO)} 
  #if("XCONTAM" %in% names(gl1table$FN125)) {gl1table$FN125$XCONTAM <-as.integer(gl1table$FN125$XCONTAM)} 
  #if("XVFI" %in% names(gl1table$FN125)) {gl1table$FN125$XVFI <-as.integer(gl1table$FN125$XVFI)} 
  if("AGE" %in% names(gl1table$FN125)) {gl1table$FN125$AGE <-as.integer(gl1table$FN125$AGE)} 
  #if("XISOTOPE" %in% names(gl1table$FN125)) {gl1table$FN125$XISOTOPE <-as.integer(gl1table$FN125$XISOTOPE)} 
  #if("XSTOM" %in% names(gl1table$FN125)) {gl1table$FN125$XSTOM <-as.integer(gl1table$FN125$XSTOM)} 
}

# FN126
if ('FN126' %in% (mytables)){
  if("SAM" %in% names(gl1table$FN126)) {gl1table$FN126$SAM <-as.integer(gl1table$FN126$SAM)} 
  if("EFF" %in% names(gl1table$FN126)) {gl1table$FN126$EFF <-as.factor(gl1table$FN126$EFF)} 
  if("SPC" %in% names(gl1table$FN126)) {gl1table$FN126$SPC <-as.factor(gl1table$FN126$SPC)} 
  if("FISH" %in% names(gl1table$FN126)) {gl1table$FN126$FISH <-as.integer(gl1table$FN126$FISH)} 
  if("F6" %in% names(gl1table$FN126)) {gl1table$FN126$F6 <-as.factor(gl1table$FN126$F6)} 
  if("FOOD" %in% names(gl1table$FN126)) {gl1table$FN126$FOOD <-as.integer(gl1table$FN126$FOOD)} 
  if("TAXON" %in% names(gl1table$FN126)) {gl1table$FN126$TAXON <-as.factor(gl1table$FN126$TAXON)} 
  if("LF" %in% names(gl1table$FN126)) {gl1table$FN126$LF <-as.integer(gl1table$FN126$LF)} 
  if("FDCNT" %in% names(gl1table$FN126)) {gl1table$FN126$FDCNT <-as.integer(gl1table$FN126$FDCNT)} 
  #if("FDMES" %in% names(gl1table$FN126)) {gl1table$FN126$FDMES <-as.integer(gl1table$FN126$FDMES)} 
  #if("FDVAL" %in% names(gl1table$FN126)) {gl1table$FN126$FDVAL <-as.integer(gl1table$FN126$FDVAL)} 
  if("PRJ_CD" %in% names(gl1table$FN126)) {gl1table$FN126$PRJ_CD <-as.factor(gl1table$FN126$PRJ_CD)} 
  if("V0" %in% names(gl1table$FN126)) {gl1table$FN126$V0 <-as.integer(gl1table$FN126$V0)} 
  if("ENTRY" %in% names(gl1table$FN126)) {gl1table$FN126$ENTRY <-as.integer(gl1table$FN126$ENTRY)} 
}

# FN127
if ('FN127' %in% (mytables)) {
  if("SAM" %in% names(gl1table$FN127)) {gl1table$FN127$SAM <-as.integer(gl1table$FN127$SAM)} 
  if("EFF" %in% names(gl1table$FN127)) {gl1table$FN127$EFF <-as.factor(gl1table$FN127$EFF)} 
  if("SPC" %in% names(gl1table$FN127)) {gl1table$FN127$SPC <-as.factor(gl1table$FN127$SPC)} 
  if("FISH" %in% names(gl1table$FN127)) {gl1table$FN127$FISH <-as.integer(gl1table$FN127$FISH)} 
  if("F7" %in% names(gl1table$FN127)) {gl1table$FN127$F7 <-as.factor(gl1table$FN127$F7)} 
  if("AGEID" %in% names(gl1table$FN127)) {gl1table$FN127$AGEID <-as.integer(gl1table$FN127$AGEID)} 
  #if("AGEMT" %in% names(gl1table$FN127)) {gl1table$FN127$AGEMT <-as.integer(gl1table$FN127$AGEMT)} 
  #if("AGE" %in% names(gl1table$FN127)) {gl1table$FN127$AGE <-as.integer(gl1table$FN127$AGE)} 
  if("NCA" %in% names(gl1table$FN127)) {gl1table$FN127$NCA <-as.integer(gl1table$FN127$NCA)} 
  #if("XCSA" %in% names(gl1table$FN127)) {gl1table$FN127$XCSA <-as.integer(gl1table$FN127$XCSA)} 
  #if("EDGE" %in% names(gl1table$FN127)) {gl1table$FN127$EDGE <-as.integer(gl1table$FN127$EDGE)} 
  #if("XEDGE" %in% names(gl1table$FN127)) {gl1table$FN127$XEDGE <-as.integer(gl1table$FN127$XEDGE)} 
  if("CONF" %in% names(gl1table$FN127)) {gl1table$FN127$CONF <-as.integer(gl1table$FN127$CONF)} 
  if("AGEA" %in% names(gl1table$FN127)) {gl1table$FN127$AGEA <-as.integer(gl1table$FN127$AGEA)} 
  #if("AGEVAL" %in% names(gl1table$FN127)) {gl1table$FN127$AGEVAL <-as.integer(gl1table$FN127$AGEVAL)} 
  #if("COMMENT7" %in% names(gl1table$FN127)) {gl1table$FN127$COMMENT7 <-as.integer(gl1table$FN127$COMMENT7)} 
  if("PRJ_CD" %in% names(gl1table$FN127)) {gl1table$FN127$PRJ_CD <-as.factor(gl1table$FN127$PRJ_CD)} 
  if("V0" %in% names(gl1table$FN127)) {gl1table$FN127$V0 <-as.integer(gl1table$FN127$V0)} 
  if("ENTRY" %in% names(gl1table$FN127)) {gl1table$FN127$ENTRY <-as.integer(gl1table$FN127$ENTRY)} 
}

gl1table

# end function
}