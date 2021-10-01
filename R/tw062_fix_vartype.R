#' Assign tw062 variable types
#' 
#' @description Changes the default 'chr' class of each variable to the desired class type (e.g. 'int') for GL1 data.  Generally this function will not be called directly as it is called from with 'import.fn.data'
#' @param tw062table is a list containing each FN table as an item in the list
#' @return returns a list that contains each FN2 table as an item in the list
#' @export
#' 
tw062.vartypes<-function(tw062table){
  # FN121
  mytables<-names(tw062table)
  if('FN121' %in% mytables){
    if("F1" %in% names(tw062table$FN121)) {tw062table$FN121$F1 <-as.factor(tw062table$FN121$F1)} 
    if('SAM' %in% names(tw062table$FN121)) {tw062table$FN121$SAM<-as.integer(tw062table$FN121$SAM)}
    if('AREA' %in% names(tw062table$FN121)) {tw062table$FN121$AREA<-as.factor(tw062table$FN121$AREA)}
    if('SITE' %in% names(tw062table$FN121)) {tw062table$FN121$SITE<-as.factor(tw062table$FN121$SITE)}
    if('XSILOC1' %in% names(tw062table$FN121)) {tw062table$FN121$XSILOC1<-as.numeric(tw062table$FN121$XSILOC1)}
    if('XSILOC2' %in% names(tw062table$FN121)) {tw062table$FN121$XSILOC2<-as.numeric(tw062table$FN121$XSILOC2)}
    if('EFFDUR' %in% names(tw062table$FN121)) {tw062table$FN121$EFFDUR<-as.numeric(tw062table$FN121$EFFDUR)}
    if("EFFDST" %in% names(tw062table$FN121)) {tw062table$FN121$EFFDST <-as.integer(tw062table$FN121$EFFDST)} 
    if('GR' %in% names(tw062table$FN121)) {tw062table$FN121$GR<-as.factor(tw062table$FN121$GR)}
    if('SIDEP' %in% names(tw062table$FN121)) {tw062table$FN121$SIDEP<-as.numeric(tw062table$FN121$SIDEP)}
    if('GRDEP' %in% names(tw062table$FN121)) {tw062table$FN121$GRDEP<-as.numeric(tw062table$FN121$GRDEP)}
    if('GRDEPMIN' %in% names(tw062table$FN121)) {tw062table$FN121$GRDEPMIN<-as.numeric(tw062table$FN121$GRDEPMIN)}
    if('GRDEPMAX' %in% names(tw062table$FN121)) {tw062table$FN121$GRDEPMAX<-as.numeric(tw062table$FN121$GRDEPMAX)}
    if('SITEM' %in% names(tw062table$FN121)) {tw062table$FN121$SITEM<-as.numeric(tw062table$FN121$SITEM)}
    if('GRTEM' %in% names(tw062table$FN121)) {tw062table$FN121$GRTEM<-as.numeric(tw062table$FN121$GRTEM)}
    if('STRATUM' %in% names(tw062table$FN121)) {tw062table$FN121$STRATUM<-as.factor(tw062table$FN121$STRATUM)}
    if('PRJ_CD' %in% names(tw062table$FN121)) {tw062table$FN121$PRJ_CD<-as.factor(tw062table$FN121$PRJ_CD)}
    if('XFOULING' %in% names(tw062table$FN121)) {tw062table$FN121$XFOULING<-as.factor(tw062table$FN121$XFOULING)}
    if("ENTRY" %in% names(tw062table$FN121)) {tw062table$FN121$ENTRY <-as.integer(tw062table$FN121$ENTRY)}
  }
  
  #FN123
  if ('FN123' %in% mytables){
    if("SAM" %in% names(tw062table$FN123)) {tw062table$FN123$SAM <-as.integer(tw062table$FN123$SAM)} 
    if("EFF" %in% names(tw062table$FN123)) {tw062table$FN123$EFF <-as.factor(tw062table$FN123$EFF)} 
    if("F3" %in% names(tw062table$FN123)) {tw062table$FN123$F3 <-as.factor(tw062table$FN123$F3)} 
    if("SPC" %in% names(tw062table$FN123)) {tw062table$FN123$SPC <-as.factor(tw062table$FN123$SPC)} 
    if("CATCNT" %in% names(tw062table$FN123)) {tw062table$FN123$CATCNT <-as.integer(tw062table$FN123$CATCNT)} 
    if("SUBCNT" %in% names(tw062table$FN123)) {tw062table$FN123$SUBCNT <-as.integer(tw062table$FN123$SUBCNT)} 
    if("SUBWT" %in% names(tw062table$FN123)) {tw062table$FN123$SUBWT <-as.numeric(tw062table$FN123$SUBWT)} 
    if("BIOCNT" %in% names(tw062table$FN123)) {tw062table$FN123$BIOCNT <-as.integer(tw062table$FN123$BIOCNT)} 
    #if("XFRSH_DT" %in% names(tw062table$FN123)) {tw062table$FN123$XFRSH_DT <-as.integer(tw062table$FN123$XFRSH_DT)} - keep as date
    #if("XFRZN_DT" %in% names(tw062table$FN123)) {tw062table$FN123$XFRZN_DT <-as.integer(tw062table$FN123$XFRZN_DT)} - keep as date
    if("XSPECNT" %in% names(tw062table$FN123)) {tw062table$FN123$XSPECNT <-as.integer(tw062table$FN123$XSPECNT)} 
    if("PRJ_CD" %in% names(tw062table$FN123)) {tw062table$FN123$PRJ_CD <-as.factor(tw062table$FN123$PRJ_CD)} 
    #if("V0" %in% names(tw062table$FN123)) {tw062table$FN123$V0 <-as.integer(tw062table$FN123$V0)}
    if("ENTRY" %in% names(tw062table$FN123)) {tw062table$FN123$ENTRY <-as.integer(tw062table$FN123$ENTRY)} 
    if("CATWT" %in% names(tw062table$FN123)) {tw062table$FN123$CATWT <-as.numeric(tw062table$FN123$CATWT)} 
  }
  # FN124
  if('FN124' %in% mytables){
    if("SAM" %in% names(tw062table$FN124)) {tw062table$FN124$SAM <-as.integer(tw062table$FN124$SAM)} 
    if("EFF" %in% names(tw062table$FN124)) {tw062table$FN124$EFF <-as.factor(tw062table$FN124$EFF)} 
    if("SPC" %in% names(tw062table$FN124)) {tw062table$FN124$SPC <-as.factor(tw062table$FN124$SPC)} 
    if("F4" %in% names(tw062table$FN124)) {tw062table$FN124$F4 <-as.factor(tw062table$FN124$F4)} 
    if("SIZ" %in% names(tw062table$FN124)) {tw062table$FN124$SIZ <-as.integer(tw062table$FN124$SIZ)} 
    if("SIZCNT" %in% names(tw062table$FN124)) {tw062table$FN124$SIZCNT <-as.integer(tw062table$FN124$SIZCNT)} 
    if("PRJ_CD" %in% names(tw062table$FN124)) {tw062table$FN124$PRJ_CD <-as.factor(tw062table$FN124$PRJ_CD)} 
    #if("V0" %in% names(tw062table$FN124)) {tw062table$FN124$V0 <-as.integer(tw062table$FN124$V0)} 
    if("ENTRY" %in% names(tw062table$FN124)) {tw062table$FN124$ENTRY <-as.integer(tw062table$FN124$ENTRY)} 
  }
  
  #FN125
  if ('FN125' %in% mytables){
    if("SAM" %in% names(tw062table$FN125)) {tw062table$FN125$SAM <-as.integer(tw062table$FN125$SAM)} 
    if("EFF" %in% names(tw062table$FN125)) {tw062table$FN125$EFF <-as.factor(tw062table$FN125$EFF)} 
    if("SPC" %in% names(tw062table$FN125)) {tw062table$FN125$SPC <-as.factor(tw062table$FN125$SPC)} 
    if("F5" %in% names(tw062table$FN125)) {tw062table$FN125$F5 <-as.factor(tw062table$FN125$F5)} 
    if("FISH" %in% names(tw062table$FN125)) {tw062table$FN125$FISH <-as.integer(tw062table$FN125$FISH)} 
    if("FLEN" %in% names(tw062table$FN125)) {tw062table$FN125$FLEN <-as.integer(tw062table$FN125$FLEN)} 
    if("TLEN" %in% names(tw062table$FN125)) {tw062table$FN125$TLEN <-as.integer(tw062table$FN125$TLEN)} 
    if("RWT" %in% names(tw062table$FN125)) {tw062table$FN125$RWT <-as.numeric(tw062table$FN125$RWT)} 
    if("XEVIS_WT" %in% names(tw062table$FN125)) {tw062table$FN125$XEVIS_WT <-as.integer(tw062table$FN125$XEVIS_WT)} 
    if("SEX" %in% names(tw062table$FN125)) {tw062table$FN125$SEX <-as.factor(tw062table$FN125$SEX)} 
    if("GON" %in% names(tw062table$FN125)) {tw062table$FN125$GON <-as.factor(tw062table$FN125$GON)} 
    if("GONWT" %in% names(tw062table$FN125)) {tw062table$FN125$GONWT <-as.integer(tw062table$FN125$GONWT)} 
    if("CLIPC" %in% names(tw062table$FN125)) {tw062table$FN125$CLIPC <-as.factor(tw062table$FN125$CLIPC)} 
    #if("NODC" %in% names(tw062table$FN125)) {tw062table$FN125$NODC <-as.integer(tw062table$FN125$NODC)} 
    if("TAGSTAT" %in% names(tw062table$FN125)) {tw062table$FN125$TAGSTAT <-as.factor(tw062table$FN125$TAGSTAT)} 
    #if("TAGID" %in% names(tw062table$FN125)) {tw062table$FN125$TAGID <-as.integer(tw062table$FN125$TAGID)} 
    #if("TAGDOC" %in% names(tw062table$FN125)) {tw062table$FN125$TAGDOC <-as.integer(tw062table$FN125$TAGDOC)} 
    #if("LAMIJC" %in% names(tw062table$FN125)) {tw062table$FN125$LAMIJC <-as.integer(tw062table$FN125$LAMIJC)} 
    #if("AGEST" %in% names(tw062table$FN125)) {tw062table$FN125$AGEST <-as.integer(tw062table$FN125$AGEST)} 
    #if("TISSUE" %in% names(tw062table$FN125)) {tw062table$FN125$TISSUE <-as.integer(tw062table$FN125$TISSUE)} 
    #if("XHEAD_BOX" %in% names(tw062table$FN125)) {tw062table$FN125$XHEAD_BOX <-as.integer(tw062table$FN125$XHEAD_BOX)} 
    if("XSTOM_WT" %in% names(tw062table$FN125)) {tw062table$FN125$XSTOM_WT <-as.integer(tw062table$FN125$XSTOM_WT)} 
    #if("COMMENT5" %in% names(tw062table$FN125)) {tw062table$FN125$COMMENT5 <-as.integer(tw062table$FN125$COMMENT5)} 
    if("PRJ_CD" %in% names(tw062table$FN125)) {tw062table$FN125$PRJ_CD <-as.factor(tw062table$FN125$PRJ_CD)} 
    #if("V0" %in% names(tw062table$FN125)) {tw062table$FN125$V0 <-as.integer(tw062table$FN125$V0)} 
    if("ENTRY" %in% names(tw062table$FN125)) {tw062table$FN125$ENTRY <-as.integer(tw062table$FN125$ENTRY)} 
    #if("XHETERO" %in% names(tw062table$FN125)) {tw062table$FN125$XHETERO <-as.integer(tw062table$FN125$XHETERO)} 
    #if("XCONTAM" %in% names(tw062table$FN125)) {tw062table$FN125$XCONTAM <-as.integer(tw062table$FN125$XCONTAM)} 
    #if("XVFI" %in% names(tw062table$FN125)) {tw062table$FN125$XVFI <-as.integer(tw062table$FN125$XVFI)} 
    if("AGE" %in% names(tw062table$FN125)) {tw062table$FN125$AGE <-as.integer(tw062table$FN125$AGE)} 
    #if("XISOTOPE" %in% names(tw062table$FN125)) {tw062table$FN125$XISOTOPE <-as.integer(tw062table$FN125$XISOTOPE)} 
    #if("XSTOM" %in% names(tw062table$FN125)) {tw062table$FN125$XSTOM <-as.integer(tw062table$FN125$XSTOM)} 
  }
  
  # FN126
  if ('FN126' %in% (mytables)){
    if("SAM" %in% names(tw062table$FN126)) {tw062table$FN126$SAM <-as.integer(tw062table$FN126$SAM)} 
    if("EFF" %in% names(tw062table$FN126)) {tw062table$FN126$EFF <-as.factor(tw062table$FN126$EFF)} 
    if("SPC" %in% names(tw062table$FN126)) {tw062table$FN126$SPC <-as.factor(tw062table$FN126$SPC)} 
    if("FISH" %in% names(tw062table$FN126)) {tw062table$FN126$FISH <-as.integer(tw062table$FN126$FISH)} 
    if("F6" %in% names(tw062table$FN126)) {tw062table$FN126$F6 <-as.factor(tw062table$FN126$F6)} 
    if("FOOD" %in% names(tw062table$FN126)) {tw062table$FN126$FOOD <-as.integer(tw062table$FN126$FOOD)} 
    if("TAXON" %in% names(tw062table$FN126)) {tw062table$FN126$TAXON <-as.factor(tw062table$FN126$TAXON)} 
    if("LF" %in% names(tw062table$FN126)) {tw062table$FN126$LF <-as.integer(tw062table$FN126$LF)} 
    if("FDCNT" %in% names(tw062table$FN126)) {tw062table$FN126$FDCNT <-as.integer(tw062table$FN126$FDCNT)} 
    #if("FDMES" %in% names(tw062table$FN126)) {tw062table$FN126$FDMES <-as.integer(tw062table$FN126$FDMES)} 
    #if("FDVAL" %in% names(tw062table$FN126)) {tw062table$FN126$FDVAL <-as.integer(tw062table$FN126$FDVAL)} 
    if("PRJ_CD" %in% names(tw062table$FN126)) {tw062table$FN126$PRJ_CD <-as.factor(tw062table$FN126$PRJ_CD)} 
    if("V0" %in% names(tw062table$FN126)) {tw062table$FN126$V0 <-as.integer(tw062table$FN126$V0)} 
    if("ENTRY" %in% names(tw062table$FN126)) {tw062table$FN126$ENTRY <-as.integer(tw062table$FN126$ENTRY)} 
  }
  
  # FN127
  if ('FN127' %in% (mytables)) {
    if("SAM" %in% names(tw062table$FN127)) {tw062table$FN127$SAM <-as.integer(tw062table$FN127$SAM)} 
    if("EFF" %in% names(tw062table$FN127)) {tw062table$FN127$EFF <-as.factor(tw062table$FN127$EFF)} 
    if("SPC" %in% names(tw062table$FN127)) {tw062table$FN127$SPC <-as.factor(tw062table$FN127$SPC)} 
    if("FISH" %in% names(tw062table$FN127)) {tw062table$FN127$FISH <-as.integer(tw062table$FN127$FISH)} 
    if("F7" %in% names(tw062table$FN127)) {tw062table$FN127$F7 <-as.factor(tw062table$FN127$F7)} 
    if("AGEID" %in% names(tw062table$FN127)) {tw062table$FN127$AGEID <-as.integer(tw062table$FN127$AGEID)} 
    #if("AGEMT" %in% names(tw062table$FN127)) {tw062table$FN127$AGEMT <-as.integer(tw062table$FN127$AGEMT)} 
    #if("AGE" %in% names(tw062table$FN127)) {tw062table$FN127$AGE <-as.integer(tw062table$FN127$AGE)} 
    if("NCA" %in% names(tw062table$FN127)) {tw062table$FN127$NCA <-as.integer(tw062table$FN127$NCA)} 
    #if("XCSA" %in% names(tw062table$FN127)) {tw062table$FN127$XCSA <-as.integer(tw062table$FN127$XCSA)} 
    #if("EDGE" %in% names(tw062table$FN127)) {tw062table$FN127$EDGE <-as.integer(tw062table$FN127$EDGE)} 
    #if("XEDGE" %in% names(tw062table$FN127)) {tw062table$FN127$XEDGE <-as.integer(tw062table$FN127$XEDGE)} 
    if("CONF" %in% names(tw062table$FN127)) {tw062table$FN127$CONF <-as.integer(tw062table$FN127$CONF)} 
    if("AGEA" %in% names(tw062table$FN127)) {tw062table$FN127$AGEA <-as.integer(tw062table$FN127$AGEA)} 
    #if("AGEVAL" %in% names(tw062table$FN127)) {tw062table$FN127$AGEVAL <-as.integer(tw062table$FN127$AGEVAL)} 
    #if("COMMENT7" %in% names(tw062table$FN127)) {tw062table$FN127$COMMENT7 <-as.integer(tw062table$FN127$COMMENT7)} 
    if("PRJ_CD" %in% names(tw062table$FN127)) {tw062table$FN127$PRJ_CD <-as.factor(tw062table$FN127$PRJ_CD)} 
    if("V0" %in% names(tw062table$FN127)) {tw062table$FN127$V0 <-as.integer(tw062table$FN127$V0)} 
    if("ENTRY" %in% names(tw062table$FN127)) {tw062table$FN127$ENTRY <-as.integer(tw062table$FN127$ENTRY)} 
  }
  
  tw062table
  
  # end function
}