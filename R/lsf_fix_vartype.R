#' Assign LSF variable types
#' @description Changes the default 'chr' class of each variable to the desired class type (e.g. 'int') for LSF data.  Generally this function will not be called directly as it is called from with 'import.fn.data'
#' @param lsftable is a list containing each FN table as an item in the list
#' @return returns a list that contains each FN2 table as an item in the list
#' @export

lsf.vartypes<-function(lsftable){
  # FN121
  mytables<-names(lsftable)
  if('FN121' %in% mytables){
    if("F1" %in% names(lsftable$FN121)) {lsftable$FN121$F1 <-as.factor(lsftable$FN121$F1)} 
    if('SAM' %in% names(lsftable$FN121)) {lsftable$FN121$SAM<-as.integer(lsftable$FN121$SAM)}
    if('AREA' %in% names(lsftable$FN121)) {lsftable$FN121$AREA<-as.factor(lsftable$FN121$AREA)}
    if('SITE' %in% names(lsftable$FN121)) {lsftable$FN121$SITE<-as.factor(lsftable$FN121$SITE)}
    if('XSILOC1' %in% names(lsftable$FN121)) {lsftable$FN121$XSILOC1<-as.numeric(lsftable$FN121$XSILOC1)}
    if('SILOC' %in% names(lsftable$FN121)) {lsftable$FN121$SILOC<-as.numeric(lsftable$FN121$SILOC)}
    if('XSILOC2' %in% names(lsftable$FN121)) {lsftable$FN121$XSILOC2<-as.numeric(lsftable$FN121$XSILOC2)}
    if('EFFDUR' %in% names(lsftable$FN121)) {lsftable$FN121$EFFDUR<-as.numeric(lsftable$FN121$EFFDUR)}
    if('GR' %in% names(lsftable$FN121)) {lsftable$FN121$GR<-as.factor(lsftable$FN121$GR)}
    if('SIDEP' %in% names(lsftable$FN121)) {lsftable$FN121$SIDEP<-as.numeric(lsftable$FN121$SIDEP)}
    if('GRDEP' %in% names(lsftable$FN121)) {lsftable$FN121$GRDEP<-as.numeric(lsftable$FN121$GRDEP)}
    if('GRDEPMIN' %in% names(lsftable$FN121)) {lsftable$FN121$GRDEPMIN<-as.numeric(lsftable$FN121$GRDEPMIN)}
    if('GRDEPMAX' %in% names(lsftable$FN121)) {lsftable$FN121$GRDEPMAX<-as.numeric(lsftable$FN121$GRDEPMAX)}
    if('SITEM' %in% names(lsftable$FN121)) {lsftable$FN121$SITEM<-as.numeric(lsftable$FN121$SITEM)}
    if('GRTEM' %in% names(lsftable$FN121)) {lsftable$FN121$GRTEM<-as.numeric(lsftable$FN121$GRTEM)}
    if('STRATUM' %in% names(lsftable$FN121)) {lsftable$FN121$STRATUM<-as.factor(lsftable$FN121$STRATUM)}
    if('PRJ_CD' %in% names(lsftable$FN121)) {lsftable$FN121$PRJ_CD<-as.factor(lsftable$FN121$PRJ_CD)}
    if('XFOULING' %in% names(lsftable$FN121)) {lsftable$FN121$XFOULING<-as.factor(lsftable$FN121$XFOULING)}
    if("ENTRY" %in% names(lsftable$FN121)) {lsftable$FN121$ENTRY <-as.integer(lsftable$FN121$ENTRY)}
    if("XSECCHI" %in% names(lsftable$FN121)) {lsftable$FN121$XSECCHI <-as.integer(lsftable$FN121$XSECCHI)}
  }
  
  #FN123
  if ('FN123' %in% mytables){
    if("SAM" %in% names(lsftable$FN123)) {lsftable$FN123$SAM <-as.integer(lsftable$FN123$SAM)} 
    if("EFF" %in% names(lsftable$FN123)) {lsftable$FN123$EFF <-as.factor(lsftable$FN123$EFF)} 
    if("F3" %in% names(lsftable$FN123)) {lsftable$FN123$F3 <-as.factor(lsftable$FN123$F3)} 
    if("SPC" %in% names(lsftable$FN123)) {lsftable$FN123$SPC <-as.factor(lsftable$FN123$SPC)} 
    if("CATCNT" %in% names(lsftable$FN123)) {lsftable$FN123$CATCNT <-as.integer(lsftable$FN123$CATCNT)} 
    if("SUBCNT" %in% names(lsftable$FN123)) {lsftable$FN123$SUBCNT <-as.integer(lsftable$FN123$SUBCNT)} 
    if("SUBWT" %in% names(lsftable$FN123)) {lsftable$FN123$SUBWT <-as.numeric(lsftable$FN123$SUBWT)} 
    if("BIOCNT" %in% names(lsftable$FN123)) {lsftable$FN123$BIOCNT <-as.integer(lsftable$FN123$BIOCNT)} 
    #if("XFRSH_DT" %in% names(lsftable$FN123)) {lsftable$FN123$XFRSH_DT <-as.integer(lsftable$FN123$XFRSH_DT)} - keep as date
    #if("XFRZN_DT" %in% names(lsftable$FN123)) {lsftable$FN123$XFRZN_DT <-as.integer(lsftable$FN123$XFRZN_DT)} - keep as date
    if("XSPECNT" %in% names(lsftable$FN123)) {lsftable$FN123$XSPECNT <-as.integer(lsftable$FN123$XSPECNT)} 
    if("PRJ_CD" %in% names(lsftable$FN123)) {lsftable$FN123$PRJ_CD <-as.factor(lsftable$FN123$PRJ_CD)} 
    #if("V0" %in% names(lsftable$FN123)) {lsftable$FN123$V0 <-as.integer(lsftable$FN123$V0)}
    if("ENTRY" %in% names(lsftable$FN123)) {lsftable$FN123$ENTRY <-as.integer(lsftable$FN123$ENTRY)} 
    if("CATWT" %in% names(lsftable$FN123)) {lsftable$FN123$CATWT <-as.numeric(lsftable$FN123$CATWT)} 
  }
  # FN124
  if('FN124' %in% mytables){
    if("SAM" %in% names(lsftable$FN124)) {lsftable$FN124$SAM <-as.integer(lsftable$FN124$SAM)} 
    if("EFF" %in% names(lsftable$FN124)) {lsftable$FN124$EFF <-as.factor(lsftable$FN124$EFF)} 
    if("SPC" %in% names(lsftable$FN124)) {lsftable$FN124$SPC <-as.factor(lsftable$FN124$SPC)} 
    if("F4" %in% names(lsftable$FN124)) {lsftable$FN124$F4 <-as.factor(lsftable$FN124$F4)} 
    if("SIZ" %in% names(lsftable$FN124)) {lsftable$FN124$SIZ <-as.integer(lsftable$FN124$SIZ)} 
    if("SIZCNT" %in% names(lsftable$FN124)) {lsftable$FN124$SIZCNT <-as.integer(lsftable$FN124$SIZCNT)} 
    if("PRJ_CD" %in% names(lsftable$FN124)) {lsftable$FN124$PRJ_CD <-as.factor(lsftable$FN124$PRJ_CD)} 
    #if("V0" %in% names(lsftable$FN124)) {lsftable$FN124$V0 <-as.integer(lsftable$FN124$V0)} 
    if("ENTRY" %in% names(lsftable$FN124)) {lsftable$FN124$ENTRY <-as.integer(lsftable$FN124$ENTRY)} 
  }
  
  #FN125
  if ('FN125' %in% mytables){
    if("SAM" %in% names(lsftable$FN125)) {lsftable$FN125$SAM <-as.integer(lsftable$FN125$SAM)} 
    if("EFF" %in% names(lsftable$FN125)) {lsftable$FN125$EFF <-as.factor(lsftable$FN125$EFF)} 
    if("SPC" %in% names(lsftable$FN125)) {lsftable$FN125$SPC <-as.factor(lsftable$FN125$SPC)} 
    if("F5" %in% names(lsftable$FN125)) {lsftable$FN125$F5 <-as.factor(lsftable$FN125$F5)} 
    if("FISH" %in% names(lsftable$FN125)) {lsftable$FN125$FISH <-as.integer(lsftable$FN125$FISH)} 
    if("FLEN" %in% names(lsftable$FN125)) {lsftable$FN125$FLEN <-as.integer(lsftable$FN125$FLEN)} 
    if("TLEN" %in% names(lsftable$FN125)) {lsftable$FN125$TLEN <-as.integer(lsftable$FN125$TLEN)} 
    if("RWT" %in% names(lsftable$FN125)) {lsftable$FN125$RWT <-as.integer(lsftable$FN125$RWT)} 
    if("XEVIS_WT" %in% names(lsftable$FN125)) {lsftable$FN125$XEVIS_WT <-as.integer(lsftable$FN125$XEVIS_WT)} 
    if("SEX" %in% names(lsftable$FN125)) {lsftable$FN125$SEX <-as.factor(lsftable$FN125$SEX)} 
    if("GON" %in% names(lsftable$FN125)) {lsftable$FN125$GON <-as.factor(lsftable$FN125$GON)} 
    if("GONWT" %in% names(lsftable$FN125)) {lsftable$FN125$GONWT <-as.integer(lsftable$FN125$GONWT)} 
    if("CLIPC" %in% names(lsftable$FN125)) {lsftable$FN125$CLIPC <-as.factor(lsftable$FN125$CLIPC)} 
    #if("NODC" %in% names(lsftable$FN125)) {lsftable$FN125$NODC <-as.integer(lsftable$FN125$NODC)} 
    if("TAGSTAT" %in% names(lsftable$FN125)) {lsftable$FN125$TAGSTAT <-as.factor(lsftable$FN125$TAGSTAT)} 
    #if("TAGID" %in% names(lsftable$FN125)) {lsftable$FN125$TAGID <-as.integer(lsftable$FN125$TAGID)} 
    #if("TAGDOC" %in% names(lsftable$FN125)) {lsftable$FN125$TAGDOC <-as.integer(lsftable$FN125$TAGDOC)} 
    #if("LAMIJC" %in% names(lsftable$FN125)) {lsftable$FN125$LAMIJC <-as.integer(lsftable$FN125$LAMIJC)} 
    #if("AGEST" %in% names(lsftable$FN125)) {lsftable$FN125$AGEST <-as.integer(lsftable$FN125$AGEST)} 
    #if("TISSUE" %in% names(lsftable$FN125)) {lsftable$FN125$TISSUE <-as.integer(lsftable$FN125$TISSUE)} 
    #if("XHEAD_BOX" %in% names(lsftable$FN125)) {lsftable$FN125$XHEAD_BOX <-as.integer(lsftable$FN125$XHEAD_BOX)} 
    if("XSTOM_WT" %in% names(lsftable$FN125)) {lsftable$FN125$XSTOM_WT <-as.integer(lsftable$FN125$XSTOM_WT)} 
    #if("COMMENT5" %in% names(lsftable$FN125)) {lsftable$FN125$COMMENT5 <-as.integer(lsftable$FN125$COMMENT5)} 
    if("PRJ_CD" %in% names(lsftable$FN125)) {lsftable$FN125$PRJ_CD <-as.factor(lsftable$FN125$PRJ_CD)} 
    #if("V0" %in% names(lsftable$FN125)) {lsftable$FN125$V0 <-as.integer(lsftable$FN125$V0)} 
    if("ENTRY" %in% names(lsftable$FN125)) {lsftable$FN125$ENTRY <-as.integer(lsftable$FN125$ENTRY)} 
    #if("XHETERO" %in% names(lsftable$FN125)) {lsftable$FN125$XHETERO <-as.integer(lsftable$FN125$XHETERO)} 
    #if("XCONTAM" %in% names(lsftable$FN125)) {lsftable$FN125$XCONTAM <-as.integer(lsftable$FN125$XCONTAM)} 
    #if("XVFI" %in% names(lsftable$FN125)) {lsftable$FN125$XVFI <-as.integer(lsftable$FN125$XVFI)} 
    if("AGE" %in% names(lsftable$FN125)) {lsftable$FN125$AGE <-as.integer(lsftable$FN125$AGE)} 
    #if("XISOTOPE" %in% names(lsftable$FN125)) {lsftable$FN125$XISOTOPE <-as.integer(lsftable$FN125$XISOTOPE)} 
    #if("XSTOM" %in% names(lsftable$FN125)) {lsftable$FN125$XSTOM <-as.integer(lsftable$FN125$XSTOM)} 
  }
  
  # FN126
  if ('FN126' %in% (mytables)){
    if("SAM" %in% names(lsftable$FN126)) {lsftable$FN126$SAM <-as.integer(lsftable$FN126$SAM)} 
    if("EFF" %in% names(lsftable$FN126)) {lsftable$FN126$EFF <-as.factor(lsftable$FN126$EFF)} 
    if("SPC" %in% names(lsftable$FN126)) {lsftable$FN126$SPC <-as.factor(lsftable$FN126$SPC)} 
    if("FISH" %in% names(lsftable$FN126)) {lsftable$FN126$FISH <-as.integer(lsftable$FN126$FISH)} 
    if("F6" %in% names(lsftable$FN126)) {lsftable$FN126$F6 <-as.factor(lsftable$FN126$F6)} 
    if("FOOD" %in% names(lsftable$FN126)) {lsftable$FN126$FOOD <-as.integer(lsftable$FN126$FOOD)} 
    if("TAXON" %in% names(lsftable$FN126)) {lsftable$FN126$TAXON <-as.factor(lsftable$FN126$TAXON)} 
    if("LF" %in% names(lsftable$FN126)) {lsftable$FN126$LF <-as.integer(lsftable$FN126$LF)} 
    if("FDCNT" %in% names(lsftable$FN126)) {lsftable$FN126$FDCNT <-as.integer(lsftable$FN126$FDCNT)} 
    #if("FDMES" %in% names(lsftable$FN126)) {lsftable$FN126$FDMES <-as.integer(lsftable$FN126$FDMES)} 
    #if("FDVAL" %in% names(lsftable$FN126)) {lsftable$FN126$FDVAL <-as.integer(lsftable$FN126$FDVAL)} 
    if("PRJ_CD" %in% names(lsftable$FN126)) {lsftable$FN126$PRJ_CD <-as.factor(lsftable$FN126$PRJ_CD)} 
    if("V0" %in% names(lsftable$FN126)) {lsftable$FN126$V0 <-as.integer(lsftable$FN126$V0)} 
    if("ENTRY" %in% names(lsftable$FN126)) {lsftable$FN126$ENTRY <-as.integer(lsftable$FN126$ENTRY)} 
  }
  
  # FN127
  if ('FN127' %in% (mytables)) {
    if("SAM" %in% names(lsftable$FN127)) {lsftable$FN127$SAM <-as.integer(lsftable$FN127$SAM)} 
    if("EFF" %in% names(lsftable$FN127)) {lsftable$FN127$EFF <-as.factor(lsftable$FN127$EFF)} 
    if("SPC" %in% names(lsftable$FN127)) {lsftable$FN127$SPC <-as.factor(lsftable$FN127$SPC)} 
    if("FISH" %in% names(lsftable$FN127)) {lsftable$FN127$FISH <-as.integer(lsftable$FN127$FISH)} 
    if("F7" %in% names(lsftable$FN127)) {lsftable$FN127$F7 <-as.factor(lsftable$FN127$F7)} 
    if("AGEID" %in% names(lsftable$FN127)) {lsftable$FN127$AGEID <-as.integer(lsftable$FN127$AGEID)} 
    #if("AGEMT" %in% names(lsftable$FN127)) {lsftable$FN127$AGEMT <-as.integer(lsftable$FN127$AGEMT)} 
    #if("AGE" %in% names(lsftable$FN127)) {lsftable$FN127$AGE <-as.integer(lsftable$FN127$AGE)} 
    if("NCA" %in% names(lsftable$FN127)) {lsftable$FN127$NCA <-as.integer(lsftable$FN127$NCA)} 
    #if("XCSA" %in% names(lsftable$FN127)) {lsftable$FN127$XCSA <-as.integer(lsftable$FN127$XCSA)} 
    #if("EDGE" %in% names(lsftable$FN127)) {lsftable$FN127$EDGE <-as.integer(lsftable$FN127$EDGE)} 
    #if("XEDGE" %in% names(lsftable$FN127)) {lsftable$FN127$XEDGE <-as.integer(lsftable$FN127$XEDGE)} 
    if("CONF" %in% names(lsftable$FN127)) {lsftable$FN127$CONF <-as.integer(lsftable$FN127$CONF)} 
    if("AGEA" %in% names(lsftable$FN127)) {lsftable$FN127$AGEA <-as.integer(lsftable$FN127$AGEA)} 
    #if("AGEVAL" %in% names(lsftable$FN127)) {lsftable$FN127$AGEVAL <-as.integer(lsftable$FN127$AGEVAL)} 
    #if("COMMENT7" %in% names(lsftable$FN127)) {lsftable$FN127$COMMENT7 <-as.integer(lsftable$FN127$COMMENT7)} 
    if("PRJ_CD" %in% names(lsftable$FN127)) {lsftable$FN127$PRJ_CD <-as.factor(lsftable$FN127$PRJ_CD)} 
    if("V0" %in% names(lsftable$FN127)) {lsftable$FN127$V0 <-as.integer(lsftable$FN127$V0)} 
    if("ENTRY" %in% names(lsftable$FN127)) {lsftable$FN127$ENTRY <-as.integer(lsftable$FN127$ENTRY)} 
  }
  
  lsftable
  
  # end function
}