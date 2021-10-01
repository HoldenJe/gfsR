#' Assign NS variable types
#' @description Changes the default 'chr' class of each variable to the desired class type (e.g. 'int') for NS data.  Generally this function will not be called directly as it is called from within 'import.fn.data'
#' @param ns1table is a list containing each FN table as an item in the list
#' @return returns a list that contains each FN table as an item in the list
#' @export

ns1.vartypes<-function(ns1table){
  # FN121
  mytables<-names(ns1table)
  if('FN121' %in% mytables){
    if("F1" %in% names(ns1table$FN121)) {ns1table$FN121$F1 <-as.factor(ns1table$FN121$F1)}
    if('SAM' %in% names(ns1table$FN121)) {ns1table$FN121$SAM<-as.integer(ns1table$FN121$SAM)}
    if('XLEAD' %in% names(ns1table$FN121)) {ns1table$FN121$XLEAD<-as.factor(ns1table$FN121$XLEAD)}
    if('GRID' %in% names(ns1table$FN121)) {ns1table$FN121$GRID<-as.factor(ns1table$FN121$GRID)}
    if('SILOC' %in% names(ns1table$FN121)) {ns1table$FN121$SILOC<-as.character(ns1table$FN121$SILOC)}
    if('SITP' %in% names(ns1table$FN121)) {ns1table$FN121$SITP<-as.factor(ns1table$FN121$SITP)}
    if('BOTTOM' %in% names(ns1table$FN121)) {ns1table$FN121$BOTTOM<-as.factor(ns1table$FN121$BOTTOM)}
    if('COVER' %in% names(ns1table$FN121)) {ns1table$FN121$COVER<-as.factor(ns1table$FN121$COVER)}
    #if('EFFDT0' %in% names(ns1table$FN121)) {ns1table$FN121$EFFDT0<-as.date(ns1table$FN121$EFFDT0)} ### Already Date format
    if('EFFTM0' %in% names(ns1table$FN121)) {ns1table$FN121$EFFTM0<-as.character(ns1table$FN121$EFFTM0)} ### chr in gl1
    if('XLEADUSE' %in% names(ns1table$FN121)) {ns1table$FN121$XLEADUSE<-as.factor(ns1table$FN121$XLEADUSE)}
    if('XDISTOFF' %in% names(ns1table$FN121)) {ns1table$FN121$XDISTOFF<-as.factor(ns1table$FN121$XDISTOFF)}
    if('XANGLE' %in% names(ns1table$FN121)) {ns1table$FN121$XANGLE<-as.factor(ns1table$FN121$XANGLE)}
    if('GRDEPMIN' %in% names(ns1table$FN121)) {ns1table$FN121$GRDEPMIN<-as.numeric(ns1table$FN121$GRDEPMIN)}
    if('GRDEPMAX' %in% names(ns1table$FN121)) {ns1table$FN121$GRDEPMAX<-as.numeric(ns1table$FN121$GRDEPMAX)}
    if('XGRDEPMID' %in% names(ns1table$FN121)) {ns1table$FN121$XGRDEPMID<-as.numeric(ns1table$FN121$XGRDEPMID)}
    #if('EFFDT1' %in% names(ns1table$FN121)) {ns1table$FN121$EFFDT1<-as.date(ns1table$FN121$EFFDT1)} ### Already Date format
    if('EFFTM1' %in% names(ns1table$FN121)) {ns1table$FN121$EFFTM1<-as.character(ns1table$FN121$EFFTM1)}
    if('EFFST' %in% names(ns1table$FN121)) {ns1table$FN121$EFFST<-as.factor(ns1table$FN121$EFFST)}
    if('AIRTEM1' %in% names(ns1table$FN121)) {ns1table$FN121$AIRTEM1<-as.numeric(ns1table$FN121$AIRTEM1)}
    if('SITEM1' %in% names(ns1table$FN121)) {ns1table$FN121$SITEM1<-as.numeric(ns1table$FN121$SITEM1)}
    if('XWAVEHT' %in% names(ns1table$FN121)) {ns1table$FN121$XWAVEHT<-as.numeric(ns1table$FN121$XWAVEHT)}
    if('CLOUD' %in% names(ns1table$FN121)) {ns1table$FN121$CLOUD<-as.factor(ns1table$FN121$CLOUD)}
    if('PRECIP' %in% names(ns1table$FN121)) {ns1table$FN121$PRECIP<-as.factor(ns1table$FN121$PRECIP)}
    #if('WIND' %in% names(ns1table$FN121)) {ns1table$FN121$WIND<-as.character(ns1table$FN121$WIND)}
    if('XWEATHER' %in% names(ns1table$FN121)) {ns1table$FN121$XWEATHER<-as.factor(ns1table$FN121$XWEATHER)}
    #if('COMMENT1' %in% names(ns1table$FN121)) {ns1table$FN121$COMMENT1<-as.character(ns1table$FN121$COMMENT1)} ### Should stay as chr?
    if('EFFDUR' %in% names(ns1table$FN121)) {ns1table$FN121$EFFDUR<-as.numeric(ns1table$FN121$EFFDUR)}
    if('STRATUM' %in% names(ns1table$FN121)) {ns1table$FN121$STRATUM<-as.factor(ns1table$FN121$STRATUM)}
  }
  #FN123
  if ('FN123' %in% mytables){
    if("SAM" %in% names(ns1table$FN123)) {ns1table$FN123$SAM <-as.integer(ns1table$FN123$SAM)} 
    if("EFF" %in% names(ns1table$FN123)) {ns1table$FN123$EFF <-as.factor(ns1table$FN123$EFF)} 
    if("F3" %in% names(ns1table$FN123)) {ns1table$FN123$F3 <-as.factor(ns1table$FN123$F3)} 
    if("SPC" %in% names(ns1table$FN123)) {ns1table$FN123$SPC <-as.factor(ns1table$FN123$SPC)} 
    if("CATCNT" %in% names(ns1table$FN123)) {ns1table$FN123$CATCNT <-as.integer(ns1table$FN123$CATCNT)} 
    if("SUBCNT" %in% names(ns1table$FN123)) {ns1table$FN123$SUBCNT <-as.integer(ns1table$FN123$SUBCNT)} 
    if("SUBWT" %in% names(ns1table$FN123)) {ns1table$FN123$SUBWT <-as.numeric(ns1table$FN123$SUBWT)} 
    if("BIOCNT" %in% names(ns1table$FN123)) {ns1table$FN123$BIOCNT <-as.integer(ns1table$FN123$BIOCNT)} 
    #if("XFRSH_DT" %in% names(ns1table$FN123)) {ns1table$FN123$XFRSH_DT <-as.integer(ns1table$FN123$XFRSH_DT)} - keep as date
    #if("XFRZN_DT" %in% names(ns1table$FN123)) {ns1table$FN123$XFRZN_DT <-as.integer(ns1table$FN123$XFRZN_DT)} - keep as date
    if("XSPECNT" %in% names(ns1table$FN123)) {ns1table$FN123$XSPECNT <-as.integer(ns1table$FN123$XSPECNT)} 
    if("PRJ_CD" %in% names(ns1table$FN123)) {ns1table$FN123$PRJ_CD <-as.factor(ns1table$FN123$PRJ_CD)} 
    #if("V0" %in% names(ns1table$FN123)) {ns1table$FN123$V0 <-as.integer(ns1table$FN123$V0)}
    if("ENTRY" %in% names(ns1table$FN123)) {ns1table$FN123$ENTRY <-as.integer(ns1table$FN123$ENTRY)} 
    if("CATWT" %in% names(ns1table$FN123)) {ns1table$FN123$CATWT <-as.numeric(ns1table$FN123$CATWT)} 
  }
  # FN124
  if('FN124' %in% mytables){
    if("SAM" %in% names(ns1table$FN124)) {ns1table$FN124$SAM <-as.integer(ns1table$FN124$SAM)} 
    if("EFF" %in% names(ns1table$FN124)) {ns1table$FN124$EFF <-as.factor(ns1table$FN124$EFF)} 
    if("SPC" %in% names(ns1table$FN124)) {ns1table$FN124$SPC <-as.factor(ns1table$FN124$SPC)} 
    if("F4" %in% names(ns1table$FN124)) {ns1table$FN124$F4 <-as.factor(ns1table$FN124$F4)} 
    if("SIZ" %in% names(ns1table$FN124)) {ns1table$FN124$SIZ <-as.integer(ns1table$FN124$SIZ)} 
    if("SIZCNT" %in% names(ns1table$FN124)) {ns1table$FN124$SIZCNT <-as.integer(ns1table$FN124$SIZCNT)} 
    if("PRJ_CD" %in% names(ns1table$FN124)) {ns1table$FN124$PRJ_CD <-as.factor(ns1table$FN124$PRJ_CD)} 
    #if("V0" %in% names(ns1table$FN124)) {ns1table$FN124$V0 <-as.integer(ns1table$FN124$V0)} 
    if("ENTRY" %in% names(ns1table$FN124)) {ns1table$FN124$ENTRY <-as.integer(ns1table$FN124$ENTRY)} 
  }
  
  #FN125
  if ('FN125' %in% mytables){
    if("SAM" %in% names(ns1table$FN125)) {ns1table$FN125$SAM <-as.integer(ns1table$FN125$SAM)} 
    if("EFF" %in% names(ns1table$FN125)) {ns1table$FN125$EFF <-as.factor(ns1table$FN125$EFF)} 
    if("SPC" %in% names(ns1table$FN125)) {ns1table$FN125$SPC <-as.factor(ns1table$FN125$SPC)} 
    if("F5" %in% names(ns1table$FN125)) {ns1table$FN125$F5 <-as.factor(ns1table$FN125$F5)} 
    if("FISH" %in% names(ns1table$FN125)) {ns1table$FN125$FISH <-as.integer(ns1table$FN125$FISH)} 
    if("FLEN" %in% names(ns1table$FN125)) {ns1table$FN125$FLEN <-as.integer(ns1table$FN125$FLEN)} 
    if("TLEN" %in% names(ns1table$FN125)) {ns1table$FN125$TLEN <-as.integer(ns1table$FN125$TLEN)} 
    if("RWT" %in% names(ns1table$FN125)) {ns1table$FN125$RWT <-as.numeric(ns1table$FN125$RWT)} 
    if("XEVIS_WT" %in% names(ns1table$FN125)) {ns1table$FN125$XEVIS_WT <-as.integer(ns1table$FN125$XEVIS_WT)} 
    if("SEX" %in% names(ns1table$FN125)) {ns1table$FN125$SEX <-as.factor(ns1table$FN125$SEX)} 
    if("GON" %in% names(ns1table$FN125)) {ns1table$FN125$GON <-as.factor(ns1table$FN125$GON)} 
    if("GONWT" %in% names(ns1table$FN125)) {ns1table$FN125$GONWT <-as.integer(ns1table$FN125$GONWT)} 
    if("CLIPC" %in% names(ns1table$FN125)) {ns1table$FN125$CLIPC <-as.factor(ns1table$FN125$CLIPC)} 
    #if("NODC" %in% names(ns1table$FN125)) {ns1table$FN125$NODC <-as.integer(ns1table$FN125$NODC)} 
    if("TAGSTAT" %in% names(ns1table$FN125)) {ns1table$FN125$TAGSTAT <-as.factor(ns1table$FN125$TAGSTAT)} 
    #if("TAGID" %in% names(ns1table$FN125)) {ns1table$FN125$TAGID <-as.integer(ns1table$FN125$TAGID)} 
    #if("TAGDOC" %in% names(ns1table$FN125)) {ns1table$FN125$TAGDOC <-as.integer(ns1table$FN125$TAGDOC)} 
    #if("LAMIJC" %in% names(ns1table$FN125)) {ns1table$FN125$LAMIJC <-as.integer(ns1table$FN125$LAMIJC)} 
    #if("AGEST" %in% names(ns1table$FN125)) {ns1table$FN125$AGEST <-as.integer(ns1table$FN125$AGEST)} 
    #if("TISSUE" %in% names(ns1table$FN125)) {ns1table$FN125$TISSUE <-as.integer(ns1table$FN125$TISSUE)} 
    #if("XHEAD_BOX" %in% names(ns1table$FN125)) {ns1table$FN125$XHEAD_BOX <-as.integer(ns1table$FN125$XHEAD_BOX)} 
    if("XSTOM_WT" %in% names(ns1table$FN125)) {ns1table$FN125$XSTOM_WT <-as.integer(ns1table$FN125$XSTOM_WT)} 
    #if("COMMENT5" %in% names(ns1table$FN125)) {ns1table$FN125$COMMENT5 <-as.integer(ns1table$FN125$COMMENT5)} 
    if("PRJ_CD" %in% names(ns1table$FN125)) {ns1table$FN125$PRJ_CD <-as.factor(ns1table$FN125$PRJ_CD)} 
    #if("V0" %in% names(ns1table$FN125)) {ns1table$FN125$V0 <-as.integer(ns1table$FN125$V0)} 
    if("ENTRY" %in% names(ns1table$FN125)) {ns1table$FN125$ENTRY <-as.integer(ns1table$FN125$ENTRY)} 
    #if("XHETERO" %in% names(ns1table$FN125)) {ns1table$FN125$XHETERO <-as.integer(ns1table$FN125$XHETERO)} 
    #if("XCONTAM" %in% names(ns1table$FN125)) {ns1table$FN125$XCONTAM <-as.integer(ns1table$FN125$XCONTAM)} 
    #if("XVFI" %in% names(ns1table$FN125)) {ns1table$FN125$XVFI <-as.integer(ns1table$FN125$XVFI)} 
    if("AGE" %in% names(ns1table$FN125)) {ns1table$FN125$AGE <-as.integer(ns1table$FN125$AGE)} 
    #if("XISOTOPE" %in% names(ns1table$FN125)) {ns1table$FN125$XISOTOPE <-as.integer(ns1table$FN125$XISOTOPE)} 
    #if("XSTOM" %in% names(ns1table$FN125)) {ns1table$FN125$XSTOM <-as.integer(ns1table$FN125$XSTOM)}
    #if("SEL" %in% names(ns1table$FN125)) {ns1table$FN125$SEL <-as.factor(ns1table$FN125$SEL)} 
    }
  
  # FN126
  if ('FN126' %in% (mytables)){
    if("SAM" %in% names(ns1table$FN126)) {ns1table$FN126$SAM <-as.integer(ns1table$FN126$SAM)} 
    if("EFF" %in% names(ns1table$FN126)) {ns1table$FN126$EFF <-as.factor(ns1table$FN126$EFF)} 
    if("SPC" %in% names(ns1table$FN126)) {ns1table$FN126$SPC <-as.factor(ns1table$FN126$SPC)} 
    if("FISH" %in% names(ns1table$FN126)) {ns1table$FN126$FISH <-as.integer(ns1table$FN126$FISH)} 
    if("F6" %in% names(ns1table$FN126)) {ns1table$FN126$F6 <-as.factor(ns1table$FN126$F6)} 
    if("FOOD" %in% names(ns1table$FN126)) {ns1table$FN126$FOOD <-as.integer(ns1table$FN126$FOOD)} 
    if("TAXON" %in% names(ns1table$FN126)) {ns1table$FN126$TAXON <-as.factor(ns1table$FN126$TAXON)} 
    if("LF" %in% names(ns1table$FN126)) {ns1table$FN126$LF <-as.integer(ns1table$FN126$LF)} 
    if("FDCNT" %in% names(ns1table$FN126)) {ns1table$FN126$FDCNT <-as.integer(ns1table$FN126$FDCNT)} 
    #if("FDMES" %in% names(ns1table$FN126)) {ns1table$FN126$FDMES <-as.integer(ns1table$FN126$FDMES)} 
    #if("FDVAL" %in% names(ns1table$FN126)) {ns1table$FN126$FDVAL <-as.integer(ns1table$FN126$FDVAL)} 
    if("PRJ_CD" %in% names(ns1table$FN126)) {ns1table$FN126$PRJ_CD <-as.factor(ns1table$FN126$PRJ_CD)} 
    if("V0" %in% names(ns1table$FN126)) {ns1table$FN126$V0 <-as.integer(ns1table$FN126$V0)} 
    if("ENTRY" %in% names(ns1table$FN126)) {ns1table$FN126$ENTRY <-as.integer(ns1table$FN126$ENTRY)} 
  }
  
  # FN127
  if ('FN127' %in% (mytables)) {
    if("SAM" %in% names(ns1table$FN127)) {ns1table$FN127$SAM <-as.integer(ns1table$FN127$SAM)} 
    if("EFF" %in% names(ns1table$FN127)) {ns1table$FN127$EFF <-as.factor(ns1table$FN127$EFF)} 
    if("SPC" %in% names(ns1table$FN127)) {ns1table$FN127$SPC <-as.factor(ns1table$FN127$SPC)} 
    if("FISH" %in% names(ns1table$FN127)) {ns1table$FN127$FISH <-as.integer(ns1table$FN127$FISH)} 
    #if("F7" %in% names(ns1table$FN127)) {ns1table$FN127$F7 <-as.factor(ns1table$FN127$F7)} 
    if("AGEID" %in% names(ns1table$FN127)) {ns1table$FN127$AGEID <-as.integer(ns1table$FN127$AGEID)} 
    #if("AGEMT" %in% names(ns1table$FN127)) {ns1table$FN127$AGEMT <-as.integer(ns1table$FN127$AGEMT)} 
    #if("AGE" %in% names(ns1table$FN127)) {ns1table$FN127$AGE <-as.integer(ns1table$FN127$AGE)} 
    if("NCA" %in% names(ns1table$FN127)) {ns1table$FN127$NCA <-as.integer(ns1table$FN127$NCA)} 
    #if("XCSA" %in% names(ns1table$FN127)) {ns1table$FN127$XCSA <-as.integer(ns1table$FN127$XCSA)} 
    #if("EDGE" %in% names(ns1table$FN127)) {ns1table$FN127$EDGE <-as.integer(ns1table$FN127$EDGE)} 
    #if("XEDGE" %in% names(ns1table$FN127)) {ns1table$FN127$XEDGE <-as.integer(ns1table$FN127$XEDGE)} 
    if("CONF" %in% names(ns1table$FN127)) {ns1table$FN127$CONF <-as.integer(ns1table$FN127$CONF)} 
    if("AGEA" %in% names(ns1table$FN127)) {ns1table$FN127$AGEA <-as.integer(ns1table$FN127$AGEA)} 
    #if("AGEVAL" %in% names(ns1table$FN127)) {ns1table$FN127$AGEVAL <-as.integer(ns1table$FN127$AGEVAL)} 
    #if("COMMENT7" %in% names(ns1table$FN127)) {ns1table$FN127$COMMENT7 <-as.integer(ns1table$FN127$COMMENT7)} 
    if("PRJ_CD" %in% names(ns1table$FN127)) {ns1table$FN127$PRJ_CD <-as.factor(ns1table$FN127$PRJ_CD)} 
    if("V0" %in% names(ns1table$FN127)) {ns1table$FN127$V0 <-as.integer(ns1table$FN127$V0)} 
    if("ENTRY" %in% names(ns1table$FN127)) {ns1table$FN127$ENTRY <-as.integer(ns1table$FN127$ENTRY)} 
  }
  
  ns1table
  
  # end function
}