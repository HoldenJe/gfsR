# Using gfsR to import FN2 archive files

The primary function of `gfsR` is to be able to import FishNet2 (FN2)
`DATA.ZIP` files directly in to **R**. This is a short vignette to
demonstrate some of the functionality available by using the common
functions.

``` r

library(gfsR)
```

The most common usage is to import a single FN2 project. This is
accomplished by providing the file path directly to one of the import
functions. For this example we’ll use `import_fn_index_net` to import a
gill net project. All functions import FN2 tables as tables in a list
and thus will need to be assigned to a variable. This approach was taken
to allow the option for different projects to be imported to a local
environment at the same time and eventually combined as required.

``` r

tw2 <- import_fn_index_net("Data/TW2/IA15_TW2/DATA.ZIP")
```

The `gfsR` functions make use of messages from the `usethis` package to
provide *chatty* functions that provide indications of progress and note
any tables that are not available.

A quick way to examine the data is to use `lapply` and `head`.

``` r

lapply(tw2, head)
#> $FN011
#>     F1  PRJ_DATE0  PRJ_DATE1                                          PRJ_NM
#> 1 <NA> 2015-09-25 2015-10-08 2015 LAKE ONTARIO BENTHIC PREY FISH FALL SURVEY
#>         PRJ_LDR                       FOF_NM FOF_LOC WBY      WBY_NM ARU
#> 1 JEREMY HOLDER LAKE ONTARIO MANAGEMENT UNIT Glenora  2H Ontario, L.  00
#>   COMMENT0         PRJ_VER PRJ_HIS PRJ_RDO PRJ_SIZE       PRJ_CD  V0 ENTRY
#> 1        8 2.0J (96.06.11)      16      NA      770 LOA_IA15_TW2 +++     1
#> 
#> $FN012
#>     F2 SPC GRP         SPC_NM SPCMRK SIZSAM SIZATT SIZINT BIOSAM AGEDEC FDSAM
#> 1 <NA> 061  00        Alewife     00      2   FLEN      1      3     A0    00
#> 2 <NA> 081  00     Lake Trout     12      2   FLEN     10      1     X0    00
#> 3 <NA> 091  00 Lake Whitefish     00      2   FLEN     10      3     X0    00
#> 4 <NA> 121  00  Rainbow Smelt     00      2   FLEN      1      3     00    00
#> 5 <NA> 302  00     White Bass     00      2   FLEN     10      1     20    00
#> 6 <NA> 331  00   Yellow Perch     00      2   FLEN      5      1     20    00
#>   GRP_DES CATCNT_S SIZCNT_S SIZ_N FISH_N FISH_NREC FLEN_N TLEN_N AGE_N RWT_N
#> 1    <NA>       80      291   291    291       158    158    158     0   158
#> 2    <NA>        3        3     3      3         3      3      3     0     3
#> 3    <NA>        1        1     1      1         1      1      1     0     1
#> 4    <NA>       56       58    58     58        52     52     52     0    52
#> 5    <NA>        1        1     1      1         1      1      1     0     1
#> 6    <NA>        1        1     1      1         1      1      1     0     1
#>   MAT_N MAT_N1 MAT_N2 SEX_N SEX_N1 SEX_N2 CLIPC_N CLIPA_N TAGC_N TAGA_N
#> 1     0      0      0   106     27     47       0       0      0      0
#> 2     0      0      0     0      0      0       0       0      0      0
#> 3     0      0      0     1      0      0       0       0      0      0
#> 4     0      0      0    42     27      5       0       0      0      0
#> 5     0      0      0     0      0      0       0       0      0      0
#> 6     0      0      0     1      0      1       0       0      0      0
#>         PRJ_CD  V0 ENTRY
#> 1 LOA_IA15_TW2 +++     1
#> 2 LOA_IA15_TW2 +++     2
#> 3 LOA_IA15_TW2 +++    11
#> 4 LOA_IA15_TW2 +++     3
#> 5 LOA_IA15_TW2 +++     4
#> 6 LOA_IA15_TW2 +++     5
#> 
#> $FN013
#>     F3   GR EFFCNT EFFDST                                  GR_DES       PRJ_CD
#> 1 <NA> TW62      2      1 3/4 Yankee Standard No. 35 Bottom Trawl LOA_IA15_TW2
#>    V0 ENTRY
#> 1 +++     1
#> 
#> $FN014
#>     GR   F4 EFF MESH GRLEN GRHT GRWID GRCOL GRMAT GRYARN GRKNOT
#> 1 TW62 <NA> 000   13  <NA>  2.3   9.9     7     1      2      2
#>                                   EFF_DES       PRJ_CD  V0 ENTRY
#> 1 3/4 Yankee Standard No. 35 Bottom Trawl LOA_IA15_TW2 +++     1
#> 
#> $FN022
#>     F2 SSN  SSN_DATE0  SSN_DATE1 SSN_DES       PRJ_CD  V0 ENTRY
#> 1 <NA>  04 2015-09-25 2015-10-08    FALL LOA_IA15_TW2 +++     1
#> 
#> $FN026
#>     F6 SPACE AREA_LST SITP_LST SITE_LST SIDEP_GE SIDEP_LT GRDEP_GE GRDEP_LT
#> 1 <NA>    01       OS     <NA>     0140      135      145      135      145
#> 2 <NA>    02       OS     <NA>     0130      125      135      125      135
#> 3 <NA>    03       OS     <NA>     0120      115      125      115      125
#> 4 <NA>    04       OS     <NA>     0110      105      115      105      115
#> 5 <NA>    05       OS     <NA>     0100       95      105       95      105
#> 6 <NA>    06       OS     <NA>     0090       85       95       85       95
#>   GRID_GE GRID_LT SPACE_SIZ SPACE_WT SPACE_DES WBY      WBY_NM ARU       PRJ_CD
#> 1    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#> 2    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#> 3    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#> 4    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#> 5    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#> 6    <NA>    <NA>      <NA>     <NA>    OSHAWA  2H Ontario, L.  00 LOA_IA15_TW2
#>    V0 ENTRY
#> 1 +++     9
#> 2 +++     4
#> 3 +++     7
#> 4 +++     8
#> 5 +++     2
#> 6 +++     6
#> 
#> $FN028
#>     F8 MODE   GR GRUSE EFFTM0_GE EFFTM0_LT EFFDUR_GE EFFDUR_LT ORIENT
#> 1 <NA>   I2 TW62     0     06:00     17:00       0.2       0.3      3
#>                     MODE_DES       PRJ_CD  V0 ENTRY
#> 1 3/4 Yankee Standard No. 35 LOA_IA15_TW2 +++     1
#> 
#> $FN121
#>     F1 SAM AREA SITE      XSILOC1      XSILOC2       DATE EFFTM0 EFFTM1 EFFDUR
#> 1 <NA>   1   OS 0140 433727784471 433730784445 2015-09-25  13:59  14:04   0.08
#> 2 <NA>   2   OS 0130 433837784506 433840784481 2015-09-28  14:41  14:46   0.08
#> 3 <NA>   3   OS 0120 433976784504 433979784478 2015-09-28  15:22  15:27   0.08
#> 4 <NA>   4   OS 0110 434213784510 434212784483 2015-09-28  16:09  16:14   0.08
#> 5 <NA>   5   OS 0100 434328784478 434327784446 2015-09-28  16:52  16:57   0.08
#> 6 <NA>   6   OS 0090 434406784477 434409784451 2015-09-29  09:52  09:57   0.08
#>   EFFDST   GR XBOT_DUR SIDEP GRDEP GRDEPMIN GRDEPMAX SITRAN SITEM GRTEM XWARP
#> 1    340 TW62     2.43   140   140      140      140   <NA>  19.3   4.0   360
#> 2    340 TW62     1.85   130   130      130      130   <NA>  19.4   4.0   330
#> 3    340 TW62     2.13   120   120      120      120   <NA>  19.3   4.0   310
#> 4    340 TW62     2.03   110   110      110      110   <NA>  19.5   4.2   280
#> 5    340 TW62     5.65   100   100      100      100   <NA>  19.5   4.3   255
#> 6    340 TW62     3.55    90    90       90       90   <NA>  19.3   4.3   240
#>   XMUSL_WT XMUSL_N XDRES COMMENT1     STRATUM       PRJ_CD   V0 ENTRY  DD_LAT0
#> 1     <NA>    <NA> 26200     <NA> 04_.._01_I2 LOA_IA15_TW2 ++++     1 43.62117
#> 2     <NA>    <NA>  3500     <NA> 04_.._02_I2 LOA_IA15_TW2 ++++     2 43.63950
#> 3     <NA>    <NA>  5400     <NA> 04_.._03_I2 LOA_IA15_TW2 ++++     3 43.66267
#> 4     <NA>    <NA>  3600     <NA> 04_.._04_I2 LOA_IA15_TW2 ++++     4 43.70217
#> 5     <NA>    <NA>   400     <NA> 04_.._05_I2 LOA_IA15_TW2 ++++     5 43.72133
#> 6     <NA>    <NA> 19500     <NA> 04_.._06_I2 LOA_IA15_TW2 ++++     6 43.73433
#>     DD_LON0  DD_LAT1   DD_LON1
#> 1 -78.74517 43.62167 -78.74083
#> 2 -78.75100 43.64000 -78.74683
#> 3 -78.75067 43.66317 -78.74633
#> 4 -78.75167 43.70200 -78.74717
#> 5 -78.74633 43.72117 -78.74100
#> 6 -78.74617 43.73483 -78.74183
#> 
#> $FN122
#> data frame with 0 columns and 0 rows
#> 
#> $FN123
#>   SAM EFF   F3 SPC CATCNT   CATWT SUBCNT   SUBWT   XFRSH_DT XFRZN_DT BIOCNT
#> 1   1 000 <NA> 382      2   24.50     NA      NA 2015-09-29     <NA>      2
#> 2   1 000 <NA> 384     32 1163.00     32 1163.00 2015-09-30     <NA>     13
#> 3   2 000 <NA> 382      3   50.50      3   50.50 2015-09-30     <NA>      3
#> 4   2 000 <NA> 384     18  143.50     18  143.50 2015-09-30     <NA>     10
#> 5   3 000 <NA> 382     13   58.39     13   58.39 2015-10-01     <NA>      5
#> 6   3 000 <NA> 384     13   71.14     13   71.14 2015-10-01     <NA>      6
#>   XSPECNT       PRJ_CD  V0 ENTRY
#> 1      NA LOA_IA15_TW2 +++     1
#> 2      NA LOA_IA15_TW2 +++     2
#> 3      NA LOA_IA15_TW2 +++     4
#> 4      NA LOA_IA15_TW2 +++     3
#> 5      NA LOA_IA15_TW2 +++     6
#> 6      NA LOA_IA15_TW2 +++     5
#> 
#> $FN124
#>   SAM EFF SPC   F4 SIZ SIZCNT       PRJ_CD  V0 ENTRY
#> 1   1 000 384 <NA>  74      1 LOA_IA15_TW2 +++     1
#> 2   1 000 384 <NA>  88      1 LOA_IA15_TW2 +++     2
#> 3   1 000 384 <NA>  91      1 LOA_IA15_TW2 +++     3
#> 4   1 000 384 <NA>  93      1 LOA_IA15_TW2 +++     4
#> 5   1 000 384 <NA> 101      1 LOA_IA15_TW2 +++     5
#> 6   1 000 384 <NA> 104      1 LOA_IA15_TW2 +++     6
#> 
#> $FN125
#>   SAM EFF SPC   F5 FISH FLEN AGE TLEN   RWT SEX GON GONWT XEVIS_WT CLIPC NODC
#> 1   1 000 382 <NA>    1   NA  NA  118 21.50   1  30    NA       NA  <NA> <NA>
#> 2   1 000 382 <NA>    2   NA  NA   63  3.00   1  23    NA       NA  <NA> <NA>
#> 3   1 000 384 <NA>    3   NA  NA  134 27.60   2  40    NA       NA  <NA> <NA>
#> 4   1 000 384 <NA>    4   NA  NA  156 42.42   1  23    NA       NA  <NA> <NA>
#> 5   1 000 384 <NA>    5   NA  NA  164 55.59   2  30    NA       NA  <NA> <NA>
#> 6   1 000 384 <NA>    6   NA  NA  143 41.07   2  30    NA       NA  <NA> <NA>
#>   TAGSTAT TAGID TAGDOC AGEST XSTOM_WT XCONTAM LAMIJC TISSUE XHEAD_BOX XVFI
#> 1    <NA>  <NA>   <NA>  <NA>       NA    <NA>   <NA>   <NA>      <NA> <NA>
#> 2    <NA>  <NA>   <NA>  <NA>       NA    <NA>   <NA>   <NA>      <NA> <NA>
#> 3    <NA>  <NA>   <NA>     A       NA    <NA>   <NA>    158      <NA> <NA>
#> 4    <NA>  <NA>   <NA>     A       NA    <NA>   <NA>    158      <NA> <NA>
#> 5    <NA>  <NA>   <NA>     A       NA    <NA>   <NA>    158      <NA> <NA>
#> 6    <NA>  <NA>   <NA>     A       NA    <NA>   <NA>    158      <NA> <NA>
#>   XSTOM XISOTOPE XISOTOPE2 COMMENT5       PRJ_CD  V0 ENTRY
#> 1  <NA>     <NA>      <NA>     <NA> LOA_IA15_TW2 +++     1
#> 2  <NA>     <NA>      <NA>     <NA> LOA_IA15_TW2 +++     2
#> 3 37175    41374     41373     <NA> LOA_IA15_TW2 +++     3
#> 4 37176    41379     41363     <NA> LOA_IA15_TW2 +++     4
#> 5 37177    41378     41364     <NA> LOA_IA15_TW2 +++     5
#> 6 37178    41384     41365     <NA> LOA_IA15_TW2 +++     6
#> 
#> $FN126
#>  [1] SAM    EFF    SPC    FISH   F6     FOOD   TAXON  LF     FDCNT  FDMES 
#> [11] FDVAL  PRJ_CD V0     ENTRY 
#> <0 rows> (or 0-length row.names)
#> 
#> $FN127
#>  [1] SAM    EFF    SPC    FISH   F7     AGEID  AGEMT  NCA    XCSA   EDGE  
#> [11] CONF   XEDGE  AGEA   AGEVAL PRJ_CD V0     ENTRY 
#> <0 rows> (or 0-length row.names)
```

Frequently users will want to access the tables directly as dataframe in
the global environment rather than as a dataframe in a list. This is
quickly accomplished with the `list2env` function. **CAUTION**: using
the `list2env` will overwrite existing tables in the global environment.

``` r

ls() # shows current variables in global environment
#> [1] "tw2"
list2env(tw2, envir = .GlobalEnv) # to move list elements to global env.
#> <environment: R_GlobalEnv>
ls() # verify all tables exist in global env.
#>  [1] "FN011" "FN012" "FN013" "FN014" "FN022" "FN026" "FN028" "FN121" "FN122"
#> [10] "FN123" "FN124" "FN125" "FN126" "FN127" "tw2"
```
