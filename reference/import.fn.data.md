# import.fn.data is the generic function used to load data

This is primary import function for loading data from archived FishNet2
projects. It assumes that data is archived using the existing GFS data
file index (e.g. "/TW1/LOA_IA13/DATA.ZIP")

## Usage

``` r
import.fn.data(mydir, yr, program, mytables = all.tables)
```

## Arguments

- mydir:

  should specify the local data warehouse ('c:/FN_Data')

- yr:

  is a field season year (e.g. 2013),

- program:

  is a character vector specifying which program to build a project code
  for ('GL1')

- mytables:

  is either a single FN2 table ('FN121') or a list of tables ('FN121',
  'FN125'). If no value is supplied the default import routine is to
  import all supported tables.

## Value

returns a list that contains each FN2 table as an item in the list
