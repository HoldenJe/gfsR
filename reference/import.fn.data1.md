# Import a single FN project from a DATA.ZIP file

This function will generally not be called directly. It is mainly
intended to be called from with import.fn.data

## Usage

``` r
import.fn.data1(mydir, yr, program, mytables = all.tables)
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
