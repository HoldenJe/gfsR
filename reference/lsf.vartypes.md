# Assign LSF variable types

Changes the default 'chr' class of each variable to the desired class
type (e.g. 'int') for LSF data. Generally this function will not be
called directly as it is called from with 'import.fn.data'

## Usage

``` r
lsf.vartypes(lsftable)
```

## Arguments

- lsftable:

  is a list containing each FN table as an item in the list

## Value

returns a list that contains each FN2 table as an item in the list
