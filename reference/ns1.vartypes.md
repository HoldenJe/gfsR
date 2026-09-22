# Assign NS variable types

Changes the default 'chr' class of each variable to the desired class
type (e.g. 'int') for NS data. Generally this function will not be
called directly as it is called from within 'import.fn.data'

## Usage

``` r
ns1.vartypes(ns1table)
```

## Arguments

- ns1table:

  is a list containing each FN table as an item in the list

## Value

returns a list that contains each FN table as an item in the list
