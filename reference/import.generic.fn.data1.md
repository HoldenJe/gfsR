# import.generic.fn.data1

A generic import function. Project file path build is omitted so it must
be provided as part of the argument.

## Usage

``` r
import.generic.fn.data1(generic_datazip)
```

## Arguments

- generic:

  File path to FN DATA.ZIP folder

## Value

a list of FN2 tables

## Examples

``` r
if (FALSE) { # \dontrun{
fn <- import.generic.fn.data("~/FNData/NS1/IA02_NS1/DATA.ZIP")
fn$FN011
lapply(fn, head)
} # }
```
