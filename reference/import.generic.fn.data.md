# import.generic.fn.data

A generic import function will open and compile multiple FN projects.
Project path build is omitted so it must be provided in file path. This
function allows the import of projects other than 'GL1' and 'TW1' but
doesn't not format the outputs to the same extent.

## Usage

``` r
import.generic.fn.data(generic, mytables = all.tables)
```

## Arguments

- generic:

  File path to FN DATA.ZIP folder

- mytables:

  Specify which FN data tables are to be imported.

## Value

a list of FN2 tables

## Examples

``` r
if (FALSE) { # \dontrun{
prjlist <- c("~/FNData/NS1/IA02_NS1/DATA.ZIP", "~/FNData/GL1/IA15_GL1/DATA.ZIP")
fn <- import.generic.fn.data(prjlist)
fn$FN011
lapply(fn, head)
} # }
```
