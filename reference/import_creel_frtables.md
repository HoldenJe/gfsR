# import creel frtables

This function will import the FR report tables from a single FN2 creel
project that has been analyzed in FN2. FN2 generates several FR\*
report/summary tables. Tables will be returned as items within a list.

## Usage

``` r
import_creel_frtables(generic_datazip)
```

## Arguments

- generic_datazip:

  a file path to a single FN2 DATA.ZIP creel project

## Value

list a list with "FR712", "FR713", "FR714" and "FR715" tables

## See also

\[import_creel_series_frtables()\] for combining multiple projects

## Examples

``` r
if (FALSE) { # \dontrun{
cr1_frtable <- import_creel_frtables("SC97_WL1/DATA.ZIP")
lapply(cr1_frtable, head)
} # }

```
