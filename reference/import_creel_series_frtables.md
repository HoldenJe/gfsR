# import creel series frtables

This function is a wrapper for \`import_creel_frtables\` that will
import the FR report tables from multiple FN2 creel projects that have
been analyzed in FN2. FN2 generates several FR\* report/summary tables.
Tables will be returned as items within a list.

## Usage

``` r
import_creel_series_frtables(datazips)
```

## Arguments

- datazips:

  a vector of file paths to FN2 DATA.ZIP creel projects

## Value

list a list with "FR712", "FR713", "FR714" and "FR715" tables

## See also

\[import_creel_frtables()\] for combining multiple projects

## Examples

``` r
if (FALSE) { # \dontrun{
cr1_frtable <- import_creel_series_frtables("SC97_WL1/DATA.ZIP", "SC96_WL11/DATA.ZIP")
lapply(cr1_frtable, head)
} # }
```
