# fix_date

A generic function to fix dates in FN2 files. Dates after 2000 are
recorded as 19XX.

## Usage

``` r
fix_date(fndt0)
```

## Arguments

- fndt0:

  A date field in FN2 files

## Value

a formatted date field

## Examples

``` r
fix_date(as.Date("1920-07-10")); fix_date(as.Date("1990-07-10")) 
#> [1] "2020-07-10"
#> [1] "1990-07-10"
```
