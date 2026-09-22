# standardize.catch

merges FN121 to FN123 and generates a catch table

## Usage

``` r
standardize.catch(NorW, fn121, fn123, netnights = FALSE, autofill = F)
```

## Arguments

- NorW:

  specifies 'CATCNT' or 'CATWT'

- fn121:

  a valid FN121 table

- fn123:

  a valid FN123 table

- netnights:

  specifies whether catch totals should be standarizded to a 24 hour net
  set

- autofill:

  specifies whether standardize catch should run fill.CATCNT or
  fill.CATWT
