# import_fn_creel

A generic import function similar to open.fn.data1 but for creel files.
Project path build is omitted so it must be provided in file path. The
function will import the following tables: FN011, FN012, FN022, FN023,
FN024, FN025, FN026, FN028, FN111, FN112, FN121, FN123, FN124, FN125,
FN126, and FN127.

## Usage

``` r
import_fn_creel(generic_datazip)
```

## Arguments

- generic_datazip:

  File path to FN DATA.ZIP folder for a creel project

## Value

Function returns a list of FN tables. If a table is not included in the
FN project an empty data frame will still be returned as well as a
\`usethis\` message during import.
