# Correct legacy FN2 date substitutions

Legacy FN2 creel projects conducted in 2000 or later may contain
surrogate calendar years chosen to preserve weekday/weekend alignment.
The true survey year is extracted from the project code (PRJ_CD; SCYY
format) and used to replace the year component of all Date fields
throughout the project.

## Usage

``` r
correct_fn2creel_legacy_dates(x)
```

## Arguments

- x:

  A creel project object.

## Value

A creel project object with corrected Date fields.

## Details

The original imported year is preserved in `FN011$FN2LEGACYYEAR`.

Users of `import_fn_creel1()` or
[`import_creel_series()`](https://holdenje.github.io/gfsR/reference/import_creel_series.md)
do not normally need to call this function directly, as it is intended
to be applied automatically during import.

Projects with survey years prior to 2000 are returned unchanged.

The original imported year is stored in `FN011$FN2LEGACYYEAR`. If this
field already exists, the function assumes the project has previously
been transformed and will stop with an error.

## See also

`import_fn_creel1`,
[`import_creel_series`](https://holdenje.github.io/gfsR/reference/import_creel_series.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Normally called internally by import_fn_creel() and
# import_creel_series().
} # }
```
