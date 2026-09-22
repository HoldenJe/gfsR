# Convert XSILOC1 FN2 Field to LAT and LON fields

XSILOC1 is a 12 character field that records LAT and LON in the format
DDMMddDDMMdd (LAT then LON). This function uses two helper functions to
parse the field and add a LAT and LON field to the FN121 table provided.

## Usage

``` r
SILOC2COORD(fn121, fn121column)
```
