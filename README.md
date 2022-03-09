# gfsR
The package contains a suite of Tools helpful for working with FishNet2 data files. The primary functions provide a convenient way of unzipping `DATA.ZIP` archive files and reading them directly in to the R working environment.

The most up to date version can be installed using:
`devtools::install_github("HoldenJe/gfsR")`

## Examples
- Creel data archive
`creel <- import_fn_creel("~/FNData/WCH/SF18_WCH/DATA.ZIP")`
`lapply(creel, head)`

- Index Netting Project
`nscin <- import_fn_index_net("~/FNData/NS1/IA02_NS1/DATA.ZIP")`
`lapply(nscin, head)`

---

## See also:
`rprocval`: tools for checking data structure and biological attributes
https://github.com/HoldenJe/rprocval

