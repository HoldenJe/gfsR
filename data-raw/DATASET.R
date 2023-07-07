## code to prepare `DATASET` dataset goes here
library(gfsR)
tw2 <- import_fn_index_net("data-raw/IA15_TW2/DATA.ZIP")
tw2 <- lapply(gn, head)
usethis::use_data(tw2, overwrite = TRUE)
