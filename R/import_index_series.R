#' import_index_series: Imports a series of DATA.ZIP files
#'
#' @param datazips
#'
#' @return list of FN2 Tables
#' @export
#'
#' @examples
#' \dontrun{
#' gl1 <- import_index_series("IA21_GL1/DATA.ZIP", "IA22_GL1/DATA.ZIP")
#' }
#'
import_index_series <- function(datazips) {
  fn011 <- data.frame()
  fn012 <- data.frame()
  fn013 <- data.frame()
  fn014 <- data.frame()
  fn022 <- data.frame()
  fn026 <- data.frame()
  fn028 <- data.frame()
  fn121 <- data.frame()
  fn122 <- data.frame()
  fn123 <- data.frame()
  fn124 <- data.frame()
  fn125 <- data.frame()
  fn126 <- data.frame()
  fn127 <- data.frame()
  
  suppressWarnings(suppressMessages(fndat <-
                                      lapply(datazips, import_fn_index_net)))
  
  for (i in 1:length(fndat)) {
    fn011 <- dplyr::bind_rows(fn011, fndat[[i]]$FN011)
    fn012 <- dplyr::bind_rows(fn012, fndat[[i]]$FN012)
    fn013 <- dplyr::bind_rows(fn013, fndat[[i]]$FN013)
    fn014 <- dplyr::bind_rows(fn014, fndat[[i]]$FN014)
    fn022 <- dplyr::bind_rows(fn022, fndat[[i]]$FN022)
    fn026 <- dplyr::bind_rows(fn026, fndat[[i]]$FN026)
    fn028 <- dplyr::bind_rows(fn028, fndat[[i]]$FN028)
    fn121 <- dplyr::bind_rows(fn121, fndat[[i]]$FN121)
    fn122 <- dplyr::bind_rows(fn122, fndat[[i]]$FN122)
    fn123 <- dplyr::bind_rows(fn123, fndat[[i]]$FN123)
    fn124 <- dplyr::bind_rows(fn124, fndat[[i]]$FN124)
    fn125 <- dplyr::bind_rows(fn125, fndat[[i]]$FN125)
    fn126 <- dplyr::bind_rows(fn126, fndat[[i]]$FN126)
    fn127 <- dplyr::bind_rows(fn127, fndat[[i]]$FN127)
  }
  
  all_FN_Data <- list(
    FN011 = fn011,
    FN012 = fn012,
    FN013 = fn013,
    FN014 = fn014,
    FN022 = fn022,
    FN026 = fn026,
    FN028 = fn028,
    FN121 = fn121,
    FN122 = fn122,
    FN123 = fn123,
    FN124 = fn124,
    FN125 = fn125,
    FN126 = fn126,
    FN127 = fn127
  )
  
  lapply(
    names(all_FN_Data),
    FUN = function(x) {
      usethis::ui_done(paste0(x, " has been imported"))
    }
  )
  
  return(all_FN_Data)
}