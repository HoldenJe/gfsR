#' Correct legacy FN2 date substitutions
#'
#' Legacy FN2 creel projects conducted in 2000 or later may contain
#' surrogate calendar years chosen to preserve weekday/weekend alignment.
#' The true survey year is extracted from the project code (PRJ_CD; SCYY
#' format) and used to replace the year component of all Date fields
#' throughout the project.
#'
#' The original imported year is preserved in
#' \code{FN011$FN2LEGACYYEAR}.
#'
#' Users of \code{import_fn_creel1()} or \code{import_creel_series()}
#' do not normally need to call this function directly, as it is intended
#' to be applied automatically during import.
#'
#' Projects with survey years prior to 2000 are returned unchanged.
#'
#' @param x A creel project object.
#'
#' @return A creel project object with corrected Date fields.
#'
#' @details
#' The original imported year is stored in
#' \code{FN011$FN2LEGACYYEAR}. If this field already exists,
#' the function assumes the project has previously been transformed
#' and will stop with an error.
#'
#' @seealso
#' \code{\link{import_fn_creel1}},
#' \code{\link{import_creel_series}}
#'
#' @examples
#' \dontrun{
#' # Normally called internally by import_fn_creel() and
#' # import_creel_series().
#' }
#'
#' @export
correct_fn2creel_legacy_dates <- function(x) {
  # Fail if project has already been transformed
  if ("FN2LEGACYYEAR" %in% names(x$FN011)) {

    stop(
      paste(
        "FN011 already contains FN2LEGACYYEAR.",
        "This project appears to have already been transformed."
      ),
      call. = FALSE
    )
  }
  # Extract survey year from SCYY portion of PRJ_CD
  get_project_year <- function(prj_cd) {

    yy <- stringr::str_extract(prj_cd, "(?<=SC)\\d{2}")

    if (is.na(yy)) {
      stop(
        "Unable to extract survey year from PRJ_CD: ",
        prj_cd,
        call. = FALSE
      )
    }

    yy <- as.integer(yy)

    # Assumes projects 00-39 belong to 2000-2039
    if (yy < 40L) {
      2000L + yy
    } else {
      1900L + yy
    }
  }

  # Basic validation
  if (!is.list(x) || is.null(x$FN011)) {
    stop(
      "Input does not appear to be a valid FN2 project.",
      call. = FALSE
    )
  }

  survey_year <- get_project_year(x$FN011$PRJ_CD[1])

  # Do nothing for genuine pre-2000 projects
  if (survey_year < 2000L) {
    return(x)
  }

  # Record original imported year
  legacy_year <- as.integer(
    format(
      x$FN011$PRJ_DATE0[1],
      "%Y"
    )
  )

  x$FN011$FN2LEGACYYEAR <- legacy_year

  # Process all tables
  for (table_name in names(x)) {

    dat <- x[[table_name]]

    if (!is.data.frame(dat)) {
      next
    }

    date_cols <- names(dat)[
      vapply(
        dat,
        inherits,
        logical(1),
        what = "Date"
      )
    ]

    if (length(date_cols) == 0) {
      next
    }

    for (col in date_cols) {

      old_dates <- dat[[col]]

      new_dates <- as.Date(
        sprintf(
          "%04d-%s",
          survey_year,
          format(old_dates, "%m-%d")
        )
      )

      # Warn if impossible dates are created
      invalid_dates <- is.na(new_dates) & !is.na(old_dates)

      if (any(invalid_dates)) {

        warning(
          sprintf(
            paste(
              "Invalid date(s) generated while updating",
              "%s$%s to survey year %s."
            ),
            table_name,
            col,
            survey_year
          ),
          call. = FALSE
        )
      }

      # Warn if weekday alignment changes
      valid_idx <- !is.na(old_dates) & !is.na(new_dates)

      if (any(valid_idx)) {

        weekday_match <- weekdays(old_dates[valid_idx]) ==
          weekdays(new_dates[valid_idx])

        if (!all(weekday_match)) {

          warning(
            sprintf(
              paste(
                "Weekday mismatch detected in %s$%s",
                "after updating dates to survey year %s."
              ),
              table_name,
              col,
              survey_year
            ),
            call. = FALSE
          )
        }
      }

      dat[[col]] <- new_dates
    }

    x[[table_name]] <- dat
  }

  usethis::ui_done("Project dates have been converted from legacy dates to actual project year")
  return(x)
}