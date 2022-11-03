#------------------------------------------------------------------------------
#' Find records with unknown and measured values
#'
#' Find records of a stem in a year with unknown and measured value(s).
#'
#' @param census_data
#'     census data.
#' @param stem_id_column
#'     a character specifying the column name of stem ID.
#' @param ld_column
#'     a character specifying the column name of alive/dead/unknown status.
#' @param deprecated_column
#'     a character specifying the column name of deprecated records.
#' @param year_column
#'     a character specifying the column name of year.
#' @param unknown_code
#'      a character specifying unknown code.
#' @param measurements
#'     a character vector specifying the column names having measurements.
#'
#' @return
#'     a data.frame having records of stems having both unknown and
#'     measurements in a year.
#'     Returns NULL if no record was detected.
#'
#' @export
#------------------------------------------------------------------------------
find_unknown_with_measurements <- function(
    census_data, stem_id_column = "stem_id", ld_column = "ld",
    deprecated_column = "修正済み", year_column = "year", unknown_code = "U",
    measurements = c("gbh", "cls")
) {
    split_data <- split(
        census_data, census_data[c(stem_id_column, year_column)], drop = TRUE
    )
    result <- split_data[
        sapply(
            split_data, is_unknown_with_measurements, ld_column = ld_column,
            deprecated_column = deprecated_column, unknown_code = unknown_code,
            measurements = measurements
        )
    ]
    return(do.call(rbind, result))
}


#------------------------------------------------------------------------------
#   (Internal) Check unknown with measurement
#
#   Check the record of a stem in a year has unknown and measured records.
#
#   @param census_data
#       census data of a stem in a year.
#   @param ld_column
#       a character specifying the column name of alive/dead/unknown status.
#   @param deprecated_column
#       a character specifying the column name of deprecated records.
#   @param unknown_code
#       a character specifying unknown code.
#   @param measurements
#       a character vector specifying the column names having measurements.
#
#   @return
#       TRUE if the record having both unknown and measurements.
#------------------------------------------------------------------------------
is_unknown_with_measurements <- function(
    census_data, ld_column, deprecated_column, unknown_code, measurements
) {
    current_data <- census_data[is.na(census_data[[deprecated_column]]), ]
    if (nrow(current_data) == 0) {
        return(FALSE)
    }
    has_unknown <- unknown_code %in% census_data[[ld_column]]
    not_na <- function(x) !is.na(x)
    has_data <- any(unlist(sapply(current_data[measurements], not_na)))
    return(has_unknown & has_data)
}
