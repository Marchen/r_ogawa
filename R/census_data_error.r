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
    census_data[["$__index__$"]] <- seq_along(census_data[[stem_id_column]])
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
    # Ignore distant rows (to omit unknown caused by wrong quadrat code).
    if (nrow(census_data) == 1) {
        is_continuous_rows <- TRUE
    } else {
        is_continuous_rows <- (
            max(dist(census_data[["$__index__$"]]))
            == (nrow(census_data) - 1)
        )
    }
    return(has_unknown & has_data & is_continuous_rows)
}


#------------------------------------------------------------------------------
#' Find resurrected stems
#'
#' Find resurrected stems, i.e., L -> U -> L or L -> D -> L.
#'
#' @param census_data
#'     a data.frame having census data.
#' @param stem_id_column
#'     a character specifying the column name of stem ID.
#' @param deprecated_column
#'     a character specifying the column name of deprecated records.
#' @param ld_column
#'     a character specifying the column name of alive/dead/unknown status.
#' @param year_column
#'     a character specifying the column name of year.
#'
#' @return
#'    a list of data.frames for resurrected stems.
#'
#' @export
#------------------------------------------------------------------------------
find_resurrection <- function(
    census_data, stem_id_column = "stem_id", deprecated_column = "修正済み",
    ld_column = "ld", year_column = "year"
) {
    data <- omit_deprecated(census_data, deprecated_column)
    data_split <- split(data, data[[stem_id_column]])
    result <- data_split[
        sapply(
            data_split, resurrected, ld_column = ld_column,
            year_column = year_column
        )
    ]
    return(result)
}


#------------------------------------------------------------------------------
#   (Internal) Is the stem resurrected?
#
#   @param x census_data of a stem.
#   @param ld_column
#       a character specifying the column name of alive/dead/unknown status.
#   @param year_column
#       a character specifying the column name of year.
#
#   @return
#       logical, returns TRUE if the stem resurrected.
#------------------------------------------------------------------------------
resurrected <- function(x, ld_column, year_column) {
    x <- x[order(x[[year_column]]), ]
    dead <- FALSE
    for (i in x[[ld_column]]) {
        if (!is.na(i)) {
            if (i %in% c("U", "D")) {
                dead <- TRUE
            }
            if (i == "L" && dead) {
                return(TRUE)
            }
        }
    }
    return(FALSE)
}


#------------------------------------------------------------------------------
#' Find multiple measurement
#'
#' Find multiple measurement for each ID in each year.
#'
#' @param census_data
#'     a data.frame having census data.
#' @param year_column
#'     a character specifying the column name of year.
#' @param deprecated_column
#'     a character specifying the column name of deprecated records.
#' @param id_columns
#'     character vector specifying the name(s) of column(s) containing ID(s).
#'
#' @returns
#'     A list having records of multiple measurements detected for each ID
#'     specified as id_columns.
#'
#' @export
#------------------------------------------------------------------------------
find_multiple_measurements <- function(
    census_data, year_column = "year", deprecated_column = "修正済み",
    id_columns = c("stem_id", "tag_no", "Aタグ")
) {
    data <- omit_deprecated(census_data, deprecated_column)
    result <- list()
    for (i in id_columns) {
        split_data <- split(data, data[c(year_column, i)])
        duplicates <- subset(split_data, sapply(split_data, nrow) != 1)
        result[[i]] <- do.call(rbind, duplicates)
    }
    return(result)
}


#------------------------------------------------------------------------------
#'  Find same tag on different stems
#'
#' @param census_data
#'     a data.frame having census data.
#' @param stem_id_column
#'     a character specifying the column name of stem ID.
#' @param tag_names
#'     a character vector specifying the column names of tags.
#' @param deprecated_column
#'     a character specifying the column name of deprecated records.
#'
#' @export
#------------------------------------------------------------------------------
find_tags_on_different_stems <- function(
    census_data, stem_id_column = "stem_id", tag_names = c("tag_no", "Aタグ"),
    deprecated_column = "修正済み"
) {
    data <- omit_deprecated(census_data, deprecated_column)
    result <- list()
    for (i in tag_names) {
        split_by_tag <- split(data, data[[i]])
        on_different_stem <- sapply(
            split_by_tag, function(x) length(unique(x[[stem_id_column]])) != 1
        )
        result[[i]] <- split_by_tag[on_different_stem]
    }
    return(result)
}

