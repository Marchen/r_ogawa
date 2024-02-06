#------------------------------------------------------------------------------
#' Find missing stem and individual IDs.
#'
#' @param stem_data
#'     a data.frame having stem sheet.
#' @param stem_id_column
#'     a character specifying the column name of stem ID.
#' @param individual_id_column
#'     a character specifying the column name of individual ID.
#'
#' @export
#------------------------------------------------------------------------------
find_missing_ids <- function(
    stem_data, stem_id_column = "stem_id", individual_id_column = "ind_id"
) {
    missing_ids <- list()
    for (i in c(stem_id_column, individual_id_column)) {
        all_ids <- as.numeric(stem_data[[i]])
        max_id <- max(all_ids, na.rm = TRUE)
        all_possible_id <- 1:max_id
        missing_ids[[i]] <- all_possible_id[!all_possible_id %in% all_ids]
    }
    return(missing_ids)
}


#------------------------------------------------------------------------------
#' Find duplicated stem ID in the stem sheet
#'
#' Find duplicated stem ID from the stem sheet.
#' Also this function checks the duplicated ID(s) having inconsistent data
#' of stem level information.
#'
#' @param stem_data
#'     a data.frame having stem sheet.
#' @param stem_id_column
#'     a character specifying the column name of stem ID.
#' @param deprecated_column
#'     a character vector specifying the column name(s) of stem level
#'     information by which inconsistency of stem level information
#'     will be checked.
#' @param stem_level_info_columns
#'     column names of stem level information such as individual id or quadrat
#'     code.
#'
#' @returns
#' A list having following fields:
#' *with_error*:
#'     a data.frame for the stem data with duplicated stem ID with some
#'     inconsistency of stem-level information.
#' *without_error*:
#'     a data.frame for the stem data with duplicated stem ID without
#'     inconsistent stem-level information.
#'  TODO: この関数、幹ID確認と幹レベル情報の確認に分離した方がよい。
#' @export
#------------------------------------------------------------------------------
find_duplicated_stem_id <- function(
    stem_data, stem_id_column = "stem_id", deprecated_column = "修正済み",
    stem_level_info_columns = c("Q", "ind_id", "species")
) {
    # Find duplicated stem IDs.
    data <- omit_deprecated(stem_data, deprecated_column)
    data_split <- split(data, data[stem_id_column])
    duplicated_data <- data_split[sapply(data_split, nrow) != 1]
    # Find data inconsistency.
    has_error <- list()
    for (i in stem_level_info_columns) {
        has_error[[i]] <- sapply(
            duplicated_data, function(x) length(unique(x[[i]])) != 1
        )
    }
    # Split duplicated stem data for with and without inconsistency.
    has_error <- as.data.frame(has_error)
    has_error <- apply(has_error, 1, any)
    result <- list()
    if (any(has_error)) {
        result$with_error <- do.call(rbind, duplicated_data[has_error])
    }
    if (any(!has_error)) {
        result$without_error <- do.call(rbind, duplicated_data[!has_error])
    }
    return(result)
}
