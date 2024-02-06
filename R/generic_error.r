#------------------------------------------------------------------------------
#' Validate format of 1cm tag
#'
#' @param tag_no a character vector containing 1cm tag numbers.
#' @return for valid 1cm tag numbers, return TRUE.
#' @export
#------------------------------------------------------------------------------
is_valid_1cm_tag <- function(tag_no) {
    re_a_tag <- "^A[0-9]{7}$"
    return(grepl(re_a_tag, tag_no))
}


#------------------------------------------------------------------------------
#' Find data inconsistency for given ID
#'
#' Find records having multiple values for a given ID, such as different
#' quadrat codes, individual ids, species, etc.
#'
#' @param data
#'     a data.frame to be checked.
#' @param id_columns
#'     a character vector representing the name(s) of column(s) used as ID(s).
#' @param data_columns
#'     a character vector specifying the names of columns to check
#'     consistency.
#' @param deprecated_column
#'     a character specifying the name of column showing deprecated records.
#'
#' @export
#------------------------------------------------------------------------------
find_data_inconsistency <- function(
    data, id_columns, data_columns = c("Q", "ind_id", "species", "x", "y"),
    deprecated_column = "修正済み"
) {
    data <- omit_deprecated(data, deprecated_column)
    split_data <- split(data, data[[id_columns]])
    result <- list()
    for (i in data_columns) {
        index <- sapply(split_data, function(x) length(unique(x[[i]])) > 1)
        if (length(index) == 0) {
            next
        }
        errors <- split_data[index]
        errors <- do.call(
            rbind, lapply(errors, function(x) transform(x, error = i))
        )
        result[[i]] <- errors
    }
    return(result)
}
