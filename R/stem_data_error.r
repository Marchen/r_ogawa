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
