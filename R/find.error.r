#------------------------------------------------------------------------------
#' Validate format of 1cm tag
#'
#' @param tag_no a character vector containing 1cm tag numbers.
#' @return for valid 1cm tag numbers, return TRUE.
#' @export
#------------------------------------------------------------------------------
is_valid_1cm_tag <- function(tag_no) {
    re_atag <- "^A[0-9]{7}$"
    return(grepl(re_atag, tag_no))
}
