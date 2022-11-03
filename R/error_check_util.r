#------------------------------------------------------------------------------
#   (Internal) Omit deprecated rows
#
#   @param data
#       data.frame that will be removed deprecated rows.
#   @param deprecated_column
#       a character specifying the name of column showing deprecated records.
#
#   @return
#       data.frame without deprecated rows.
#------------------------------------------------------------------------------
omit_deprecated <- function(data, deprecated_column) {
    return(subset(data, is.na(data[[deprecated_column]])))
}
