
#' @description Function check if variable contains some sort of empty data
#' (NULL, NA, "", other 0-length data) and return `TRUE` (`FALSE` if data is
#' not 'empty').
#'
#' Needed for proper data validation.
check_for_empty_data <- function(value_to_check) {

  # for any 0-length
  if (length(value_to_check) == 0) return(TRUE)

  # for NA
  if (is.logical(value_to_check) && is.na(value_to_check)) return(TRUE)

  # for NULL
  if (is.null(value_to_check)) return(TRUE)

  # for non-empty Date (RETURN FALSE)
  if (inherits(value_to_check, "Date") && length(value_to_check) != 0) return(FALSE)

  # for empty strings (stands before checking non-empty data for avoid mistakes)
  if (value_to_check == "") return(TRUE)

  FALSE
}
