
get_dummy_data <- function(type) {
  if (type %in% c("text", "select_one", "select_multiple")) return("dummy")
  if (type %in% c("radio", "checkbox")) return("dummy")
  if (type %in% c("date")) return(as.Date("1990-01-01"))
  if (type %in% c("number")) return(as.double(999))
}

# get_empty_data <- function(type) {
#   if (type %in% c("text", "select_one", "select_multiple")) return(as.character(NA))
#   if (type %in% c("radio", "checkbox")) return(as.character(NA))
#   if (type %in% c("date")) return(as.Date(NA))
#   if (type %in% c("number")) return(as.character(NA))
# }

get_dummy_df <- function() {
  purrr::map(
    .x = inputs_simple_list,
    .f = get_empty_data
  ) %>%
    as_tibble()
}


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
