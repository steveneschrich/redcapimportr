#' Extract the names that are in a list
#'
#' @name from

#' @description Extract a list (or regex) `x` from a list `y`, returning the actual values.
#'
#' @details This may be a re-implementation of an existing function, however often it would be
#'  helpful to extract specific elements and return these from within a list (possible with a
#'  regex). This function does that.
#'
#' @param x A target list or regex to extract
#' @param y A list of values to search against
#'
#' @return A list of elements within `y` that match `x`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#' "A" %from% c("A","B","C")
"%from%" <- function(x, y) {
  tibble::enframe(y, name=NULL) %>%
    dplyr::filter(stringr::str_detect(.data$value, x)) %>%
    dplyr::pull("value")
}

#' Return a default value if missing
#'
#' @description If a value is null, return a default instead.
#'
#' @name or
#' @param a Value to return, unless null
#' @param b Default value to return
#'
#' @return The parameter `a`, unless it is null, in which case `b` is returned.
#' @export
#'
#' @examples
#' NA %||% 1
"%||%" <- function(a, b) {
  if (!is.null(a) & !is.na(a)) a else b
}
