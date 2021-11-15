#f<-"~eschris/Desktop/TOFILE/RECData/data-raw/RECResearchSummerRotationPrePo_DataDictionary_2021-09-29.csv"


#' Import REDCap-exported data dictionary
#'
#' @description REDCap provides a csv-formatted data dictionary, describing variables,
#' instruments and the variable type. This function loads the dictionary into R.
#'
#' @param f A filename representing the data dictionary export.
#'
#' @return A tibble (see details) of information.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{import_data_dictionary("data_dictionary_export_from_redcap.csv")}
import_data_dictionary <- function(f) {

  message(glue::glue("Loading REDCap data dictionary from {f}."))
  dd<-readr::read_csv(f, col_types=list(readr::col_character()) ) %>%
    dplyr::mutate(Levels = split_choices(.data$`Choices, Calculations, OR Slider Labels`))

  dd
}

#' Split text encoding of choices
#'
#' @description The REDCap data dictionary encodes the choices information for variables a
#' certain way. This function extracts the details into a list of character matrices.
#'
#' @details
#' REDCap provides choice encoding in the form of
#' ```
#' 1, Poor|2, Average
#' ``
#' This function will extract the individual levels (e.g., 1,Poor vs 2,Average) and then
#' split each into a value and label (e.g., 1 and Poor). See \code{\link{split_levels}} for
#' details on the second step.
#'
#' The result of this function call is a list of character tibbles (corresponding to the
#' vector of input strings) with each containing a tibble of two columns: `Value` and `Label`.
#'
#' @param s A string to split (vectorized)
#'
#' @return A list of character tibbles with Value/Label column names.
#' @export
#'
#' @examples
#' split_choices(c(
#'   "1, Poor|2, Below Average|3, Average|4, Good|5, Excellent|6, Not Applicable",
#'   NA
#' ))
split_choices <- function(s) {
  purrr::map(stringr::str_split(s,"\\|"), ~split_levels(.data$.))
}


#' Split a REDCap-encoded level into Value and Label
#'
#' @description Split a string representing a REDCap-encoded level into two
#' components: a value and a label.
#'
#' @details REDCap provides Choices in the form of `2, Good`. This function will
#' split this encoding into a tibble of two columns called `Value` and `Label`.
#'
#' @param s A string representing a level (vectorized).
#'
#' @return A tibble (`Value`, `Label`) representing the encoded string.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples
#'
#' split_levels(c("1, Poor", "2, Good"))
split_levels <- function(s) {
  stringr::str_split_fixed(s, pattern=", ", n=2) %>%
    magrittr::set_colnames(c("Value","Label")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$Value != "")
}
