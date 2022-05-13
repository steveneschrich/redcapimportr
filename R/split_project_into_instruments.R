#' Split project data into separate instruments
#'
#' @description REDCap data is organized into different instruments which correspond to
#' different surveys or data collection tools. This function will use the data dictionary
#' to split the combined data from a REDCap project into different instruments.
#'
#' @param project The REDCap tibble
#' @param dictionary A REDCap data dictionary tibble
#'
#' @return A list of tibbles
#' @export
#'
#' @examples
split_project_into_instruments <- function(project, dictionary) {
  vars_by_instrument <-
    dictionary %>%
      dplyr::group_by(.data$`Form Name`) %>%
      dplyr::summarize(fields = list(.data$`Variable / Field Name`)) %>%
      tibble::deframe()

  # Add in record_id, redcap_*
  purrr::map(vars_by_instrument, function(v) {
    project %>%
      dplyr::select(.data$`record_id`, tidyselect::starts_with("redcap_"), tidyselect::all_of(v) )
  })

 # if projectcontainsredcap_repeat_instrument
}
