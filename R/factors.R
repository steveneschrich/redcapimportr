#' Split instrument into labelled and unlabelled versions
#'
#' @param x An input REDCap instrument (or raw combination of instruments).
#'
#' @return A list of two tibbles, `labelled` and `unlabelled`.
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
split_instrument_by_labels<-function(x) {
  var_pairs<-find_factor_pairs(x)

  # Create the labelled set (having the factors, not the unlabelled variables).
  labelled <- x %>%
    dplyr::select(dplyr::everything(), -dplyr::all_of(var_pairs$unlabelled)) %>%
    dplyr::rename_with(~stringr::str_replace_all(.x, "\\.factor",""))
  # Create the unlabelled set (no factors).
  unlabelled <- x %>%
    dplyr::select(dplyr::everything(), -dplyr::all_of(var_pairs$labelled))

  list(labelled = labelled, unlabelled = unlabelled)
}

#' Find variable pairs with factor and non-factor entries
#'
#' @param x A redcap instrument
#'
#' @return A tibble of pairs (`unlabelled` and `labelled`).
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
find_factor_pairs<-function(x) {
  colnames(x) %>%
    tibble::enframe("name"=NULL, value = "unlabelled") %>%
    dplyr::filter(stringr::str_detect(.data$unlabelled, "\\.factor$", negate=TRUE)) %>%
    dplyr::mutate(putative_factor_var = stringr::str_c(.data$unlabelled, ".factor", sep="")) %>%
    dplyr::inner_join(tibble::enframe(colnames(x), "name"=NULL, value="colname"),
                      by=c("putative_factor_var"="colname")) %>%
    dplyr::rename(labelled = .data$putative_factor_var)

}
