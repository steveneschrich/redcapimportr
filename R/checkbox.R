#' Coalesce checkbox variables into a single list
#'
#' @description A checkbox variable in redcap comes in as yes/no on multiple fields. These
#' are coalesced into a single variable which is a list of selections.
#'
#' @param project A redcap tibble
#'
#' @return A modified tibble with checkbox fields replaced with single columns
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data

#' @examples
coalesce_checkbox_fields <- function(project) {
  all_checkbox_variables <- "___\\d+$" %from% colnames(project)
  all_checkbox_variables_factor <- paste0(all_checkbox_variables, ".factor")
  checkbox_variables <- all_checkbox_variables %>%
    stringr::str_replace_all("___\\d+$","") %>%
    unique()

  # Added to support no checkboxes in the data.
  if ( length(checkbox_variables) == 0 ) return(project)

  project %>%
    dplyr::bind_cols(
      purrr::map_dfc(checkbox_variables, function(v) {
        create_checkbox_variable(project, v)
      })
    ) %>%
    dplyr::select(-{{ all_checkbox_variables}}, -{{all_checkbox_variables_factor}})
}



#' Create a combined checkbox variable from individual variables
#'
#' @param project A tibble of redcap data
#' @param variable A variable to create based on individual choices
#' @param sep Separator for individual choices in the variable
#' @return A tibble column representing `variable` which combines values from checkbox variables.
#'
#' @export
#' @importFrom rlang :=
#' @examples
create_checkbox_variable <- function(project, variable, sep="|") {
  checkbox_variables <- glue::glue("{variable}___\\d+$") %from% colnames(project)
  clean_label <- stringr::str_replace_all(labelled::var_label(project %>% dplyr::select(checkbox_variables[[1]])),
                                      checkbox_choice_regex(),"")

  variable_list <- paste0(variable,"_list")

  project %>%
    # Recode checkbox variables to be values, not 0/1
    dplyr::mutate(dplyr::across(
      dplyr::all_of(checkbox_variables), ~recode_checkbox_variable(.))) %>%
    # Combine values to a single variable (without the ___)
    tidyr::unite({{variable}}, dplyr::all_of(checkbox_variables), sep=sep, na.rm=TRUE, remove = FALSE) %>%

    # Combine values into a list as well.
    embed(vars = checkbox_variables, target = variable_list) |>

    # Label new variable per the existing ones
    labelled::set_variable_labels(
      {{variable}} := clean_label,
      {{variable_list}} := clean_label
    ) %>%
    # Return the new variable
    dplyr::select(variable, variable_list)
}



#' Recode the checkbox variable based on the value embedded in variable label
#'
#' @description A checkbox variable consists of two components: a value that the checkbox
#' represents (in the variable label) and the status of the checkbox for a series of
#' observations. This function recodes the variable to the checkbox value.
#'
#' @details A checkbox variable consists of two components: a value that the checkbox
#' represents (in the variable label) and the status of the checkbox for a series of
#' observations. This function will recode a variable (checkbox) into value/NA with the
#' value being the checkbox's value.
#'
#' For example, consider an input variable
#' ```
#' checkbox_variable___1
#' "Which one or more of the following are blue? (choice=sky)"
#' [1] 0 0 0 0 1 0
#' ```
#' Note that the question is the \code{\link[labelled]{var_label}} for the variable. The values
#' of 0/1 indicate that the choice was selected or not. The result of this function should
#' return
#' ```
#' "Which one or more of the following are blue? (choice=sky)"
#'  [1] NA NA NA NA "sky" NA
#' ```
#'
#' @param variable A labelled vector representing a checkbox variable.
#'
#' @return A checkbox
#' @export
#'
#' @examples
#' recode_checkbox_variable(
#'  labelled::set_variable_labels(data.frame(sky_choice=c(0,0,0,0,1,0)),
#'                sky_choice="Which one or more of the following are blue? (choice=sky)")
#' )
#' #     sky_choice
#' #[1,] NA
#' #[2,] NA
#' #[3,] NA
#' #[4,] NA
#' #[5,] "sky"
#' #[6,] NA
recode_checkbox_variable <- function(variable) {
  ifelse(variable == 1,
         extract_checkbox_choice(labelled::var_label(variable)),
         NA
  )
}


#' Extract checkbox choice from string
#'
#' @description A checkbox choice is embedded in a string describing the question.
#'
#' @details The checkbox is embedded in a string of the type
#' ```
#' This is the question? (choice=<choice>)
#' ```
#'
#' This function extracts the `<choice>` from the text.
#'
#' @param s A string to extract the checkbox choice from.
#'
#' @return The choice string
#' @export
#'
#' @examples
#' extract_checkbox_choice( "Which one or more of the following are blue? (choice=sky)")
#' # [1] "sky"
extract_checkbox_choice <- function(s) {
  stringr::str_match(s,checkbox_choice_regex())[,2]
}

checkbox_choice_regex<-function() "\\s*\\(choice=(.+)\\)$"
