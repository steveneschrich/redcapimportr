#' Import REDCap data into R
#'
#' @description REDCap exported data (exported to R) is imported with a few tweaks to the data
#' for usability.
#'
#' @details
#' REDCap allows a user to export all of the data in the project to R format. This results in
#' two files (an R file and a CSV file). Typically, one would simply source the R file and the
#' REDCap data would be available in the R session. However, there are a few small caveats with
#' this data, prompting the creation of this function.
#'
#' Specifically, there are two issues:
#' \itemize{
#' \item Clearing the environment
#' \item No labels on factor variables
#' }
#'
#' This function fixes those two issues and returns a \code{\link[tibble]{tibble}} for
#' further processing.
#'
#' @note The input parameter to this function can be either the R script name, or the R/CSV pairs
#'  as determined by \code{\link{return_filename_pair}}.
#'
#' @param f A R script filename, or the R/CSV pairs as determined by \code{\link{return_filename_pair}}
#'
#' @return A \code{\link[tibble]{tibble}} of REDCap imported data.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' import_redcap_data("export_from_redcap.R")
#' }
import_redcap_data <- function(f) {
  stopifnot(is_filename_r(f) || (is.list(f) && utils::hasName(f, "r")))
  f <- ifelse(is.list(f), f$r, f)

  message(glue::glue("Loading REDCap data from {f}."))

  # First, load the script that will load the data and return it as a function.
  r_script <- load_redcap_script(f)

  # The script defines a function (r_script) which will load the data for us.
  res<-r_script() %>%
    # Label the factor variables (which are not done by default)
    label_factor_variables() %>%
    # Merge checkboxes into a single field
    coalesce_checkbox_fields()


  res
}


#' Apply Label Information to Factor Variables in REDCap Output
#'
#' @description By default, REDCap imported data does not include the label information
#' for the variables that are transformed to factors. This function adds them in.
#'
#' @details
#' The REDCap default import is great. It knows what variables should be factors and turns them
#' into factors (while keeping the original numerical forms). This is really helpful when summarizing
#' REDCap data. However, a minor point is that, although the numerical forms have labels (e.g., the
#' question asked), the converted factor variables do not.
#'
#' This function adds in labels to the factor variables. It does this by simply copying over the
#' label that was on the non-factor version of the variable. The resulting data is then returned.
#'
#' @note This function is not exported and should generally not be called outside of the package.
#'  It is part of a set of function calls to clean up redcap data. Check out \code{\link{import_redcap_data}}
#'  for the best entrypoint to this code.
#'
#' @param x A table of REDCap imported data.
#'
#' @return A tibble of REDCap imported data with factor variables having labels.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
label_factor_variables <- function(x) {

  # Convert to a tibble for easier manipulation. Some dplyr magic below.
  x <- tibble::as_tibble(x)

  # Factor variables do not have labels, but the non-factor versions do. So
  # do some dplyr magic to set the factor label to the same as the non-factor
  # version. This magic gives you a named list of variable=value to label.
  var_map <- tibble::enframe(
    sapply(labelled::var_label(x), function(v){if (is.null(v)) "" else v})) %>%
    dplyr::mutate(name = stringr::str_replace(.data$name, "\\.factor","")) %>%
    dplyr::group_by(.data$name) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::summarize( value = stringr::str_c(.data$value, collapse="")) %>%
    dplyr::mutate( name = stringr::str_c(.data$name,".factor")) %>%
    dplyr::select(.data$name, .data$value) %>%
    tibble::deframe() %>%
    as.list()

  # The labelled package has a nice vectorized way to set labels.
  x <- labelled::set_variable_labels(x, .labels = var_map)

  x
}

#' Load REDCap data loading script
#'
#' @description This is a function that will read in an R script which was generated
#'   to load REDCap data into R. It will return a function that, when called, will
#'   perform this work.
#'
#' @details
#' REDCap generates an R script that is used to load REDCap data (and labels, etc) into
#' R. The R script will load the corresponding csv file (that contains the data) and
#' set the various metadata based on REDCap information. This script is typically sourced
#' into R directly.
#'
#' This function is a wrapper to load this R script in and make the data loading available
#' as a function call. That is,
#' ```
#' load_redcap_script(filename)()
#' ```
#' will give the actual REDCap data, formatted as intended.
#'
#' Why do this? There are several issues with the default REDCap R script. First off, they remove
#' everything in the environment. This is helpful to avoid name collisions, etc but not as helpful
#' as part of a larger system or library. Secondly, the files (csv and R) have to be in the working
#' directory (in the default script) which is a bit limiting. Therefore, this function takes care
#' of both of these issues which makes loading easier.
#'
#' @note This function generally shouldn't be run (and is not exported) since there is a higher-level
#' function, \code{\link{import_redcap_data}} that manages a few other aspects of the data loading. This
#' function only manages loading the R script itself.
#'
#' @param f The R script file to load
#' @return A function that, when executed with no arguments, will load the REDCap data.
#'
#' @importFrom magrittr %>%
#'
#'
utils::globalVariables(".")
load_redcap_script <- function(f) {
  # Read the script in so we can fix a few lines in it.
  r_script <- readLines(f) %>%
    tibble::enframe(value="text", name=NULL) %>%
    # Remove the rm() entry in the script.
    dplyr::filter(stringr::str_detect(.data$text, "rm\\(list=ls\\(\\)\\)", negate=TRUE)) %>%
    # Remove the library() call to keep from adding to the namespace
    dplyr::filter(stringr::str_detect(.data$text, "^library", negate = TRUE)) %>%
    # Since library is removed, prefix calls to 'label' with Hmisc::
    dplyr::mutate(text = stringr::str_replace_all(.data$text, "^label\\(","Hmisc::label\\(")) %>%
    # Wrap script in a function call, so that can evaluate it without respect to
    # the global environment
    dplyr::bind_rows(c(text=".internal_redcap_load<-function() {"), .) %>%
    dplyr::bind_rows(c(text=" data }")) %>%
    # Next, when we read the csv file it doesn't have the option to be in another directory.
    # Here, we'll assume it's in the same place as f, so translate to csv file.
    dplyr::mutate(text = stringr::str_replace(.data$text,
                                              "^(data=read.csv\\()'.*'(\\))$",
                                              glue::glue("\\1'{r_filename_to_csv(f)}'\\2") )) %>%
    dplyr::pull("text")



  # Simple check that the rm was removed successfully.
  stopifnot(length(grep("rm(list=ls())",r_script))==0)

  # Now we can define the loader function, then call it. Local keeps it in
  # the function's environment (rather than global).
  source(exprs=parse(text=r_script), local=TRUE)

  # Return the function for loading.
  get(".internal_redcap_load")
}

