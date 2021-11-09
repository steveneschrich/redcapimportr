
#' Return REDCap filename pairs (R and DATA)
#'
#' @param f A filename to derive pairs from
#'
#' @return A list of two filenames: the R and DATA elements.
#' @export
#'
#' @examples
#' return_filename_pair("PartnershipTrackingP_DATA_2021-11-08_1628.csv")
#'
return_filename_pair <- function(f) {

  list(r=r_filename(f), csv=csv_filename(f))
}

#' Is the filename an R script?
#'
#' @param f A filename to test
#'
#' @return Logical. If the filename is an r script.
#' @export
#'
#' @examples
#' is_filename_r("test.R")
#' # [1] TRUE
#'
is_filename_r <- function(f) {
  stringr::str_ends(f,  "\\.[rR]")
}

#' Is the filename a csv file?
#'
#' @param f  A filename to test
#'
#' @return Logical. If the filename is a csv file.
#' @export
#'
#' @examples
#' is_filename_csv("test.csv")
#' # [1] TRUE
is_filename_csv <- function(f) {
  stringr::str_ends(f, "\\.[cC][sS][vV]")
}

#' Return the R filename based on input filename
#'
#' @description Given a filename, return the R filename regardless
#' of which input (csv, R) is provided.
#'
#' @param f A REDCap filename (either csv or R)
#'
#' @return A R filename derived from the input filename.
#' @export
#'
#' @examples
#' r_filename("PartnershipTrackingP_DATA_2021-11-08_1628.csv")
#' # [1] "PartnershipTrackingP_R_2021-11-08_1628.r"
r_filename <- function(f) {
  ifelse(is_filename_r(f), f, csv_filename_to_r(f))
}

#' Return the CSV filename based on input filename
#'
#' @description Given a filename, return the R filename regardless
#' of which input (csv, R) is provided.
#'
#' @param f A REDCap filename (either csv or R)
#'
#' @return  A CSV filename derived from the input filename.
#' @export
#'
#' @examples
#' csv_filename("PartnershipTrackingP_R_2021-11-08_1628.r")
csv_filename <- function(f) {
  ifelse(is_filename_csv(f), f, r_filename_to_csv(f))

}
#' Split Filename into REDCap Components
#'
#' @description Split a filename from REDCap export into the components, as
#'   a named list.
#'
#' @details A REDCap export file set consists of two filenames with specific
#'   format names. This function splits the name into components which are
#'   then returned in a named list. The items of the list are
#'   \describe{
#'   \item{preamble}{The name of the file, without the REDCap pieces}
#'   \item{type}{The type, which is either R or DATA}
#'   \item{date}{The YYYY-MM-DD of the export}
#'   \item{time}{The HHMM of the export}
#'   \item{ext}{The file extensions}
#' }
#' @param f The filename to split apart.
#'
#' @return A list of filename components, or the filename (plus a warning) in
#' the case of an error.
#' @export
#'
#' @examples
#' split_redcap_filename("PartnershipTrackingP_DATA_2021-11-08_1628.csv")
#' split_redcap_filename("PartnershipTrackingP_R_2021-11-08_1628.r")
split_redcap_filename <- function(f) {
  m <-stringr::str_match(f, "(.*)P_(R|DATA)_(\\d{4}\\-\\d{2}\\-\\d{2})_(\\d{4})\\.(csv|r)")
  # If full match is NA, the file is not in the correct format.
  if (is.na(m[1,1])) {
    warning(glue::glue("`{f}` not in appropriate form to split per REDCap standards."))
    return(f)
  }
  list(preamble = m[1,2], type = m[1,3],
       date = m[1,4], time = m[1,5], ext = m[1,6])
}


#' Transform R filename to CSV filename.
#'
#' @param f A filename to transform.
#'
#' @return A CSV filename, equivalent to the R filename
#' @export
#'
#' @examples
#' r_filename_to_csv("PartnershipTrackingP_R_2021-11-08_1628.r")
r_filename_to_csv <- function(f) {
  if (!is_filename_r(f)) {
    warning(glue::glue("{f} is not an R file to convert to CSV."))
    return(NA)
  }

  comp<-split_redcap_filename(f)
  comp$type <- "DATA"
  comp$ext <- "csv"
  format_redcap_filename(comp)
}

#' Transform CSV filename to R filename.
#'
#' @param f A filename to transform.
#'
#' @return A R filename, equivalent to the CSV filename
#' @export
#'
#' @examples
#' csv_filename_to_r("PartnershipTrackingP_DATA_2021-11-08_1628.csv")
csv_filename_to_r <- function(f) {
  if (!is_filename_csv(f)) {
    warning(glue::glue("{f} is not a CSV file to convert to R."))
    return(NA)
  }

  comp<-split_redcap_filename(f)
  comp$type <- "R"
  comp$ext <- "r"
  format_redcap_filename(comp)
}

#' Format REDCap components to a filename.
#'
#' @description The components of a REDCap file, as a list of components, can
#' be reassembled into a filename string. This function does that.
#'
#' @param comp A list of components. See \code{\link{split_redcap_filename}}.
#'
#' @return A character vector filename corresponding to the components.
#' @export
#'
#' @examples
#' format_redcap_filename(list(preamble="PartnershipTracking",
#' type="DATA",
#' date="2021-11-08",
#' time = "1628",
#' ext="csv"
#' ))
#'
format_redcap_filename<-function(comp) {
  as.character(glue::glue_data(comp, "{preamble}P_{type}_{date}_{time}.{ext}"))
}
