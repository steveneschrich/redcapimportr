#' Embed columns into a longer, nested tibble
#'
#' @description Embed a selected list of columns (variables) into a `long` tibble that
#' is nested per row.
#'
#' @details This function provides a two-step transformation. When provided with a list of `vars`,
#' it will extract these variables and transpose them into an embedded data frame with each row of
#' the data frame representing one of the variables in the original table.
#
#' Behind the scenes, it will use [tidyr::pivot_longer()] to transform these variables into repeated rows.
#' It will then nest these repeated rows into a tibble.
#'
#' @param x A data frame to embed variables within
#' @param vars The columns (variables) to extract and embed.
#' @param target The column name for the embedded data.
#' @param embedded_names (Default NULL) The embedded column name for variable names (if they are kept).
#' Use NULL to keep only column values, not the column name.
#'
#' @return A data frame with `vars` embedded as a tibble, with each variable in a separate row.
#' @export
#'
#' @examples
#' \dontrun{
#' embed(iris, c("Sepal.Length","Sepal.Width"), "Sepal.Stuff", embedded_names="Sepal.Variable")
#' }
embed <- function(x, vars, target, embedded_names = NULL) {
  x |>
    # Insert a temporary id for when we nest information (in case of dups)
    dplyr::mutate(tmp_rownumber = dplyr::row_number()) |>
    # Pivot all of vars into separate rows with same data otherwise.
    tidyr::pivot_longer(cols = tidyselect::all_of(vars), names_to = embedded_names, values_to = target) |>
    # Nest collapses these separate rows into a nested tibble called target
    tidyr::nest({{target}} := c(target, embedded_names)) |>
    # Remove the temporary id
    dplyr::select(-.data$tmp_rownumber)
}
