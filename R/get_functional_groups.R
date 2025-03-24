utils::globalVariables(c("V1", "V2"))

#' Get the functional groups from an EwE file
#'
#' @description
#' Functional group names are useful for the column names of output data from
#' an EeW model, and thus, this function is a way to get them automatically
#' from the EwE output. The functional group names come from the basic
#' estimates file.
#'
#' @param file_path The path to the EwE file.
#' @return
#' A vector of functional group names.
#' @export
#' @examples
#' get_functional_groups(
#'   file_path = fs::path(
#'     system.file("extdata", package = "ecosystemdata"),
#'     "ewe_nwatlantic", "base_run", "basic_estimates.csv"
#'   )
#' )
get_functional_groups <- function(file_path) {
  # Load the EwE data file and extract the data
  temp <- scan(file_path, what = "", sep = "\n", quiet = TRUE)
  # Extract the data
  utils::read.table(
    text = as.character(temp[-1]),
    sep = ","
  ) |> 
    # Extract non-NA rows from the first column
    dplyr::filter(!is.na(V1)) |>
    # Pull the names of the functional groups
    dplyr::pull(V2)
}
