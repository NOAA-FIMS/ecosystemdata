#' Reshape environmental data and return it as a tibble.
#'
#' @param index_name A string indicating the name of the environmental index.
#' @param values A numeric vector of values corresponding to the environmental index.
#' @param years An integer vector of years corresponding to the values.
#' @param lag An integer indicating the lag applied to the environmental index.
#' @param impacted_group A string indicating the group impacted by the environmental index.
#'
#' @return A tibble containing the reshaped environmental data.
#'
#' @examples
#' data <- reshape_environment_data(
#'   index_name = "index1",
#'   values = rnorm(24, mean = 1, sd = 0.2),
#'   years = 1984:1985,
#'   lag = 1,
#'   impacted_group = "menhaden 0"
#' ) |>
#'   dplyr::bind_rows(
#'     reshape_environment_data(
#'       index_name = "index2",
#'       values = rnorm(24, mean = 1, sd = 0.2),
#'       years = 1985:1986,
#'       lag = 0,
#'       impacted_group = "menhaden 1"
#'     )
#'   )
#'
#' @export
reshape_environment_data <- function(index_name, values, years, lag, impacted_group) {

  # Validate input types
  if (!is.character(index_name) || length(index_name) != 1) {
    cli::cli_abort("{.arg index_name} must be a single character string.")
  }
  if (!is.numeric(values)) {
    cli::cli_abort("{.arg values} must be a numeric vector.")
  }
  if (!is.integer(years)) {
    cli::cli_abort("{.arg years} must be an integer vector.")
  }
  if (!is.numeric(lag) || length(lag) != 1) {
    cli::cli_abort("{.arg lag} must be a single numeric value.")
  }
  if (!is.character(impacted_group) || length(impacted_group) != 1) {
    cli::cli_abort("{.arg impacted_group} must be a single character string.")
  }

  # Check that the length of values matches the number of expected time steps (monthly data)
  expected_length <- length(years) * 12
  if (length(values) != expected_length) {
    cli::cli_abort(c(
      "Length mismatch between {.arg values} and expected time steps.",
      "x" = "{.arg values} has length {length(values)}.",
      "i" = "Expected length is {expected_length}, based on {length(years)} year(s) times 12 months."
    ))
  }

  # Split the impacted group into species and group components
  split_group <- split_functional_groups(impacted_group)

  # Create output tibble
  out <- dplyr::tibble(
    index_name = as.character(index_name),
    value = as.numeric(values),
    year = as.integer(rep(years, each = 12)),
    month = as.integer(rep(1:12, times = length(years))),
    lag = as.integer(lag),
    impacted_group = as.character(impacted_group),
    species = as.character(split_group[["species"]]),
    group = as.character(split_group[["group"]])
  )
}