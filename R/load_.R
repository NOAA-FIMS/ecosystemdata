utils::globalVariables(c("timestep", "group", "fleet", "month", "type", "year"))

#' Load in EwE monthly output data
#'
#' @description
#' The function loads in Ecopath with Ecosim (EwE) monthly output data from a CSV file.
#'
#' @param file_path The path to the CSV file containing the EwE output data.
#' @param model_years A vector of years corresponding to the model years. TODO: this can be removed
#' after having a utility function to extract model years globally from annual data
#' @param functional_groups A vector of names of the functional groups in the model.
#'
#' @export
#' @examples
#' \dontrun{
#' # The following example is not run by default because these files are only
#' # included in the GitHub clone of the repository and not in the package data
#' load_csv_ewe(
#'   file_path = fs::path(
#'     "data-raw", "ewe_nwatlantic", "base_run", "biomass_monthly.csv"
#'   ),
#'   model_years = 1985:2017,
#'   functional_groups = get_functional_groups(
#'     file_path = fs::path(
#'       base_run_dir, "basic_estimates.csv"
#'     )
#'   )
#' )
#' }
# TODO: double check that average of monthly data matches annual data for more than just biomass
load_csv_ewe <- function(file_path, model_years, functional_groups) {
  # Load the EwE data file and extract the data
  data <- read_n_skip(file_path)
  if (
    NCOL(data) == NROW(functional_groups) + 1 &&
      colnames(data)[2] == "X1"
  ) {
    colnames(data) <- c("timestep", functional_groups[["functional_group"]])
    # Read the data into a data frame and add year and month columns
    out <- data |>
      dplyr::mutate(
        year = rep(model_years, each = 12),
        month = rep(1:12, times = length(model_years))
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(functional_groups[["functional_group"]]),
        names_to = "functional_group",
        values_to = "value"
      ) |>
      dplyr::select(-timestep)
  } else {
    functional_groups_vector <- functional_groups[["functional_group"]]
    out <- data |>
      dplyr::rename(
        timestep = dplyr::starts_with("timestep")
      ) |>
      dplyr::mutate(
        reference = (timestep %/% 12) + 1,
        year = model_years[(timestep %/% 12) + 1],
        month = timestep %% 12,
        month = ifelse(month == 0, 12, month)
      ) |>
      dplyr::group_by(fleet, group) |>
      dplyr::mutate(
        year = rep(model_years, each = 12)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        functional_group = functional_groups_vector[group]
      ) |>
      dplyr::select(-group, -timestep)
  }
  out |>
    dplyr::mutate(
      type = get_type_from_file(file_path)
    ) |>
    dplyr::left_join(
      functional_groups,
      by = "functional_group"
    ) |>
    dplyr::select(type, year, month, dplyr::everything())
}

#' Load an ecosystem model
#'
#' Load the necessary files from an ecosystem model and return a single, long
#' data frame of information.
#'
#' @param ... Arguments that are passed onto lower level `load_model_()*`
#'   functions. Such as those needed for `load_model_ewe()`, which are
#'   `directory` and `functional_groups`.
#' @param type A string indicating which type of model data you want to load.
#'   The default is `r toString(formals(load_model)[["type"]][2])`. Strings
#'   should be all lower case text.
#'
#' @export
#' @return
#' A tibble is returned that matches the structure of [ewe_nwatlantic_base].
#'
load_model <- function(..., type = c("ewe", "atlantis")) {
  type <- tolower(type)
  type <- rlang::arg_match(type)
  if (type == "ewe") {
    model <- load_model_ewe(...)
  } else {
    cli::cli_abort(
      "{type} is not yet configured for {.fn load_model}"
    )
  }
  return(model)
}

load_model_ewe <- function(directory, functional_groups) {
  # Determine the number of years in the model
  years <- read_n_skip(
    file_path = fs::path(directory, "biomass_annual.csv"),
    keyword = "year"
  ) |>
    dplyr::pull(1)

  # Load monthly data
  terms <- c("biomass", "catch", "landings", "mortality", "weight")
  monthly_files <- fs::dir_ls(
    path = directory,
    regexp = paste(.Platform[["file.sep"]], terms, "_monthly", sep = "", collapse = "|"),
    type = "file"
  )

  data_monthly <- purrr::map_df(
    monthly_files,
    load_csv_ewe,
    functional_groups = functional_groups,
    model_years = years,
    .id = "file_name"
  )

  # TODO: build up this data set
  data_output <- data_monthly |>
    tibble::as_tibble()
  return(data_output)
}

get_type_from_file <- function(file_path) {
  base <- basename(file_path)
  gsub("_monthly|_annual|\\.csv", "", base)
}
