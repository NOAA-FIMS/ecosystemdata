#' Load in EwE monthly output data
#' 
#' @description
#' The function loads in Ecopath with Ecosim (EwE) monthly output data from a CSV file.
#' 
#' @param file_path The path to the CSV file containing the EwE output data.
#' @param model_years A vector of years corresponding to the model years. TODO: this can be removed 
#' after havgina a utility function to extract model years globally from annual data
#' @param functional_groups A vector of names of the functional groups in the model.
#' 
#' @export 
#' @examples
#' load_csv_ewe(
#'   file_path = fs::path("data-raw", "ewe_nwatlantic", "base_run", "biomass_monthly.csv"),
#'   model_years = 1985:2017, 
#'   functional_groups = c(
#'     "StripedBass0",
#'     "StripedBass2_5",
#'     "StripedBass6",
#'     "AtlanticMenhaden0",
#'     "AtlanticMenhaden1",
#'     "AtlanticMenhaden2",
#'     "AtlanticMenhaden3",
#'     "AtlanticMenhaden4",
#'     "AtlanticMenhaden5",
#'     "AtlanticMenhaden6",
#'     "SpinyDogfish",
#'     "BluefishJuvenile",
#'     "BluefishAdult",
#'     "WeakfishJuvenile",
#'     "WeakfishAdult",
#'     "AtlanticHerring0_1",
#'     "AtlanticHerring2",
#'     "Anchovies",
#'     "Benthos",
#'     "Zooplankton",
#'     "Phytoplankton",
#'     "Detritus"
#'   ) 
#' )
# TODO: double check that average of montly data matches annual data for more than just biomass

load_csv_ewe <- function(file_path, model_years, functional_groups) {
  # Load the EwE data file and extract the data
  data <- read_n_skip(file_path)
  # Read the data into a data frame and add year and month columns
  data <- read.table(
    text = as.character(data),
    sep = ",",
    col.names = c("timestep", functional_groups)
  ) |> 
    dplyr::mutate(
      year = rep(model_years, each = 12),
      month = rep(1:12, times = length(model_years))
    ) |>
    dplyr::select(year, month, everything())
    return(data)
}

#' Load an ecosystem model
load_model <- function(..., type = c("ewe", "atlantis")) {
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
    file_path = fs::path(directory, "biomass_annual.csv")
  ) |>
    dplyr::pull(year.group)

  # Load monthly data
  terms <- c("biomass", "catch", "landings")
  monthly_files <- fs::dir_ls(
    path = directory,
    regexp = paste(.Platform[["file.sep"]], terms, "_monthly", sep = "", collapse = "|"),
    type = "file"
  )
  data_monthly <- purrr::map_df(
    monthly_files,
    load_csv_ewe,
    functional_groups = functional_groups
    .id = "file_name"
  )
  
  # TODO: build up this data set
  data_output <- data_monthly
  return(data_output)
}
