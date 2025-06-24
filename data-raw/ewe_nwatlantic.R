# code to prepare `ewe_nwatlantic` datasets

# TODO: remove this line after testing the update-data-r GHA
# The functional groups included in the model
functional_groups <- get_functional_groups(
  file_path = fs::path(
    "data-raw", "ewe_nwatlantic", "base_run", "basic_estimates.csv"
  )
)

ewe_nwatlantic_base <- load_model(
  directory = fs::path("data-raw", "ewe_nwatlantic", "base_run"),
  type = "ewe",
  functional_groups = functional_groups
)
ewe_nwatlantic_env <- load_model(
  directory = fs::path("data-raw", "ewe_nwatlantic", "environmental_link"),
  type = "ewe",
  functional_groups = functional_groups
)

usethis::use_data(ewe_nwatlantic_base, overwrite = TRUE)
usethis::use_data(ewe_nwatlantic_env, overwrite = TRUE)
