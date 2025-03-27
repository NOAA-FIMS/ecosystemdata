# code to prepare `ewe_nwatlantic` datasets

# The functional groups included in the model
functional_groups <- c(
  "StripedBass0", "StripedBass2_5", "StripedBass6",
  "AtlanticMenhaden0", "AtlanticMenhaden1", "AtlanticMenhaden2", "AtlanticMenhaden3",
  "AtlanticMenhaden4", "AtlanticMenhaden5", "AtlanticMenhaden6",
  "SpinyDogfish",
  "BluefishJuvenile",
  "BluefishAdult",
  "WeakfishJuvenile",
  "WeakfishAdult",
  "AtlanticHerring0_1", "AtlanticHerring2",
  "Anchovies",
  "Benthos",
  "Zooplankton",
  "Phytoplankton",
  "Detritus"
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
