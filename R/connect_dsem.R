devtools::load_all()
# Load EwE operating model (OM) output data: the tibble includes
# biomass, catch, landings, mortality, and weight for each 
# functional group in the EwE model.
data_ewe <- ecosystemdata::ewe_nwatlantic_env

# Load environmental data (raw) used in the EwE OM.
# read in Atlantic Multidecadal Oscillation (AMO) data
# and Palmer Drought Severity Index (PDSI) data.
amo_path <- fs::path(
  system.file("extdata", package = "ecosystemdata"),
  "ewe_nwatlantic", "environmental_link", "amo_lag1.csv"
)
amo_data <- read.csv(amo_path)

pdsi_path <- fs::path(
  system.file("extdata", package = "ecosystemdata"),
  "ewe_nwatlantic", "environmental_link", "pdsi_lag0.csv"
)
pdsi_data <- read.csv(pdsi_path)

# Use load_csv_environmental_data() to reshape the environmental data
data_env <- ecosystemdata::load_csv_environmental_data(
    file_path = amo_path,
    lag_months = 12,
    impacted_group = "menhaden 0"
  ) |>
    dplyr::bind_rows(
      load_csv_environmental_data(
        file_path = pdsi_path,
        lag_months = 0,
        impacted_group = "phytoplankton"
      )
    )

  # fit a dsem model - this is an example 
  
 sem <- "
  # Link, lag, param_name
  
  # internal data relationship
  annual_ppd -> annual_ppd, 1, ar_ppd
  mean_zoo_anom -> mean_zoo_anom, 1, ar_zoo 
  fall_forage -> fall_forage, 1, ar_for
  mean_condition -> mean_condition, 1, ar_cond 

  # causal links
  annual_ppd -> mean_zoo_anom, 0, pp_zoo
  mean_zoo_anom -> fall_forage, 0, zoo_ff
  fall_forage -> mean_condition, 0, ff_mc
  "

  # fit model
  fit = 
   dsem( 
    sem = sem,
    tsdata = df_na_sc,
    control)


