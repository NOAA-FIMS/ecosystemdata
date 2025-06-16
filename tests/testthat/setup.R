# Path to the basic_estimates.csv file in the package
base_run_path <- fs::path(
  system.file("extdata", package = "ecosystemdata"),
  "ewe_nwatlantic", "base_run", "basic_estimates.csv"
)
# Scan the file to read its contents
temp <- scan(base_run_path, what = "", sep = "\n", quiet = TRUE)

# Check if the second element of temp contains "oid" as part of the string.
# If so, run the system command to install git-lfs and pull the data
if (grepl("oid", temp[2])) {
  sysname <- Sys.info()[["sysname"]]
  switch(
    sysname,
    "Linux" = system("sudo apt update; sudo apt install -y git-lfs; git lfs install; git lfs pull", intern = TRUE),
    "Darwin" = system("brew install git-lfs; git lfs install; git lfs pull", intern = TRUE),
    "Windows" = system("winget install -e --id GitHub.GitLFS; git lfs install; git lfs pull", intern = TRUE)
  )
}