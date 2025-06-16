# Path to the basic_estimates.csv file in the package
base_run_path <- fs::path(
  system.file("extdata", package = "ecosystemdata"),
  "ewe_nwatlantic", "base_run", "basic_estimates.csv"
)
# Scan the file to read its contents
temp <- scan(base_run_path, what = "", sep = "\n", quiet = TRUE)

# Check if the second element of temp contains "oid" as part of the string.
# If so, run the system command to install git-lfs and pull the data
# Code for Linux system
if (grepl("oid", temp[2])) {
  # Check if the system is Linux
  if (Sys.info()["sysname"] != "Linux") {
    system(
      "sudo apt update; sudo apt install git-lfs; git lfs install; git lfs pull",
      intern = TRUE
    )
  } 

  # Check if the system is macOS
  if (Sys.info()["sysname"] == "Darwin") {
    system(
      "brew install git-lfs; git lfs install; git lfs pull",
      intern = TRUE
    )
  }

  if (Sys.info()["sysname"] == "Windows") {
    system("winget install -e --id GitHub.GitLFS; git lfs install; git lfs pull")
  }
}
