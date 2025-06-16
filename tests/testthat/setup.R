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
  # Check if git LFS is installed using the system command
  git_lfs_installed <- system(
    "git lfs version", 
    intern = TRUE, 
    ignore.stdout = TRUE, 
    ignore.stderr = TRUE
  )

  if (length(git_lfs_installed) == 0) {
    # If git LFS is not installed, install it
    sysname <- Sys.info()[["sysname"]]
    switch(
      sysname,
      "Linux" = system("sudo apt update; sudo apt install -y git-lfs; git lfs install", intern = TRUE),
      "Darwin" = system("brew install git-lfs; git lfs install", intern = TRUE),
      "Windows" = system("winget install -e --id GitHub.GitLFS; git lfs install", intern = TRUE)
    )
  } else {
    # If git LFS is installed, pull the data
    system("git lfs pull", intern = TRUE)
  }
}