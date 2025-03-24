#' Find the number of rows to skip when reading in an EWE file
#' @noRd
read_n_skip <- function(file_path, keyword = "timestep") {
  # Load the data file
  temp <- scan(file_path, what = "", sep = "\n")
  # For example with the default
  # Find the line containing "timestep\\group" to skip
  skip_nrows <- which(grepl(keyword, temp))
  # Extract the data
  data <- temp[-c(1:skip_nrows)]
  return(data)
}
