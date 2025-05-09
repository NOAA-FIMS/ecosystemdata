% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_functional_groups.R
\name{split_functional_groups}
\alias{split_functional_groups}
\title{Split the functional groups into species and group}
\usage{
split_functional_groups(x)
}
\arguments{
\item{x}{A character vector of functional group names. This is most often
created by running \code{\link[=unique]{unique()}} on the functional group column of the data.}
}
\value{
A tibble with the following three columns: functional_group, species, and
group, is returned.
}
\description{
Split strings containing functional groups into species and group names.
Functional groups can contain age ranges, plus groups, or sub categories,
e.g., juvenile. This information is extracted from the functional group name
and returned as the group name.
}
\examples{
# A hypothetical example of functional groups
split_functional_groups(
  c("spiny dogfish 0", "spiny dogfish 1-2", "spiny dogfish +")
)
split_functional_groups(c("spiny dogfish", "spiny dogfish juvenile"))
data("ewe_nwatlantic_base", package = "ecosystemdata")
split_functional_groups(unique(ewe_nwatlantic_base[["functional_group"]]))
}
\author{
Kelli F. Johnson
}
