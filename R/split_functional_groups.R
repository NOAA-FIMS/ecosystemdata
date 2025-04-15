utils::globalVariables(c("functional_group", "species", "group"))
#' Split the functional groups into species and group
#'
#' Split strings containing functional groups into species and group names.
#' Functional groups can contain age ranges, plus groups, or sub categories,
#' e.g., juvenile. This information is extracted from the functional group name
#' and returned as the group name.
#'
#' @param x A character vector of functional group names. This is most often
#'   created by running [unique()] on the functional group column of the data.
#' @return
#' A tibble with the following three columns: functional_group, species, and
#' group, is returned.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' # A hypothetical example of functional groups
#' split_functional_groups(
#'   c("spiny dogfish 0", "spiny dogfish 1-2", "spiny dogfish +")
#' )
#' split_functional_groups(c("spiny dogfish", "spiny dogfish juvenile"))
#' data("ewe_nwatlantic_base", package = "ecosystemdata")
#' split_functional_groups(unique(ewe_nwatlantic_base[["functional_group"]]))
split_functional_groups <- function(x) {
  if (!inherits(x, what = "character")) {
    cli::cli_abort("{.var x} must be a character vector not a {class(x)}.")
  }
  if (length(x) != length(unique(x))) {
    cli::cli_abort("{.var x} must be unique functional groups.")
  }
  test_for_characters <- grepl("[^a-zA-Z0-9 _\\+-]", x)
  if (any(test_for_characters)) {
    cli::cli_abort(c(
      x = "{.var x} cannot contain non alpha-numerics except +, -, or _.",
      i = "The following functional groups contain non alpha-numerics:",
      "{.var {x[test_for_characters]}}"
    ))
  }

  groups <- x |>
    # Split camelCase and PascalCase into words separated by a space
    gsub(pattern = "([a-z])([[:upper:]])", replacement = "\\1 \\2") |>
    # Split words followed by a number into words separated by a space
    gsub(pattern = "([a-z])([[:digit:]])", replacement = "\\1 \\2") |>
    # split digits separated by an underscore into digits separated by a dash
    # e.g., 1_2 becomes 1-2
    gsub(pattern = "([[:digit:]])_([[:digit:]])", replacement = "\\1-\\2")

  # Search for groups with age, age range, or plus group
  # - and + are special characters and must be escaped with \\
  groups_with_ages <- groups[grepl("[0-9\\-\\+]+", groups)]

  # Search for groups with two words and no age
  groups_with_alpha <- groups[grepl("[a-zA-Z]+ [a-zA-Z]+$", groups)]

  # Search for groups that should be left as is
  groups_with_nothing <- groups[grepl("^[a-zA-Z]+$", groups)]

  # Some of the groups_with_alpha are just two-word groups that belong in the
  # groups_with_nothing group. For example, "spiny dogfish"
  first_word <- gsub(" [a-zA-Z]+", "", groups_with_alpha)
  # Find which groups
  move_to_groups_with_nothing <- grepl(
    pattern = paste0(
      first_word[which(duplicated(first_word))],
      " ",
      collapse = "|"
    ),
    groups_with_alpha
  )
  groups_with_nothing <- c(
    groups_with_nothing,
    groups_with_alpha[!move_to_groups_with_nothing]
  )
  groups_with_alpha <- groups_with_alpha[move_to_groups_with_nothing]

  # Return a 3-column tibble after separating groups from functional groups
  tibble::as_tibble_col(
    c(groups_with_ages, groups_with_alpha),
    column_name = "functional_group"
  ) |>
    tidyr::separate_wider_regex(
      col = functional_group,
      pattern = c(species = "[a-zA-Z ]+", group = " [a-zA-Z]+$| [0-9\\-\\+]+"),
      cols_remove = FALSE,
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      group = gsub(" ", "", group)
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        functional_group = groups_with_nothing,
        species = groups_with_nothing
      )
    ) |>
    dplyr::select(
      functional_group, species, group
    ) |>
    # ensure the order of the data frame matches the input x
    dplyr::full_join(
      x = data.frame(functional_group = groups),
      by = "functional_group"
    ) |>
    # Change back to the input instead of the pretty groups object
    dplyr::mutate(
      functional_group = x[match(functional_group, groups)]
    ) |>
    tibble::as_tibble()
}
