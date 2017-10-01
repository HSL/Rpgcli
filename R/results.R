#' Query Output
#'
#' Returns TRUE if the call succeeded in generating population data.
#'
#' @param pop_gen_out Structure returned from \code{\link{generate_population}}
#' @return TRUE if pop_gen_out contains population data, FALSE otherwise
#' @export

generate_succeeded <- function(pop_gen_out) {

  if (!("rpgcli" %in% class(pop_gen_out))) {
    stop("Expecting generation output")
  }

  succeeded <- !is.null(pop_gen_out$raw)

  return(succeeded)

}

#' Query Output
#'
#' Returns TRUE if the call failed to generate population data.
#'
#' @param pop_gen_out Structure returned from \code{\link{generate_population}}
#' @return TRUE if pop_gen_out does not contain population data, FALSE otherwise
#' @export

generate_failed <- function(pop_gen_out) {

  return(!generate_succeeded(pop_gen_out))

}

#' Retrieve Results
#'
#' Parse the service output for population data.
#'
#' When converting numerical data, any NAs produced are not accompanied by a diagnostic message.
#'
#' @param pop_gen_out Structure returned from \code{\link{generate_population}}
#' @param convert Whether to convert service output to numerical data, and convert to a data frame
#' @return If convert is TRUE, a data frame; otherwise, a matrix
#' @export

extract_individuals <- function(pop_gen_out, convert = TRUE) {

  if (!("rpgcli" %in% class(pop_gen_out))) {
    stop("Expecting generation output")
  }

  if (is.null(pop_gen_out$raw)) {
    stop("No data")
  }

  individuals_node <- xml2::xml_find_first(pop_gen_out$raw, "./RetrieveResultsResult/Individuals")
  n_individuals <- xml2::xml_length(individuals_node)

  if (1 > n_individuals) {
    stop("Zero data")
  }

  column_names <- list()

  individual_nodes <- xml2::xml_children(individuals_node)
  individual <- individual_nodes[[1]]

  columns <- xml2::xml_children(individual)
  n_columns <- length(columns)

  for (i in 1:n_columns) {

    column <- columns[[i]]
    column_name <- xml2::xml_name(column)

    if (column_name == "Enzymes") {

      enzymes <- xml2::xml_find_all(individual, "Enzymes/Enzyme/Enzyme")

      n_enzymes <- length(enzymes)

      if (0 < n_enzymes) {

        for (j in 1:n_enzymes) {
          instance <- enzymes[[j]]
          name_node <- xml2::xml_find_first(instance, "Name")
          column_names <- c(column_names, xml2::xml_text(name_node))
        }

      }

      mppgl <- xml2::xml_find_all(individual, "Enzymes/Mppgl")
      if (!is.na(mppgl)) column_names <- c(column_names, "Mppgl")

    } else if (column_name == "Tissues") {

      tissues <- xml2::xml_find_all(individual, "Tissues/Tissue")
      n_tissues <- length(tissues)

      if (0 < n_tissues) {

        for (j in 1:n_tissues) {

          tissue <- tissues[[j]]
          type_node <- xml2::xml_find_first(tissue, "Type")
          type <- xml2::xml_text(type_node)
          type <- gsub(" ", ".", type)
          column_names <- c(column_names, paste(type, "mass", sep = "."), paste(type, "flow", sep = "."))
        }

      }

    } else {

      column_names <- c(column_names, column_name)

    }
  }

  column_data <- lapply(column_names, function(n) vector("list", n_individuals))

  for (i in 1:n_individuals) {

    individual <- individual_nodes[[i]]

    columns <- xml2::xml_children(individual)
    n_columns <- length(columns)

    column_index <- 1

    for (j in 1:n_columns) {

      column <- columns[[j]]
      column_name <- xml2::xml_name(column)

      if (column_name == "Enzymes") {

        enzymes <- xml2::xml_find_all(individual, "Enzymes/Enzyme/Enzyme")

        n_enzymes <- length(enzymes)

        if (0 < n_enzymes) {

          for (k in 1:n_enzymes) {
            instance <- enzymes[[k]]
            rate_node <- xml2::xml_find_first(instance, "InVivoEnzymeRate")
            column_data[[column_index]][[i]] <- xml2::xml_text(rate_node)
            column_index <- column_index + 1
          }

        }

        mppgl <- xml2::xml_find_all(individual, "Enzymes/Mppgl")
        if (!is.na(mppgl)) {

          column_data[[column_index]][[i]] <- xml2::xml_text(mppgl)
          column_index <- column_index + 1

        }

      } else if (column_name == "Tissues") {

        tissues <- xml2::xml_find_all(individual, "Tissues/Tissue")
        n_tissues <- length(tissues)

        if (0 < n_tissues) {

          for (k in 1:n_tissues) {

            tissue <- tissues[[k]]

            mass_node <- xml2::xml_find_first(tissue, "Mass")
            column_data[[column_index]][[i]] <- xml2::xml_text(mass_node)
            column_index <- column_index + 1

            flow_node <- xml2::xml_find_first(tissue, "Flow")
            column_data[[column_index]][[i]] <- xml2::xml_text(flow_node)
            column_index <- column_index + 1

          }

        }

      } else {

        column_data[[column_index]][[i]] <- xml2::xml_text(column)
        column_index <- column_index + 1

      }

    }

  }

  names(column_data) <- column_names

  if (!convert) {

    as_matrix <- matrix(unlist(column_data), ncol = length(column_data), byrow = F)
    colnames(as_matrix) <- names(column_data)

    return(as_matrix)

  }

  for (name in column_names) {

    if (name == "Ethnicity" || name == "Sex") {

      column_data[[name]] <- as.factor(unlist(column_data[[name]]))

    } else {

      suppressWarnings(column_data[[name]] <- as.numeric(column_data[[name]]))

    }

  }

  extracted <- do.call(data.frame, column_data)

  return(extracted)

}

#' Retrieve Results
#'
#' Parse the service output for stats relating to male individuals.
#'
#' When converting numerical data, any NAs produced are not accompanied by a diagnostic message.
#'
#' @param pop_gen_out Structure returned from \code{\link{generate_population}}
#' @param convert Whether to convert service output to numerical data, and convert to a data frame
#' @return If convert is TRUE, a data frame; otherwise, a matrix
#' @export

extract_stats_male <- function(pop_gen_out, convert = TRUE) {

  do_extract_stats(pop_gen_out, convert, "Male")

}

#' Retrieve Results
#'
#' Parse the service output for stats relating to female individuals.
#'
#' When converting numerical data, any NAs produced are not accompanied by a diagnostic message.
#'
#' @param pop_gen_out Structure returned from \code{\link{generate_population}}
#' @param convert Whether to convert service output to numerical data, and convert to a data frame
#' @return If convert is TRUE, a data frame; otherwise, a matrix
#' @export

extract_stats_female <- function(pop_gen_out, convert = TRUE) {

  do_extract_stats(pop_gen_out, convert, "Female")

}

do_extract_stats <- function(pop_gen_out, convert, sex) {

  if (!("rpgcli" %in% class(pop_gen_out))) {
    stop("Expecting generation output")
  }

  if (is.null(pop_gen_out$raw)) {
    stop("No data")
  }

  xpath <- paste0("./RetrieveResultsResult/Summary/", sex)

  sex_node <- xml2::xml_find_first(pop_gen_out$raw, xpath)

  flow_stats_nodes <- xml2::xml_find_all(sex_node, "./Flow/Stats")

  rate_stats_nodes <- xml2::xml_find_all(sex_node, "./InVivoEnzymeRate/Stats")

  mass_stats_nodes <- xml2::xml_find_all(sex_node, "./Mass/Stats")

  mppgl_stats_nodes <- xml2::xml_find_all(sex_node, "./Mppgl/Stats")

  all_stats_nodes <- list(flow_stats_nodes, rate_stats_nodes, mass_stats_nodes, mppgl_stats_nodes)

  n_columns <- sum(sapply(all_stats_nodes, length))

  column_names <- vector("list", length = n_columns)

  column_name_suffices <- c(
    rep(".flow", length(flow_stats_nodes)),
    rep("", length(rate_stats_nodes)),
    rep(".mass", length(mass_stats_nodes)),
    rep("", length(mppgl_stats_nodes))
  )

  if (0 == length(flow_stats_nodes)) stop("No flow stats present")

  stats_row_names <- lapply(xml2::xml_children(flow_stats_nodes[[1]]), xml2::xml_name)
  stats_row_names[[which(stats_row_names == "Name")]] <- NULL

  n_stats <- length(stats_row_names)

  column_data <- lapply(column_names, function(n) vector("list", n_stats))

  name_row_index_skip <- 0

  for (row in 1:(n_stats+1)) {

    column <- 1

    for (i in 1:4) {

      stats_nodes <- all_stats_nodes[[i]]
      n_stats_nodes <- length(stats_nodes)

      for (j in 1:n_stats_nodes) {

        stats_node <- stats_nodes[[j]]

        nodes <- xml2::xml_children(stats_node)

        node <- nodes[[row]]

        name <- xml2::xml_name(node)

        if (name == "Name") {

          column_name <- xml2::xml_text(node)
          column_name <- gsub(" ", ".", column_name)
          column_name <- paste(column_name, column_name_suffices[column], sep = "")
          column_names[[column]] <- column_name
          name_row_index_skip <- 1

        } else {

          column_data[[column]][[row - name_row_index_skip]] <- xml2::xml_text(node)

        }

        column <- column + 1

      }

    }

  }

  names(column_data) <- column_names

  if (!convert) {

    as_matrix <- matrix(unlist(column_data), ncol = length(column_data), byrow = F)
    colnames(as_matrix) <- names(column_data)
    rownames(as_matrix) <- stats_row_names

    return(as_matrix)

  }

  for (name in column_names) {

    suppressWarnings(column_data[[name]] <- as.numeric(column_data[[name]]))

  }

  extracted <- do.call(data.frame, column_data)
  rownames(extracted) <- stats_row_names

  return(extracted)

}
