#' @export
print.rpgcli_summary <- function(pop_gen_summary) {

  if (!("rpgcli_summary" %in% class(pop_gen_summary))) {
    stop("Expecting generation summary")
  }

  if (is.null(pop_gen_summary$msg)) {

    cat("Generated", pop_gen_summary$n_individuals, "individuals")
    cat("\n")
    cat("Next generation can commence at", pop_gen_summary$next_gen_start_from)

  } else {

    message(pop_gen_summary$msg)

  }


  return(invisible(pop_gen_summary))
}

#' @export
print.rpgcli <- function(pop_gen_out, ...){

  print(summary(pop_gen_out))

}

#' @export
summary.rpgcli <- function(pop_gen_out, ...) {

  if (!("rpgcli" %in% class(pop_gen_out))) {
    stop("Expecting generation output")
  }

  err <- pop_gen_out$err

  if (!is.null(err)) {

    msg <- character()

    if ("simpleError" %in% class(pop_gen_out$err)) {

      msg <- err$message

    } else if (is.list(err)) {

      msg <- paste0(c("Service problem or invalid input parameters:\n", err), collapse = "\n")

    } else {

      msg <- as.character(err)

    }

    return(structure(list(msg = msg), class = "rpgcli_summary"))

  }

  individuals_node <- xml2::xml_find_first(pop_gen_out$raw, "./RetrieveResultsResult/Individuals")
  n_individuals <- xml2::xml_length(individuals_node)
  next_gen_start_from <- sprintf(
    "%02d:%02d:%02d",
    as.integer(lubridate::hour(pop_gen_out$wait_until)),
    as.integer(lubridate::minute(pop_gen_out$wait_until)),
    as.integer(round(lubridate::second(pop_gen_out$wait_until)))
    )

  pop_gen_summary <- structure(list(
    n_individuals = n_individuals,
    next_gen_start_from = next_gen_start_from
    ), class = "rpgcli_summary")

  return(pop_gen_summary)

}
