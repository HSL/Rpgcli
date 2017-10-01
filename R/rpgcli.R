#' PopGen Client
#'
#' A set of R functions for calling the \href{http://xnet.hsl.gov.uk/popgen}{PopGen} web service and for extracting data from the response.
#'
#' @details
#' Pay a visit to \href{http://xnet.hsl.gov.uk/popgen}{PopGen} and review the inputs form before
#' using this package for programmatic access.
#'
#' Refer to the service's \href{http://xnet.hsl.gov.uk/Popgen/svc/pg.svc?xsd=xsd2}{inputs schema} for concise
#' coverage of the input requirements.
#'
#' @docType package
#' @name Rpgcli
#' @references
#' McNally K., Cotton R., Hogg A., Loizou G. (2014). PopGen: a virtual human population generator. \emph{Toxicology} 315 70-85. 10.1016/j.tox.2013.07.009
#' \href{https://doi.org/10.1016/j.tox.2013.07.009}{[Cross ref]}
#'
#' @examples
#' # this example samples data from all four datasets
#' datasets <- c("P3M", "ICRP", "HSE", "NDNS")
#' n_datasets <- length(datasets)

#' data <- lapply(1:n_datasets, function(i) {

#'   dataset <- datasets[i]

#'   parameters <- list(
#'     Dataset = dataset,
#'     PopGenUserName = "kevin.soandso@some.sci.org",
#'     PopulationSize = 100
#'   )

#'   if (dataset == "NDNS") {

#'     # See https://discover.ukdataservice.ac.uk/series/?sn=2000033#abstract

#'     parameters$AgeLower <- 2
#'     parameters$AgeUpper <- 4

#'     parameters$BodyMassIndexLower <- 10

#'     parameters$HeightLower <- 80
#'     parameters$HeightUpper <- 120

#'   }

#'   parameters <- apply_defaults(parameters)

#'   cat(paste("Sampling ", dataset, "\n", sep = ""))

#'   # call service with no activity feedback
#'   pop_gen_out <- generate_population(parameters, silent = T)

#'   if (generate_failed(pop_gen_out)) {

#'     # show error messages now
#'     print(summary(pop_gen_out))
#'     stop()

#'   }

#'   if (i < n_datasets) {

#'     # service is throttled - must pause here
#'     cat("Waiting for service to resume...\n")
#'     Sys.sleep(pop_gen_out$wait_for)

#'   }

#'   individuals <- extract_individuals(pop_gen_out)
#'   return(individuals)

#' })

#' names(data) <- datasets
NULL

#' Call PopGen Web Service
#'
#' Sample a population dataset using PopGen.
#'
#' Use of this API requires an active account on the \href{http://xnet.hsl.gov.uk/popgen}{PopGen} web site.
#'
#' The returned object consists either of an XML document fragment containing the generated population, or one or more error messages.
#'
#' @param parameters Inputs that describe the target population
#' @param silent Whether or not to print activity messages during the call
#' @return A structure containing data returned by the PopGen service.
#' @seealso Query outcome of service call with \code{\link{generate_succeeded}}. Retrieve results with \code{\link{extract_individuals}}.
#' @export
#' @examples
#' # Set PopGen account name
#' parameters <- list(
#'   PopGenUserName = "kevin.soandso@some.sci.org",
#'   PopulationSize = 100
#'   )
#'
#' # Add typical values for other inputs
#' parameters <- apply_defaults(parameters)
#'
#' # Call web service
#' pop_gen_out <- generate_population(parameters)

generate_population <- function(parameters, silent = FALSE) {

  pop_gen_out <- tryCatch({

    offerIdentifier <- paste0("urn:uuid:", uuid::UUIDgenerate())

    acceptIdentifier <- create_sequence(offerIdentifier, silent)

    messageNumber <- 1

    apply_defaults(parameters)

    validation_errors <- generate(acceptIdentifier, messageNumber, parameters, silent)

    if (0 < length(validation_errors)) {

      pop_gen_out <- structure(list(
        inputs = parameters,
        err = validation_errors,
        wait = 0
        ), class = "rpgcli")

      return(pop_gen_out)

    }

    messageNumber <- wait_for_completion(acceptIdentifier, messageNumber, offerIdentifier, silent)

    retrieveResultsResult <- retrieve_results(acceptIdentifier, messageNumber, offerIdentifier, silent)

    messageNumber <- messageNumber + 1
    secs_to_wait <- clean_up(acceptIdentifier, messageNumber, offerIdentifier, silent)

    messageNumber <- messageNumber + 1
    last_message(acceptIdentifier, messageNumber, offerIdentifier, silent)

    messageNumber <- messageNumber + 1
    terminate_sequence(acceptIdentifier, messageNumber, offerIdentifier, silent)

    pop_gen_out <- structure(
      list(
        inputs = parameters,
        raw = retrieveResultsResult,
        wait_for = secs_to_wait,
        wait_until = lubridate::now() + lubridate::seconds(secs_to_wait)
        ),
      class = "rpgcli"
      )

  }, error = function(e) {

    pop_gen_out <- structure(list(inputs = parameters, err = e), class = "rpgcli")
    return(pop_gen_out)

  })

  return(pop_gen_out)
}
