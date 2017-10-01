
envelope_ns_defs <- list(
  s = "http://www.w3.org/2003/05/soap-envelope",
  a = "http://www.w3.org/2005/08/addressing",
  r = "http://schemas.xmlsoap.org/ws/2005/02/rm",
  p = "urn:uk-gov-hsl-msu:pgsvc-v03"
  )

envelope_xml_str <- paste0(
  "<s:Envelope xmlns:s=\"",
  envelope_ns_defs[["s"]],
  "\" xmlns:r=\"",
  envelope_ns_defs[["r"]],
  "\" xmlns:a=\"",
  envelope_ns_defs[["a"]],
  "\"></s:Envelope>"
  )

defaults <- list(
  AgeLower = 20,
  AgeUpper = 30,
  BodyMassIndexLower = 20,
  BodyMassIndexUpper = 30,
  Dataset = "P3M",
  EnzymeRateParameter = "Vmax",
  EnzymeRateUnits = "PicoMolsPerMinute",
  EthnicityProbabilityBlack = 0.3,
  EthnicityProbabilityNonBlackOrWhite = 0.3,
  EthnicityProbabilityWhite = 0.3,
  FlowUnits = "MilliLitresPerMinute",
  HeightLower = 150,
  HeightUpper = 180,
  InVitroEnzymeRates = list(),
  IsRichlyPerfusedTissueDiscrete = list(),
  IsSlowlyPerfusedTissueDiscrete = list(),
  MolecularWeight = 0,
  PopGenUserName = "",
  PopulationSize = 10,
  PopulationType = "Realistic",
  ProbabilityOfMale = 0.5,
  Seed = -1
  )

secs_post_call_timeout <- 60

add_header <- function(
  envelope,
  messageID,
  action,
  sequenceIdentifier = NULL,
  messageNumber = NULL,
  replyTo = FALSE,
  acknowledgeIdentifier = NULL,
  acknowledgeLower = NULL,
  acknowledgeUpper = NULL
  ) {

  header_node <- xml2::xml_add_child(envelope, "s:Header")

  if (!is.null(acknowledgeIdentifier) && !is.null(acknowledgeLower) && !is.null(acknowledgeUpper)) {

    seq_ack_node <- xml2::xml_add_child(header_node, "r:SequenceAcknowledgement")
    xml2::xml_add_child(seq_ack_node, "r:Identifier", acknowledgeIdentifier)
    ack_range_node <- xml2::xml_add_child(seq_ack_node, "r:AcknowledgementRange")
    xml2::xml_set_attrs(ack_range_node, c(Lower = acknowledgeLower, Upper = acknowledgeUpper))

  }

  if (!is.null(sequenceIdentifier) && !is.null(messageNumber)) {

    seq_node <- xml2::xml_add_child(header_node, "r:Sequence")
    xml2::xml_attr(seq_node, "s:mustUnderstand") <- "1"
    xml2::xml_add_child(seq_node, "r:Identifier", sequenceIdentifier)
    xml2::xml_add_child(seq_node, "r:MessageNumber", messageNumber)
    if (endsWith(action, "LastMessage")) {
      xml2::xml_add_child(seq_node, "r:LastMessage")
    }

  }

  action_node <- xml2::xml_add_child(header_node, "a:Action", action)
  xml2::xml_attr(action_node, "s:mustUnderstand") <- "1"

  if (!is.null(messageID)) {
    xml2::xml_add_child(header_node, "a:MessageID", messageID)
  }

  if (isTRUE(replyTo)) {

    reply_to_node <- xml2::xml_add_child(header_node, "a:ReplyTo")
    xml2::xml_add_child(reply_to_node, "a:Address", "http://www.w3.org/2005/08/addressing/anonymous")

  }

  to_node <- xml2::xml_add_child(header_node, "a:To", "http://xnet.hsl.gov.uk/popgen/svc/pg.svc/epv03")
  xml2::xml_attr(to_node, "s:mustUnderstand") <- "1"

}

call_service <- function(body) {

  if (has_proxy_settings()) {

    proxy_settings <- load_proxy_settings()

    response <- httr::POST(
      "http://xnet.hsl.gov.uk/popgen/svc/pg.svc/epv03",
      body = body,
      httr::timeout(secs_post_call_timeout),
      encode = "raw",
      httr::content_type("application/soap+xml"),
      httr::use_proxy(
        proxy_settings$url,
        port = proxy_settings$port,
        username = proxy_settings$username,
        password = proxy_settings$password,
        auth = proxy_settings$auth
        )
      )

  } else {

    response <- httr::POST(
      "http://xnet.hsl.gov.uk/popgen/svc/pg.svc/epv03",
      body = body,
      httr::timeout(secs_post_call_timeout),
      encode = "raw",
      httr::content_type("application/soap+xml")
      )

  }

  return(response)

}

unwrap_body <- function(response, messageID = NULL) {

  content <- httr::content(response, as = "text")

  envelope <- xml2::read_xml(content)

  if (httr::http_error(response)) {

    text_node <- xml2::xml_find_first(envelope, "/s:Envelope/s:Body/s:Fault/s:Reason/s:Text")

    if (!is.na(text_node)) {
      stop(xml2::xml_text(text_node), call. = F)
    } else {
      stop(httr::http_status(response), call. = F)
    }

  } else if (!is.null(messageID)) {

    relates_to_node <- xml2::xml_find_first(envelope, "/s:Envelope/s:Header/a:RelatesTo")
    relates_to <- xml2::xml_text(relates_to_node)
    if (relates_to != messageID) {
      stop("WS-RX fault", call. = F)
    }

  }

  body_node <- xml2::xml_find_first(envelope, "/s:Envelope/s:Body")

  return(body_node)

}

create_sequence <- function(offerIdentifier, silent) {

  messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(envelope, messageID, "http://schemas.xmlsoap.org/ws/2005/02/rm/CreateSequence")

  body_node <- xml2::xml_add_child(envelope, "s:Body")

  create_seq_node <- xml2::xml_add_child(body_node, "CreateSequence")
  xml2::xml_attr(create_seq_node, "xmlns") <- envelope_ns_defs$r

  acks_to_node <- xml2::xml_add_child(create_seq_node, "AcksTo")
  xml2::xml_add_child(acks_to_node, "a:Address", "http://www.w3.org/2005/08/addressing/anonymous")

  offer_node <- xml2::xml_add_child(create_seq_node, "Offer")
  xml2::xml_add_child(offer_node, "Identifier", offerIdentifier)

  post_body <- as.character(envelope)

  if(!silent) cat("Calling PopGen...\n")

  response <- call_service(post_body)

  body_node <- unwrap_body(response, messageID)

  xml2::xml_ns_strip(body_node)

  accept_identifier_node <- xml2::xml_find_first(body_node, "CreateSequenceResponse/Identifier")

  accept_identifier <- xml2::xml_text(accept_identifier_node)

  return(accept_identifier)

}

#' List of Inputs
#'
#' Get a list of required inputs with typical settings.
#'
#' PopGenUserName must be assigned the email address of an active \href{http://xnet.hsl.gov.uk/popgen}{PopGen} account.
#'
#' @return A list of required inputs
#' @export
#' @examples
#' parameters <- get_default_parameters()
#' parameters$InVitroEnzymeRates = list(
#'   c(Enzyme = "CYP1A2", Rate = 123),
#'   c(Enzyme = "CYP2B6", Rate = 456)
#'   )


get_default_parameters <- function() {

  n_default_parameters <- length(defaults)
  to_edit <- vector("list", length = n_default_parameters)

  for (i in 1:n_default_parameters) {
    to_edit[[i]] <- defaults[[i]]
  }

  defaults_names <- names(defaults)

  names(to_edit) <- defaults_names

  return(to_edit)

}

#' Ensure Valid Inputs
#'
#' Fills in any missing inputs with typical values.
#'
#' Takes an existing list and adds tags and default values for missing items.
#'
#' @param parameters A list
#' @return An updated list
#' @export

apply_defaults <- function(parameters) {

  if (!is.list(parameters)) {
    stop("Expecting parameters to be a list object", call. = F)
  }

  defaults_names = names(defaults)
  for (name in defaults_names) {
    if (is.null(parameters[[name]])) {
      parameters[[name]] <- defaults[[name]]
    }
  }

  return(parameters)

}

generate <- function(acceptIdentifier, messageNumber, parameters, silent) {

  messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(
    envelope,
    messageID,
    "urn:uk-gov-hsl-msu:pgsvc-v03/PopGenSvc/Generate",
    acceptIdentifier,
    messageNumber,
    T
    )

  body_node <- xml2::xml_add_child(envelope, "s:Body")

  generate_node <- xml2::xml_add_child(body_node, "Generate")
  xml2::xml_attr(generate_node, "xmlns") <- envelope_ns_defs$p

  inputs_node <- xml2::xml_add_child(generate_node, "inputs")

  parameter_names <- sort(names(parameters))

  for (name in parameter_names) {

    if (name == "InVitroEnzymeRates") {

      input_rates_node <- xml2::xml_add_child(inputs_node, "InVitroEnzymeRates")

      inVitroEnzymeRates <- parameters[[name]]
      nInVitroEnzymeRates <- length(inVitroEnzymeRates)

      if (0 < nInVitroEnzymeRates) {

        for (i in 1:nInVitroEnzymeRates) {

          inVitroEnzymeRate <- inVitroEnzymeRates[[i]]
          input_rate_node <- xml2::xml_add_child(input_rates_node, "InVitroEnzymeRate")
          xml2::xml_add_child(input_rate_node, "Enzyme", inVitroEnzymeRate[["Enzyme"]])
          xml2::xml_add_child(input_rate_node, "Rate", inVitroEnzymeRate[["Rate"]])

        }

      }

    } else if (name == "IsRichlyPerfusedTissueDiscrete" || name == "IsSlowlyPerfusedTissueDiscrete") {

      tissue_names <- parameters[[name]]
      if (0 < length(tissue_names)) {

        text <- do.call(paste, as.list(tissue_names))
        xml2::xml_add_child(inputs_node, name, text)

      }

    } else {

      xml2::xml_add_child(inputs_node, name, parameters[[name]])

    }

  }

  post_body <- as.character(envelope)

  response <- call_service(post_body)

  body_node <- unwrap_body(response, messageID)

  xml2::xml_ns_strip(body_node)

  string_nodes <- xml2::xml_find_all(body_node, "GenerateResponse/GenerateResult/b:string")

  validation_errors <- lapply(string_nodes, xml2::xml_text)

  return(validation_errors)

}

wait_for_completion <- function(acceptIdentifier, messageNumber, offerIdentifier, silent) {

  done <- FALSE
  wait_time <- 2.0
  max_wait_time <- 100

  while (!done) {

    if(!silent) cat(paste("Waiting ", round(wait_time, 1), "s for generation to complete\n", sep = ""))
    Sys.sleep(wait_time)

    messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

    envelope <- xml2::read_xml(envelope_xml_str)

    add_header(
      envelope,
      messageID,
      "urn:uk-gov-hsl-msu:pgsvc-v03/PopGenSvc/IsCompleted",
      acceptIdentifier,
      messageNumber + 1,
      T,
      offerIdentifier,
      1,
      messageNumber
    )

    body_node <- xml2::xml_add_child(envelope, "s:Body")
    is_completed_node <- xml2::xml_add_child(body_node, "IsCompleted")
    xml2::xml_attr(is_completed_node, "xmlns") <- envelope_ns_defs$p

    post_body <- as.character(envelope)

    response <- call_service(post_body)

    body_node <- unwrap_body(response, messageID)

    xml2::xml_ns_strip(body_node)

    is_completed_node <- xml2::xml_find_first(body_node, "IsCompletedResponse/IsCompletedResult")

    is_completed <- xml2::xml_text(is_completed_node)

    if ("true" == tolower(is_completed)) {
      done <- TRUE
    }

    messageNumber <- messageNumber + 1

    if (!done) {
      wait_time <- wait_time * jitter(1.5) # back off
    }

    if (!done && max_wait_time < wait_time) {
      stop("Timed out waiting for generation to complete", call. = F)
    }
  }

  return(messageNumber)
}

retrieve_results <- function(acceptIdentifier, messageNumber, offerIdentifier, silent) {

  messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(
    envelope,
    messageID,
    "urn:uk-gov-hsl-msu:pgsvc-v03/PopGenSvc/RetrieveResults",
    acceptIdentifier,
    messageNumber + 1,
    T,
    offerIdentifier,
    1,
    messageNumber
  )

  body_node <- xml2::xml_add_child(envelope, "s:Body")
  retrieve_results_node <- xml2::xml_add_child(body_node, "RetrieveResults")
  xml2::xml_attr(retrieve_results_node, "xmlns") <- envelope_ns_defs$p

  post_body <- as.character(envelope)

  if(!silent) cat("Downloading results\n")

  response <- call_service(post_body)

  body_node <- unwrap_body(response, messageID)

  retrieve_results_response_node <- xml2::xml_children(body_node)[[1]]

  xml2::xml_ns_strip(retrieve_results_response_node)

  return(retrieve_results_response_node)
}

clean_up <- function(acceptIdentifier, messageNumber, offerIdentifier, silent) {

  messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(
    envelope,
    messageID,
    "urn:uk-gov-hsl-msu:pgsvc-v03/PopGenSvc/CleanUp",
    acceptIdentifier,
    messageNumber + 1,
    T,
    offerIdentifier,
    1,
    messageNumber
  )

  body_node <- xml2::xml_add_child(envelope, "s:Body")
  clean_up_node <- xml2::xml_add_child(body_node, "CleanUp")
  xml2::xml_attr(clean_up_node, "xmlns") <- envelope_ns_defs$p

  post_body <- as.character(envelope)

  if(!silent) cat("Ending call\n")

  response <- call_service(post_body)

  body_node <- unwrap_body(response, messageID)

  xml2::xml_ns_strip(body_node)

  clean_up_result_node <- xml2::xml_find_first(body_node, "CleanUpResponse/CleanUpResult")

  secs_to_wait <- xml2::xml_integer(clean_up_result_node)

  return(secs_to_wait)
}

last_message <- function(acceptIdentifier, messageNumber, offerIdentifier, silent) {

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(
    envelope,
    NULL,
    "http://schemas.xmlsoap.org/ws/2005/02/rm/LastMessage",
    acceptIdentifier,
    messageNumber + 1,
    F,
    offerIdentifier,
    1,
    messageNumber
  )

  xml2::xml_add_child(envelope, "s:Body")

  post_body <- as.character(envelope)

  response <- call_service(post_body)
}

terminate_sequence <- function(acceptIdentifier, messageNumber, offerIdentifier, silent) {

  messageID <- paste0("urn:uuid:", uuid::UUIDgenerate())

  envelope <- xml2::read_xml(envelope_xml_str)

  add_header(
    envelope,
    messageID,
    "http://schemas.xmlsoap.org/ws/2005/02/rm/TerminateSequence",
    NULL,
    NULL,
    F,
    offerIdentifier,
    1,
    messageNumber
  )

  body_node <- xml2::xml_add_child(envelope, "s:Body")
  terminate_sequence_node <- xml2::xml_add_child(body_node, "r:TerminateSequence")
  xml2::xml_add_child(terminate_sequence_node, "r:Identifier", acceptIdentifier)

  post_body <- as.character(envelope)

  call_service(post_body)
}
