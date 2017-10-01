# adaptation of https://github.com/sdoyen/r_password_crypt

key_file_name <- ".rpg-k.RData"
proxy_file_name <- ".rpg-p.bin"

#' Proxy Server Configuration
#'
#' Set up configuration when internet access is via a proxy server.
#'
#' The configuration is stored as encrypted data in the user's home directory.
#'
#' Refer to \code{\link[httr]{use_proxy}} for more information.
#'
#' @param url Location of proxy
#' @param port Location of proxy
#' @param username Login details for proxy, if needed
#' @param password Login details for proxy, if needed
#' @param auth Type of HTTP authentication to use. Should be one of the following: basic, digest, digest_ie, gssnegotiate, ntlm, any.
#' @export
#' @examples
#' # set up access to proxy server requiring authentication
#' store_proxy_settings("my_corporate_proxy_server", 8080, "kevin", "pass123!")

store_proxy_settings <- function(url, port = NA_integer_, username = NA_character_, password = NA_character_, auth = "basic") {

  make_key_and_store()
  key <- load_key()
  settings <- data.frame(
    url = url,
    port = port,
    username = username,
    password = password,
    auth = auth,
    stringsAsFactors = F
    )
  path_to_file <- paste(dirname(path.expand("~")), proxy_file_name, sep = "/")
  write_aes(settings, path_to_file, key)

}

has_proxy_settings <- function() {

  path_to_file <- paste(dirname(path.expand("~")), proxy_file_name, sep = "/")
  has <- file.exists(path_to_file)
  return(has)
}

load_proxy_settings <- function() {

  key <- load_key()
  path_to_file <- paste(dirname(path.expand("~")), proxy_file_name, sep = "/")
  stopifnot(file.exists(path_to_file))
  settings <- read_aes(path_to_file, key)
  settings_list <- list(
    url = settings[1, "url"],
    port = settings[1, "port"],
    username = settings[1, "username"],
    password = settings[1, "password"],
    auth = settings[1, "auth"]
    )
  if (is.na(settings_list$port)) settings_list$port <- NULL
  if (is.na(settings_list$username)) settings_list$username <- NULL
  if (is.na(settings_list$password)) settings_list$password <- NULL
  return(settings_list)
}

#' Proxy Server Configuration
#'
#' Remove configuration stored in user's home directory.
#' @export

clear_proxy_settings <- function() {

  path_to_file <- paste(dirname(path.expand("~")), proxy_file_name, sep = "/")
  has <- file.exists(path_to_file)
  if (has) file.remove(path_to_file)

  path_to_file <- paste(dirname(path.expand("~")), key_file_name, sep = "/")
  has <- file.exists(path_to_file)
  if (has) file.remove(path_to_file)

  return(invisible(has))

}

write_aes <- function(df, filename, key) {

  conn_name <- "write_aes_out"
  out <- textConnection(conn_name, "w", local = T)
  write.csv(df, out, row.names = F)
  close(out)
  csv <- paste(get(conn_name), collapse = "\n")
  raw <- charToRaw(csv)
  raw <- c(raw, as.raw(rep(0, 16 - length(raw) %% 16)))
  aes <- digest::AES(key, mode = "ECB")
  aes$encrypt(raw)
  writeBin(aes$encrypt(raw), filename)
  rm(list = ls(pattern = conn_name))

}

read_aes <- function(filename, key) {

  dat <- readBin(filename, "raw", n = 1000)
  aes <- digest::AES(key, mode = "ECB")
  raw <- aes$decrypt(dat, raw = TRUE)
  txt <- rawToChar(raw[raw > 0])
  df <- read.csv(text = txt, stringsAsFactors = F)
  return(df)

}

make_key_and_store <- function() {

  key <- as.raw(sample(1:16, 16))
  path_to_file <- paste(dirname(path.expand("~")), key_file_name, sep = "/")
  save(key, file = path_to_file)
  rm(key)

}

load_key <- function() {

  path_to_file <- paste(dirname(path.expand("~")), key_file_name, sep = "/")
  stopifnot(file.exists(path_to_file))
  load(path_to_file)
  return(key)

}
