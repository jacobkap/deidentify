#' @title generate aes or libsodium keys
#'
#' @param ...
#' arguments to pass to openssl::aes_keygen or sodium::keygen
#' @return
#' return a cyphr key object for use with deidentify_id and identify_id functions
#' @importFrom sodium keygen
#' @importFrom openssl aes_keygen
#'
#' @examples
#' \dontrun{gen_aes_key(12)}
#'@export
gen_aes_key <- function(...) {
  cykey <- cyphr::key_openssl(key = aes_keygen(...))
  cykey
}

#'@export
gen_sodium_key <- function(...) {
  sodkey <- cyphr::key_sodium(key = keygen(...))
  sodkey
}
