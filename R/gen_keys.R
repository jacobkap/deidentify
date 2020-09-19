#' @title Generate aes or libsodium key
#'
#' @param ...
#' arguments to pass to `openssl::aes_keygen()` or `sodium::keygen()`
#' @return
#' A `cyphr` key object for use with `deidentify_id()` and identify_id functions
#' @importFrom sodium keygen
#' @importFrom openssl aes_keygen
#'
#' @examples
#' \dontrun{gen_aes_key(16)}
#' @export
gen_aes_key <- function(...) {
  cykey <- cyphr::key_openssl(key = aes_keygen(...))
  return(cykey)
}


#' @title Generate sodium key
#' @param ...
#' arguments to pass to `openssl::aes_keygen()` or `sodium::keygen()`
#'
#' @return
#' A `cyphr` key object for use with `deidentify_id()` and identify_id functions
#' @export
#'
#' @examples
#' \dontrun{key_sodium(12)}
gen_sodium_key <- function(...) {
  sodkey <- cyphr::key_sodium(key = sodium::keygen(...))
  return(sodkey)
}
