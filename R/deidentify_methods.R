#'@title methods to de-identify different forms of data
#'@details
#'@param .data
#'a data frame with columns to encrypt
#'@param cols_to_encrypt
#'A character vector with the names associated with the columns to encrypt
#'@param key
#'A cyphr key generated from the `cyphr` package
#'@return
#'A Dataframe with encrypted columns ending with the prefix 'encrypt_id'
#'@examples
#'\dontrun{
#'deidentify_text(mtcars, "mpg")
#'}
#'@export
deidentify_text <- function(.data, cols_to_encrypt, key = NULL){
  if(!is.character(cols_to_encrypt)){
    stop("cols_to_encrypt must be a character vector!")
  }
  if(class(key) != "cyphr_key"){
    stop("key must be a cyphr key")
  }
  .data[,cols_to_encrypt, drop = FALSE] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),list("encrypt_id" = ~.apply_enc(as.character(.),key = key))))
}



.enc_strings <- function(col_to_encrypt,key) {
  # encrypts the vector depending on the key
  enc_string <- paste0(cyphr::encrypt_string(col_to_encrypt,key = key), collapse = "")
  return(enc_string)
}


# this encrypts the unique vector of strings and passes it on.

.apply_enc <- function(string_to_encrypt, key) {
  unique_ent <- unique(string_to_encrypt)
  enc_string <-
    unlist(lapply(unique_ent, function(x)
      .enc_strings(x, key)))
  # change the names based on the original encrypted string
  names(enc_string) <- unique_ent
  # use a lookup table to speedily replace values
  # this also removes the names of the vector
  final_vec <- unname(enc_string[string_to_encrypt])
  return(final_vec)
}

