#'@title decrypt data using a cyphr key
#'@description
#'When you encrypt a field with deidentify, it is relatively secure depending on the method of encryption
#'that you may use. However, in the case that you need to decrypt any of the fields and used encryption
#'methods to do so, you will also be able to decrypt any of the fields back to their original form.
#'@inheritParams deidentify_text
#'@param cols_to_decrypt
#'Specific to data.frames. Should be a character vector of the names of the columns that you
#'would like to decrypt.
#'@return
#'either a data.frame or character vector depending on the object passed through
#'@examples
#'\dontrun{
#'key <- gen_aes_key()
#'encrypt_string <- deidentify_text("cat",key)
#'identical(identify_text(encrypt_string,key),"cat")
#'}
#'@export
identify_text <- function(.data,key,cols_to_decrypt){
  UseMethod("identify_text")
}


#'@export
identify_text.data.frame <- function(.data,key = NULL,cols_to_decrypt = NULL){
  if (!is.character(cols_to_decrypt)) {
    stop("cols_to_encrypt must be a character vector.")
  }
  if (class(key) != "cyphr_key") {
    stop("key must be a cyphr key")
  }
  # check that the column name actually exists in the data.
  if (!all(cols_to_decrypt %in% names(.data))) {
    stop("You have selected columns which are not in the data.")
  }
  decrypted_data <- .data[, cols_to_decrypt, drop = F] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), list("decrypt_id" = ~ .apply_decryption(., key)))) %>%
    dplyr::left_join(.data,by = cols_to_decrypt)
  return(decrypted_data)
}

#'@export
identify_text.character <- function(.data,key = NULL,cols_to_decrypt = NULL){
  if(is.null(key)){
    stop("Key must be provided in order to decrypt")
  }
  if (class(key) != "cyphr_key") {
    stop("key must be a cyphr key")
  }
  decrypted_data <- .apply_decryption(.data,key)
  return(decrypted_data)
}


# extract the unique values and then decrypt and join them back
.apply_decryption <- function(value_to_decrypt,key){
  uniq_string <- unique(value_to_decrypt) # This cuts processing time by a lot if dup strings
  decrypted_string <- .decrypt_data(uniq_string,key)
  names(decrypted_string) <- uniq_string
  decrypted_string <- unname(decrypted_string[value_to_decrypt])
  return(decrypted_string)
}

# decrypts a character vector
.decrypt_data <- function(value_to_decrypt,key) {
  sep_string <- strsplit(value_to_decrypt, "(?<=..)", perl = TRUE)
  unlist(lapply(sep_string, function(x) {
    cyphr::decrypt_string(as.raw(strtoi(x, 16L)), key)
  }))
}
