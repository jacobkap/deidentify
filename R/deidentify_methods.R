#'@title methods to deidentify different forms of data
#'
#'
#'







# fixing up the progress bar so that it responds to the name of the column
create_prog <- function(vect){
  x <- length(vect)
  progress::progress_bar$new(total = x,format = " Encrypting column :what [:bar] :percent eta: :eta")
}


enc_strings <- function(col_to_encrypt,name_of_col,key) {
  pb$tick(tokens = list(what = name_of_col))
  # encrypts the vector depending on the key
  enc_string <- paste0(cyphr::encrypt_string(x,key = key), collapse = "")
}


# this encrypts the unique vector of strings and passes it on.

function(string_to_encrypt, key) {

  enc_string <-
    unlist(lapply(string_to_encrypt, function(x)
      enc_strings(x, , key)))
  # change the names based on the original encrypted string
  names(enc_string) <- string_to_encrypt
  # use a lookup table to speedily replace values
  # this also removes the names of the vector
  final_vec<- unname(enc_string[data_to_encrypt])
}
