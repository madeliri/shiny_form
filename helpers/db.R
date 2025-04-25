# based on https://github.com/datastorm-open/shinymanager/


#' @export
write_db_encrypt <- function(conn, value, name, passphrase = Sys.getenv("AUTH_DB_KEY")) {
  if (is.character(conn)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = conn)
    on.exit(DBI::dbDisconnect(conn))
  }

  if (name == "credentials" && "password" %in% colnames(value)) {
    if (!"is_hashed_password" %in% colnames(value)) {
      value$is_hashed_password <- FALSE
    }
    to_hash <- which(!as.logical(value$is_hashed_password))
    if (length(to_hash) > 0) {
      # store hashed password
      value$password[to_hash] <- sapply(value$password[to_hash], function(x) scrypt::hashPassword(x))
      value$is_hashed_password[to_hash] <- TRUE
    }
  }

  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- openssl::sha256(passphrase)
    value_serialized <- serialize(value, NULL)
    value_encrypted <- openssl::aes_cbc_encrypt(data = value_serialized, key = key)
    value <- data.frame(value = I(list(value_encrypted)), iv = I(list(attr(value_encrypted, "iv"))))
  }

  DBI::dbWriteTable(conn = conn, name = name, value = value, overwrite = TRUE)
}


#' @export
read_db_encrypt <- function(conn, name, passphrase = Sys.getenv("AUTH_DB_KEY")) {

  if (is.character(conn)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = conn)
    on.exit(DBI::dbDisconnect(conn))
  }

  out <- DBI::dbReadTable(conn = conn, name = name)

  if (!is.null(passphrase)) {
    passphrase <- as.character(passphrase)
    passphrase <- charToRaw(passphrase)
    key <- openssl::sha256(passphrase)
    value <- out$value[[1]]
    attr(value, "iv") <- out$iv[[1]]
    out <- openssl::aes_cbc_decrypt(value, key = key)
    out <- unserialize(out)
  }

  return(out)
}
