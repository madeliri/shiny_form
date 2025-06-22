# box ========
.on_load = function(ns) {
  message(
    'Loading module "', box::name(), '"\n',
    'Module path: "', basename(box::file()), '"'
  )
}
# ================

# DB RELATED ===========================
#' @export
#' @description Function to open connection to db, disigned to easy dubugging.
#' @param where text mark to distingiush calss
make_db_connection <- function(where = "") {
  if (getOption("APP.DEBUG", FALSE)) message("=== DB CONNECT ", where)
  DBI::dbConnect(RSQLite::SQLite(), getOption("APP.FILE_DB", FALSE))
}

#' @export
#' @description Function to close connection to db, disigned to easy dubugging and
#' hide warnings.
close_db_connection <- function(con, where = "") {
  tryCatch(
    expr = DBI::dbDisconnect(con),
    error = function(e) print(e),
    warning = function(w) if (getOption("APP.DEBUG", FALSE)) message("=!= ALREADY DISCONNECTED ", where),
    finally = if (getOption("APP.DEBUG", FALSE)) message("=/= DB DISCONNECT ", where)
  )
}