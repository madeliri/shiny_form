# box ========
.on_load = function(ns) {
  message(
    'Loading module "', box::name(), '"\n',
    'Module path: "', basename(box::file()), '"'
  )
  set_global_options()
}

# ================

#' @export
#' @description костыли для упрощения работы себе
set_global_options <- function(
  SYMBOL_DELIM = "; ",
  APP.DEBUG = FALSE,
  APP.FILE_DB = fs::path("data.sqlite"),
  shiny.host = "127.0.0.1",
  shiny.port = 1337,
  ...
) {
  options(
    SYMBOL_DELIM = SYMBOL_DELIM,
    APP.DEBUG = APP.DEBUG,
    APP.FILE_DB = APP.FILE_DB,
    shiny.host = shiny.host,
    shiny.port = shiny.port,
    ...
  )
}