# box ========
.on_load = function(ns) {
  message(
    'Loading module "', box::name(), '"\n',
    'Module path: "', basename(box::file()), '"'
  )
}

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

#' @export
#' проверить если таблица есть в базе данных и инициировать ее, если от
check_if_table_is_exist_and_init_if_not <- function(
  table_name,
  forms_id_type_list,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  if (table_name %in% DBI::dbListTables(con)) {

    cli::cli_inform("таблица есть такая: 'table_name'")

    # если таблица существует, производим проверку структуры таблицы
    compare_existing_table_with_schema(table_name, forms_id_type_list)

  } else {

    dummy_df <- dplyr::mutate(get_dummy_df(forms_id_type_list), id = "dummy")

    # write dummy df into base, then delete dummy row
    DBI::dbWriteTable(con, table_name, dummy_df, append = TRUE)
    DBI::dbExecute(con, "DELETE FROM main WHERE id = 'dummy'")

    cli::cli_alert_success("таблица '{table_name}' успешно создана")
  }

}

get_dummy_data <- function(type) {

  if (type %in% c("text", "select_one", "select_multiple")) return("dummy")
  if (type %in% c("radio", "checkbox")) return("dummy")
  if (type %in% c("date")) return(as.Date("1990-01-01"))
  if (type %in% c("number")) return(as.double(999))

}

get_dummy_df <- function(forms_id_type_list) {

  options(box.path = here::here())
  box::use(modules/utils)

  purrr::map(
    .x = forms_id_type_list,
    .f = utils$get_empty_data
  ) |>
    dplyr::as_tibble()

}

compare_existing_table_with_schema <- function(
  table_name,
  forms_id_type_list,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  options(box.path = here::here())
  box::use(modules/utils)

  # checking if db structure in form compatible with alrady writed data (in case on changig form)
  if (identical(colnames(DBI::dbReadTable(con, table_name)), names(forms_id_type_list))) {
    print("identical")
  } else {
    df_to_rewrite <- DBI::dbReadTable(con, table_name)
    form_base_difference <- setdiff(names(forms_id_type_list), colnames(df_to_rewrite))
    base_form_difference <- setdiff(colnames(df_to_rewrite), names(forms_id_type_list))

    # if lengths are equal
    if (length(names(forms_id_type_list)) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) == 0 &&
          length(base_form_difference) == 0) {
      warning("changes in scheme file detected: assuming order changed only")
    }

    if (length(names(forms_id_type_list)) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) != 0 &&
          length(base_form_difference) != 0) {
      stop("changes in scheme file detected: structure has been changed")
    }

    if (length(names(forms_id_type_list)) > length(colnames(df_to_rewrite)) && length(form_base_difference) != 0) {
      warning("changes in scheme file detected: new inputs form was added")
      warning("trying to adapt database")

      # add empty data for each new input form
      for (i in form_base_difference) {
        df_to_rewrite <- df_to_rewrite |>
          dplyr::mutate(!!dplyr::sym(i) := utils$get_empty_data(forms_id_type_list[i]))
      }

      # reorder due to scheme
      df_to_rewrite <- df_to_rewrite |>
        dplyr::select(dplyr::all_of(names(forms_id_type_list)))

      DBI::dbWriteTable(con, table_name, df_to_rewrite, overwrite = TRUE)
      DBI::dbExecute(con, "DELETE FROM main WHERE id = 'dummy'")
    }

    if (length(names(forms_id_type_list)) < length(colnames(df_to_rewrite))) {
      stop("changes in scheme file detected: some of inputs form was deleted! it may cause data loss!")
    }
    # cleaning
    rm(df_to_rewrite, form_base_difference)
  }
}