
#' @export
#' @description Function to open connection to db, disigned to easy dubugging.
#' @param where text mark to distingiush calss
make_db_connection <- function(where = "") {
  if (getOption("APP.DEBUG", FALSE)) message("=== DB CONNECT ", where)
  DBI::dbConnect(RSQLite::SQLite(), getOption("APP.FILE_DB", FALSE))
}

#' @export
#' @description 
#' Function to close connection to db, disigned to easy dubugging and
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
#' @description
#' Проверить если таблица есть в базе данных и инициировать ее, если от
check_if_table_is_exist_and_init_if_not <- function(
  table_name,
  forms_id_type_list,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  if (table_name %in% DBI::dbListTables(con)) {

    cli::cli_inform(c("*" = "таблица есть такая: '{table_name}'"))

    # если таблица существует, производим проверку структуры таблицы
    compare_existing_table_with_schema(table_name, forms_id_type_list)

  } else {

    if (table_name == "main") {
      dummy_df <- dplyr::mutate(
        get_dummy_df(forms_id_type_list), 
        main_key = "dummy",
        .before = 1
      )
    }
    if (table_name != "main") {
      dummy_df <- get_dummy_df(forms_id_type_list) |>
        dplyr::mutate(
          main_key = "dummy",
          nested_key = "dummy",
          .before = 1
        )
    }

    # write dummy df into base, then delete dummy row
    DBI::dbWriteTable(con, table_name, dummy_df, append = TRUE)
    DBI::dbExecute(con, glue::glue("DELETE FROM {table_name} WHERE main_key = 'dummy'"))

    cli::cli_alert_success("таблица '{table_name}' успешно создана")
  }

}

#' @description
#' Возращает пустое значение для каждого типа формы
get_dummy_data <- function(type) {

  if (type %in% c("text", "select_one", "select_multiple")) return("dummy")
  if (type %in% c("radio", "checkbox")) return("dummy")
  if (type %in% c("date")) return(as.Date("1990-01-01"))
  if (type %in% c("number")) return(as.double(999))
  cli::cli_abort("для типа формы '{type}' нет примера пустого значения!")

}

#' @description
#' Генерация пустого датасета с пустыми значениями соответствующие
#' типу данных
get_dummy_df <- function(forms_id_type_list) {

  options(box.path = here::here())
  box::use(modules/utils)

  purrr::map(
    .x = forms_id_type_list,
    .f = utils$get_empty_data
  ) |>
    dplyr::as_tibble()

}

#' @description
#' Сравнение полей в существующей в базе данных таблице и попытка
#' коррекции таблицы
compare_existing_table_with_schema <- function(
  table_name,
  forms_id_type_list,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  forms_id_type_list_names <- names(forms_id_type_list)

  if (table_name == "main") {
    forms_id_type_list_names <- c("main_key", forms_id_type_list_names)
  } else {
    forms_id_type_list_names <- c("main_key", "nested_key", forms_id_type_list_names)
  }

  options(box.path = here::here())
  box::use(modules/utils)

  # checking if db structure in form compatible with alrady writed data (in case on changig form)
  if (identical(colnames(DBI::dbReadTable(con, table_name)), forms_id_type_list_names)) {
    # ...
  } else {

    df_to_rewrite <- DBI::dbReadTable(con, table_name)
    form_base_difference <- setdiff(forms_id_type_list_names, colnames(df_to_rewrite))
    base_form_difference <- setdiff(colnames(df_to_rewrite), forms_id_type_list_names)

    # if lengths are equal
    if (length(forms_id_type_list_names) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) == 0 &&
          length(base_form_difference) == 0) {
      cli::cli_warn("changes in scheme file detected: assuming order changed only")
      print(forms_id_type_list_names)
    }

    if (length(forms_id_type_list_names) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) != 0 &&
          length(base_form_difference) != 0) {
      cli::cli_abort("changes in scheme file detected: structure has been changed")
    }

    if (length(forms_id_type_list_names) > length(colnames(df_to_rewrite)) && length(form_base_difference) != 0) {
      cli::cli_warn("changes in scheme file detected: new inputs form was added")
      cli::cli_warn("trying to adapt database")

      # add empty data for each new input form
      for (i in form_base_difference) {
        df_to_rewrite <- df_to_rewrite |>
          dplyr::mutate(!!dplyr::sym(i) := utils$get_empty_data(forms_id_type_list[i]))
      }

      # reorder due to scheme
      df_to_rewrite <- df_to_rewrite |>
        dplyr::select(dplyr::all_of(forms_id_type_list_names))

      DBI::dbWriteTable(con, table_name, df_to_rewrite, overwrite = TRUE)
      DBI::dbExecute(con, glue::glue("DELETE FROM {table_name} WHERE main_key = 'dummy'"))
    }

    if (length(forms_id_type_list_names) < length(colnames(df_to_rewrite))) {
      cli::cli_abort("changes in scheme file detected: some of inputs form was deleted! it may cause data loss!")
    }
    
  }
}

#' @export
write_df_to_db <- function(df, table_name, scheme, main_key, nested_key, con) {

  date_columns   <- subset(scheme, form_type == "date", form_id, drop = TRUE)
  number_columns <- subset(scheme, form_type == "number", form_id, drop = TRUE)

  excel_to_db_dates_converter <- function(date) {

    parse_date1 <- tryCatch(
      as.Date(date, tryFormats = c("%Y-%m-%d")),
      error = function(e) NULL
    )
    parse_date2 <- suppressWarnings(as.Date(as.numeric(date), origin = "1899-12-30"))

    date <- if (!is.null(parse_date1)) {
      parse_date1
    } else if (!is.na(parse_date2)) {
      parse_date2
    } else {
      date
    }
    
    date <- as.character(format(date, "%Y-%m-%d"))
  }

  df <- df |>
    dplyr::mutate(
      # даты - к единому формату
      dplyr::across(tidyselect::all_of({{date_columns}}), \(x) purrr::map_chr(x, excel_to_db_dates_converter)),
      # числа - к единому формату десятичных значений
      dplyr::across(tidyselect::all_of({{number_columns}}), ~ gsub("\\.", "," , .x)),
    )

  if (table_name == "main") {
    del_query <- glue::glue("DELETE FROM main WHERE main_key = '{main_key}'")
  }

  if (table_name != "main") {
    if (is.null(nested_key)) {
      del_query <- glue::glue("DELETE FROM '{table_name}' WHERE main_key = '{main_key}'")
    } else {
      del_query <- glue::glue("DELETE FROM '{table_name}' WHERE main_key = '{main_key}' AND nested_key = '{nested_key}'")
    }
  }

  deleted <- DBI::dbExecute(con, del_query)
  cli::cli_alert_success("deleted {deleted} rows for '{main_key}' in '{table_name}")

  # записать данные
  DBI::dbWriteTable(con, table_name, df, append = TRUE)

  # report
  cli::cli_alert_success("данные для '{main_key}' в таблице '{table_name}' успешно обновлены")

}

#' @export
#' reading tables from db by name and id ========
read_df_from_db_by_id <- function(table_name, main_key, nested_key, con) {

  # check if this table exist
  if (table_name == "main") {
    query <- glue::glue("
      SELECT * 
      FROM main
      WHERE main_key = '{main_key}'
    ")
  }

  if (table_name != "main") {
    if(!missing(nested_key)) {
      query <- glue::glue("
        SELECT * 
        FROM {table_name}
        WHERE main_key = '{main_key}' AND nested_key = '{nested_key}'
      ")
    } else {
      query <- glue::glue("
        SELECT * 
        FROM {table_name}
        WHERE main_key = '{main_key}'
      ")
    }
  }
  DBI::dbGetQuery(con, query)
}

#' @export
get_keys_from_table <- function(table_name, con) {

  DBI::dbGetQuery(con, glue::glue("SELECT DISTINCT main_key FROM {table_name}")) |>
    dplyr::pull()

}

#' @export
get_nested_keys_from_table <- function(table_name, main_key, con) {

  DBI::dbGetQuery(con, glue::glue("SELECT DISTINCT nested_key FROM {table_name} WHERE main_key == '{main_key}'")) |>
    dplyr::pull()

}