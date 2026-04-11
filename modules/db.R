
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
  schm,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  main_key <- schm$get_main_key_id

  purrr::walk(
    .x = schm$all_tables_names,
    .f = \(table_name, con) {

      forms_id_type_list <- schm$get_id_type_list(table_name)
      key_name           <- schm$get_key_id(table_name)

      if (table_name %in% DBI::dbListTables(con)) {

        cli::cli_inform(c("*" = "таблица есть такая: '{table_name}'"))

        # если таблица существует, производим проверку структуры таблицы
        compare_existing_table_with_schema(
          table_name = table_name,
          schm = schm
        )

      } else {

        if (table_name == "main") {
          dummy_df <- get_dummy_df(forms_id_type_list) |>
            dplyr::mutate(
              !!dplyr::sym(main_key) := "dummy",
              .before = 1
            )
        }
        if (table_name != "main") {
          dummy_df <- get_dummy_df(forms_id_type_list) |>
            dplyr::mutate(
              !!dplyr::sym(main_key) := "dummy",
              !!dplyr::sym(key_name) := "dummy",
              .before = 1
            )
        }

        # write dummy df into base, then delete dummy row
        DBI::dbWriteTable(con, table_name, dummy_df, append = TRUE)
        DBI::dbExecute(con, glue::glue("DELETE FROM {table_name} WHERE {main_key} = 'dummy'"))

        cli::cli_alert_success("таблица '{table_name}' успешно создана")
      }
    },
    con = con
  )
  
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
  schm,
  con = rlang::env_get(rlang::caller_env(), nm = "con")
) {

  main_key  <- schm$get_main_key_id
  key_id    <- schm$get_key_id(table_name)
  forms_ids <- schm$get_forms_ids(table_name)

  if (table_name == "main") {
    all_ids_from_schema <- c(main_key, forms_ids)
  } else {
    all_ids_from_schema <- c(main_key, key_id, forms_ids)
  }

  options(box.path = here::here())
  box::use(modules/utils)

  # checking if db structure in form compatible with alrady writed data (in case on changig form)
  if (identical(colnames(DBI::dbReadTable(con, table_name)), all_ids_from_schema)) {
    # ...
  } else {

    df_to_rewrite <- DBI::dbReadTable(con, table_name)
    form_base_difference <- setdiff(all_ids_from_schema, colnames(df_to_rewrite))
    base_form_difference <- setdiff(colnames(df_to_rewrite), all_ids_from_schema)

    # if lengths are equal
    if (length(all_ids_from_schema) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) == 0 &&
          length(base_form_difference) == 0) {
      cli::cli_warn("changes in scheme file detected: assuming order changed only")
    }

    if (length(all_ids_from_schema) == length(colnames(df_to_rewrite)) &&
          length(form_base_difference) != 0 &&
          length(base_form_difference) != 0) {
      cli::cli_abort("changes in scheme file detected: structure has been changed")
    }

    if (length(all_ids_from_schema) > length(colnames(df_to_rewrite)) && length(form_base_difference) != 0) {
      cli::cli_warn("changes in scheme file detected: new inputs form was added")
      cli::cli_warn("trying to adapt database")

      # add empty data for each new input form
      for (i in form_base_difference) {
        df_to_rewrite <- df_to_rewrite |>
          dplyr::mutate(!!dplyr::sym(i) := utils$get_empty_data(forms_id_type_list[i]))
      }

      # reorder due to scheme
      df_to_rewrite <- df_to_rewrite |>
        dplyr::select(dplyr::all_of(all_ids_from_schema))

      DBI::dbWriteTable(con, table_name, df_to_rewrite, overwrite = TRUE)
      DBI::dbExecute(con, glue::glue("DELETE FROM {table_name} WHERE {main_key} = 'dummy'"))
    }

    if (length(all_ids_from_schema) < length(colnames(df_to_rewrite))) {
      cli::cli_abort("changes in scheme file detected: some of inputs form was deleted! it may cause data loss!")
    }
    
  }
}

#' @export
write_df_to_db <- function(
  df, 
  table_name, 
  schm,
  main_key_value, 
  nested_key_value, 
  con
) {

  scheme         <- schm$get_schema(table_name)
  main_key_id    <- schm$get_main_key_id
  nested_key_id  <- schm$get_key_id(table_name)

  date_columns   <- subset(scheme, form_type == "date", form_id, drop = TRUE)
  number_columns <- subset(scheme, form_type == "number", form_id, drop = TRUE)

  df <- df |>
    dplyr::mutate(
      # даты - к единому формату
      dplyr::across(tidyselect::all_of({{date_columns}}), \(x) purrr::map_chr(x, excel_to_db_dates_converter)),
      # числа - к единому формату десятичных значений
      dplyr::across(tidyselect::all_of({{number_columns}}), ~ gsub("\\.", "," , .x)),
    )

  if (table_name == "main") {
    del_query <- glue::glue("DELETE FROM main WHERE {main_key_id} = '{main_key_value}'")
  }

  if (table_name != "main") {
    if (is.null(nested_key_value)) {
      del_query <- glue::glue("DELETE FROM '{table_name}' WHERE {main_key_id} = '{main_key_value}'")
    } else {
      del_query <- glue::glue("DELETE FROM '{table_name}' WHERE {main_key_id} = '{main_key_value}' AND {nested_key_id} = '{nested_key_value}'")
    }
  }

  deleted <- DBI::dbExecute(con, del_query)
  cli::cli_alert_success("deleted {deleted} rows for '{main_key_value}' in '{table_name}")

  # записать данные
  DBI::dbWriteTable(con, table_name, df, append = TRUE)

  # report
  cli::cli_alert_success("данные для '{main_key_value}' в таблице '{table_name}' успешно обновлены")

}

#' @export
#' reading tables from db by name and id ========
read_df_from_db_by_id <- function(
  table_name,
  schm,
  main_key_value, 
  nested_key_value, 
  con
) {

  main_key_id <- schm$get_main_key_id

  # check if this table exist
  if (table_name == "main") {
    query <- glue::glue("
      SELECT * 
      FROM main
      WHERE {main_key_id} = '{main_key_value}'
    ")
  }

  if (table_name != "main") {
    if(!missing(nested_key_value)) {
      key_id <- schm$get_key_id(table_name)
      query <- glue::glue("
        SELECT * 
        FROM {table_name}
        WHERE {main_key_id} = '{main_key_value}' AND {key_id} = '{nested_key_value}'
      ")
    } else {
      query <- glue::glue("
        SELECT * 
        FROM {table_name}
        WHERE {main_key_id} = '{main_key_value}'
      ")
    }
  }
  DBI::dbGetQuery(con, query)
}

#' @export
get_keys_from_table <- function(table_name, schm, con) {

  main_key_id <- schm$get_main_key_id
  DBI::dbGetQuery(con, glue::glue("SELECT DISTINCT {main_key_id} FROM {table_name}")) |>
    dplyr::pull()

}

#' @export
get_nested_keys_from_table <- function(table_name, schm, main_key_value, con) {

  main_key_id <- schm$get_main_key_id
  key_id <- schm$get_key_id(table_name)

  DBI::dbGetQuery(con, glue::glue("SELECT DISTINCT {key_id} FROM {table_name} WHERE {main_key_id} == '{main_key_value}'")) |>
    dplyr::pull()

}


### HELPERS ---------
#' @export
excel_to_db_dates_converter <- function(date) {

  if (is.na(date)) return(NA)
  # cli::cli_inform("date: {date} | nchar: {nchar(date)} | typeof: {typeof(date)}")

  # если текст, количество символов 7, и маска соответствует 'MM.YYYY'
  if (typeof(date) == "character") {
    date <- trimws(date)

    if (nchar(date) == 4 & grepl("((?:19|20)\\d\\d)", date)) {
      date <- sprintf("%s-01-01", date)
    } else if (nchar(date) == 7 & grepl("(0?[1-9]|1[012])\\.((?:19|20)\\d\\d)", date)) {
      # если текст, количество символов 7, и маска соответствует 'MM.YYYY'
      date <- sprintf("01.%s", date)
    } else if (nchar(date) == 10 & grepl("([12][0-9]|3[01]|0?[1-9])\\.(0?[1-9]|1[012])\\.((?:19|20)\\d\\d)", date)) {
      # ...
    } else if (nchar(date) == 10 & grepl("((?:19|20)\\d\\d)-(0?[1-9]|1[012])-([12][0-9]|3[01]|0?[1-9])", date)) {
      # ...
    } else {
      cli::cli_alert_warning("can't compute date from '{date}'")
      return(date)
    }
  }

  parse_date1 <- tryCatch(
    as.Date(date, tryFormats = c("%d.%m.%Y", "%Y-%m-%d")),
    error = function(e) NULL
  )
  parse_date2 <- suppressWarnings(as.Date(as.numeric(date), origin = "1899-12-30"))

  fin_date <- if (!is.null(parse_date1)) {
    parse_date1
  } else if (!is.na(parse_date2)) {
    parse_date2
  } else {
    date
  }
  
  fin_date <- as.character(format(fin_date, "%Y-%m-%d"))
  fin_date
}
