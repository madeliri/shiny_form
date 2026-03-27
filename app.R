suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(tidyr)
  library(dplyr)
  library(purrr)
  library(magrittr)
  library(shiny)
  library(bslib)
  library(rhandsontable)
  library(shinymanager)
})

source("helpers/functions.R")

# box::purge_cache()
# box::use(./helpers/db)

# SOURCE FILES ============================
config <- config::get(file = "configs/config.yml")

folder_with_schemas <- fs::path("configs/schemas")
FILE_SCHEME         <- fs::path(folder_with_schemas, "main.xlsx")
# dbfile              <- fs::path("data.sqlite")

# options(box.path = getwd())
box::purge_cache()
box::use(
  modules/utils,
  modules/global_options,
  modules/db
)

# SETTINGS ================================
AUTH_ENABLED <- config$auth_module

# CHECK FOR PANDOC
# TEMP ! NEED TO HANDLE
rmarkdown::find_pandoc(dir = "/opt/homebrew/bin/")

# TODO: dynamic button render depend on pandoc installation
if (!rmarkdown::pandoc_available()) warning("Can't find pandoc!")


load_scheme_from_xlsx <- function(
  filename,
  colnames = c("part", "subgroup", "form_id", "form_label", "form_type")
) {

  readxl::read_xlsx(filename) |>
    # fill NA down
    tidyr::fill(all_of(colnames), .direction = "down") |>
    dplyr::group_by(form_id) |>
    tidyr::fill(c(condition, required), .direction = "down") |>
    dplyr::ungroup()

}

# SCHEME_MAIN UNPACK ==========================
# load scheme
SCHEME_MAIN <- load_scheme_from_xlsx(FILE_SCHEME)

# get list of simple inputs
inputs_simple_list <- SCHEME_MAIN |>
  dplyr::filter(!form_type %in% c("inline_table", "inline_table2","description", "description_header")) |>
  dplyr::distinct(form_id, form_type) |>
  tibble::deframe()

# get list of inputs with inline tables
inputs_tables_list <- SCHEME_MAIN |>
  dplyr::filter(form_type == "inline_table") |>
  dplyr::distinct(form_id) |>
  tibble::deframe()

inputs_table_df <- SCHEME_MAIN |>
  dplyr::filter(form_type == "inline_table2") |>
  dplyr::distinct(form_id, .keep_all = TRUE)

# establish connection
con <- db$make_db_connection()

# init DB (write dummy data to "main" table)
if (!"main" %in% DBI::dbListTables(con)) {
  dummy_df <- dplyr::mutate(get_dummy_df(), id = "dummy")

  # write dummy df into base, then delete dummy row
  DBI::dbWriteTable(con, "main", dummy_df, append = TRUE)
  DBI::dbExecute(con, "DELETE FROM main WHERE id = 'dummy'")

  rm(dummy_df)
}

# checking if db structure in form compatible with alrady writed data (in case on changig form)
if (identical(colnames(DBI::dbReadTable(con, "main")), names(inputs_simple_list))) {
  print("identical")
} else {
  df_to_rewrite <- DBI::dbReadTable(con, "main")
  form_base_difference <- setdiff(names(inputs_simple_list), colnames(df_to_rewrite))
  base_form_difference <- setdiff(colnames(df_to_rewrite), names(inputs_simple_list))

  # if lengths are equal
  if (length(names(inputs_simple_list)) == length(colnames(df_to_rewrite)) &&
        length(form_base_difference) == 0 &&
        length(base_form_difference) == 0) {
    warning("changes in scheme file detected: assuming order changed only")
  }

  if (length(names(inputs_simple_list)) == length(colnames(df_to_rewrite)) &&
        length(form_base_difference) != 0 &&
        length(base_form_difference) != 0) {
    stop("changes in scheme file detected: structure has been changed")
  }

  if (length(names(inputs_simple_list)) > length(colnames(df_to_rewrite)) && length(form_base_difference) != 0) {
    warning("changes in scheme file detected: new inputs form was added")
    warning("trying to adapt database")

    # add empty data for each new input form
    for (i in form_base_difference) {
      df_to_rewrite <- df_to_rewrite |>
        dplyr::mutate(!!dplyr::sym(i) := utils$get_empty_data(inputs_simple_list[i]))
    }

    # reorder due to scheme
    df_to_rewrite <- df_to_rewrite |>
      dplyr::select(dplyr::all_of(names(inputs_simple_list)))

    DBI::dbWriteTable(con, "main", df_to_rewrite, overwrite = TRUE)
    DBI::dbExecute(con, "DELETE FROM main WHERE id = 'dummy'")
  }

  if (length(names(inputs_simple_list)) < length(colnames(df_to_rewrite))) {
    stop("changes in scheme file detected: some of inputs form was deleted! it may cause data loss!")
  }
  # cleaning
  rm(df_to_rewrite, form_base_difference)
}

# close connection to prevent data loss
db$close_db_connection(con)


# INLINE TABLES =====================
# создаем для каждой таблицы объект
inline_tables <- purrr::map(
  .x = purrr::set_names(inputs_tables_list),
  .f = \(x_inline_table_name) {

    # получить имя файла со схемой
    file_name <- SCHEME_MAIN |>
      dplyr::filter(form_id == x_inline_table_name) |>
      dplyr::pull(choices)

    # load scheme
    schemaaa <- readxl::read_xlsx(fs::path(folder_with_schemas, file_name)) |>
      tidyr::fill(dplyr::everything(), .direction = "down")

    # список форм в схеме
    inline_forms <- schemaaa |>
      dplyr::distinct(form_id) |>
      dplyr::pull()

    # макет таблицы (пустой)
    DF_gen <- as.list(setNames(rep(as.character(NA), length(inline_forms)), inline_forms)) |>
      as.data.frame()

    # make 12 more empty rows
    DF_gen <- rbind(DF_gen, DF_gen[rep(1, 12), ])
    rownames(DF_gen) <- NULL

    list(schema = schemaaa, df_empty = DF_gen)
  }
)

# generate nav panels for each page
nav_panels_list <- purrr::map(
  .x = unique(SCHEME_MAIN$part),
  .f = \(page_name) {

    # отделить схему для каждой страницы
    this_page_panels_scheme <- SCHEME_MAIN |>
      dplyr::filter(part == {{page_name}})

    this_page_panels <- utils$make_panels(this_page_panels_scheme)

    # add panel wrap to nav_panel
    bslib::nav_panel(
      title = page_name,
      this_page_panels
    )
  }
)

# UI =======================
ui <- page_sidebar(
  title = config$header,
  theme = bs_theme(version = 5, preset = "bootstrap"),
  sidebar = sidebar(
    actionButton("save_data_button", "Сохранить данные", icon("floppy-disk", lib = "font-awesome")),
    actionButton("clean_data_button", "Очистить данные", icon("user-plus", lib = "font-awesome")),
    textOutput("status_message"),
    textOutput("status_message2"),
    actionButton("load_data_button", "Загрузить данные", icon("pencil", lib = "font-awesome")),
    downloadButton("downloadData", "Экспорт в .xlsx"),
    downloadButton("downloadDocx", "get .docx (test only)"),
    position = "left",
    open = list(mobile = "always")
  ),
  # list of rendered panels
  navset_card_underline(
    !!!nav_panels_list,
    header = NULL
  )
)

# MODALS ========================
# окно для подвтерждения очищения данных
modal_clean_all <- modalDialog(
  "Данное действие очистит все заполненные данные. Убедитесь, что нужные данные сохранены.",
  title = "Очистить форму?",
  footer = tagList(
    actionButton("cancel_button", "Отмена"),
    actionButton("clean_all_action", "Очистить.", class = "btn btn-danger")
  ),
  easyClose = TRUE
)

# окно для подвтерждения удаления
modal_overwrite <- modalDialog(
  "Запись с данным id уже существует в базе. Это действие перезапишет сохраненные ранее данные.",
  title = "Перезаписать данные?",
  footer = tagList(
    actionButton("cancel_button", "Отмена"),
    actionButton("data_save", "Перезаписать", class = "btn btn-danger")
  ),
  easyClose = TRUE
)

# окно для подвтерждения удаления
modal_load_patients <- modalDialog(
  "Загрузить данные",
  uiOutput("load_menu"),
  title = "Загрузить имеющиеся данные",
  footer = tagList(
    actionButton("cancel_button", "Отмена", class = "btn btn-danger"),
    actionButton("read_data", "Загрузить данные"),
  ),
  easyClose = TRUE
)

# init auth =======================
if (AUTH_ENABLED) ui <- shinymanager::secure_app(ui, enable_admin = TRUE)

# SERVER LOGIC =============================
server <- function(input, output) {
  # AUTH SETUP ========================================
  if (AUTH_ENABLED) {
    # check_credentials directly on sqlite db
    res_auth <- shinymanager::secure_server(
      check_credentials = check_credentials(
        db = "auth.sqlite",
        passphrase = Sys.getenv("AUTH_DB_KEY")
      ),
      keep_token = TRUE
    )
  }

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # REACTIVE VALUES =================================
  # Create a reactive values object to store the input data
  values <- reactiveValues(data = NULL)
  rhand_tables <- reactiveValues()

  # inline tables 2 ========================
  purrr::walk(
    .x = inputs_table_df$form_id,
    .f = \(table_name) {

      observeEvent(input[[table_name]], {

        this_inline_table2_info <- inputs_table_df |>
          dplyr::filter(form_id == {table_name})

        inline_table2_file_name <- this_inline_table2_info$choices

        this_inline_table2_scheme <- fs::path(folder_with_schemas, inline_table2_file_name) |>
          load_scheme_from_xlsx(colnames = c("form_id", "form_label", "form_type"))

        yay_its_fun <- purrr::pmap(
            .l = dplyr::distinct(this_inline_table2_scheme, form_id, form_label, form_type),
            .f = utils$render_forms,
            main_scheme = this_inline_table2_scheme
          )

        ui_for_inline_table <- card(
            height = "800px",
            layout_sidebar(
              sidebar = selectizeInput(
                inputId = "aboba",
                label = "key", 
                choices = c("a", "b")
              ),
              yay_its_fun
            )
          )
        
        showModal(modalDialog(
          ui_for_inline_table,
          # title = modalButton("Dismiss"),,
          footer = modalButton("Dismiss"),
          size = "l"
        ))

      })
    }
  )

  # VALIDATIONS ============================
  # create new validator
  iv <- shinyvalidate::InputValidator$new()

  # add rules to all inputs
  purrr::walk(
    .x = names(inputs_simple_list),
    .f = \(x_input_id) {
      form_type <- inputs_simple_list[[x_input_id]]

      choices <- dplyr::filter(SCHEME_MAIN, form_id == {{x_input_id}}) |>
        dplyr::pull(choices)

      val_required <- dplyr::filter(SCHEME_MAIN, form_id == {{x_input_id}}) |>
        dplyr::distinct(required) |>
        dplyr::pull(required)

      # for `number` type: if in `choices` column has values then parsing them to range validation
      # value `0; 250` -> transform to rule validation value from 0 to 250
      if (form_type == "number") {

        iv$add_rule(x_input_id, function(x) {
          # exit if empty
          if (check_for_empty_data(x)) {
            return(NULL)
          }
          # check for numeric
          # if (grepl("^[-]?(\\d*\\,\\d+|\\d+\\,\\d*|\\d+)$", x)) NULL else "Значение должно быть числом."
          if (grepl("^[+-]?\\d*[\\.|\\,]?\\d+$", x)) NULL else "Значение должно быть числом."
        })

        # проверка на соответствие диапазону значений
        if (!is.na(choices)) {
          # разделить на несколько елементов
          ranges <- as.integer(stringr::str_split_1(choices, "; "))

          # проверка на кол-во значений
          if (length(ranges) > 3) {
            warning("Количество переданных элементов'", x_input_id, "' > 2")
          } else {
            iv$add_rule(
              x_input_id,
              function(x) {

                # замена разделителя десятичных цифр
                x <- stringr::str_replace(x, ",", ".")

                # exit if empty
                if (check_for_empty_data(x)) {
                  return(NULL)
                }

                # check for currect value
                if (between(as.double(x), ranges[1], ranges[2])) {
                  NULL
                } else {
                  glue::glue("Значение должно быть между {ranges[1]} и {ranges[2]}.")
                }
              }
            )
          }
        }
      }

      # if in `required` column value is `1` apply standart validation
      if (!is.na(val_required) && val_required == 1) {
        iv$add_rule(x_input_id, shinyvalidate::sv_required(message = "Необходимо заполнить."))
      }
    }
  )
  # enable validator
  iv$enable()

  # STATUSES ===============================
  # вывести отображение что что-то не так
  output$status_message <- renderText({
    shiny::validate(
      need(input$id, "⚠️ Необходимо указать id пациента!")
    )
    paste0("ID: ", input$id)
  })

  output$status_message2 <- renderText({
    iv$is_valid()
    # res_auth$admin
  })

  # CREATE RHANDSOME TABLES =====================
  # записать массив пустых табличек в rhands_tables
  purrr::walk(
    .x = purrr::set_names(inputs_tables_list),
    .f = \(x_inline_table) {
      rhand_tables[[x_inline_table]] <- inline_tables[[x_inline_table]]$df_empty
    }
  )

  # render tables
  observe({
    # MESSAGE
    purrr::walk(
      .x = inputs_tables_list,
      .f = \(x) {
        # вытаскиваем схемы из заготовленного ранее списка
        schema <- inline_tables[[x]]$schema

        # убрать дубликаты
        schema_comp <- schema |>
          dplyr::distinct(form_id, form_label, form_type)

        # заголовки
        headers <- dplyr::pull(schema_comp, form_label)

        # fixes empty rows error
        rownames(rhand_tables[[x]]) <- NULL

        # создать объект рандсонтебл
        rh_tabel <- rhandsontable::rhandsontable(
          rhand_tables[[x]],
          colHeaders = headers,
          rowHeaders = NULL,
          height = 400,
        ) |>
          rhandsontable::hot_cols(
            colWidths = 120,
            manualColumnResize = TRUE,
            columnSorting = TRUE
          )

        # циклом итерируемся по индексу;
        for (i in seq(1, length(schema_comp$form_id))) {
          # получаем информацию о типе столбца
          type <- dplyr::filter(schema_comp, form_id == schema_comp$form_id[i]) |>
            dplyr::pull(form_type)

          # информация о воможных вариантнах выбора
          choices <- dplyr::filter(schema, form_id == schema_comp$form_id[i]) |>
            dplyr::pull(choices)

          ## проверки
          # текстовое поле
          if (type == "text") {
            rh_tabel <- rh_tabel |>
              hot_col(col = headers[i], type = "autocomplete")
          }

          # выбор из списка
          if (type == "select_one") {
            rh_tabel <- rh_tabel |>
              hot_col(col = headers[i], type = "dropdown", source = choices)
          }

          # дата
          if (type == "date") {
            rh_tabel <- rh_tabel |>
              hot_col(col = headers[i], type = "date", dateFormat = "DD.MM.YYYY", language = "ru-RU")
          }
        }

        # передаем в оутпут полученный объект
        output[[x]] <- renderRHandsontable({
          rh_tabel
        })
      }
    )
  })


  # BUTTONS LOGIC ======================
  ## clear all inputs ==================
  # show modal on click of button
  observeEvent(input$clean_data_button, {
    showModal(modal_clean_all)
  })

  # when action confirm - perform action
  observeEvent(input$clean_all_action, {
    # rewrite all inputs with empty data
    purrr::walk2(
      .x = inputs_simple_list,
      .y = names(inputs_simple_list),
      .f = \(x_type, x_id) {
        # using function to update forms
        utils$update_forms_with_data(
          id = x_id,
          type = x_type,
          value = utils$get_empty_data(x_type)
        )
      }
    )

    # inline tables
    purrr::walk(
      .x = inputs_tables_list,
      .f = \(x_table_name) {
        rhand_tables[[x_table_name]] <- inline_tables[[x_table_name]]$df_empty
      }
    )

    removeModal()
    showNotification("Данные очищены!", type = "warning")
  })

  ## saving inputs to db ========================
  # сохранить простые данные;
  observeEvent(input$save_data_button, {
    req(input$id)
    con <- db$make_db_connection("save_data_button")
    on.exit(db$close_db_connection(con, "save_data_button"), add = TRUE)

    ## MAIN
    # собрать все значения по введенным данным;
    result_df <- purrr::map(
      .x = names(inputs_simple_list),
      .f = \(x) {
        input_d <- input[[x]]

        # return empty if 0 element
        if (length(input_d) == 0) {
          return(utils$get_empty_data(inputs_simple_list[[x]]))
        }
        # return element if there one
        if (length(input_d) == 1) {
          return(input_d)
        }
        # если елементов больше одного - объединять через ";"
        if (length(input_d) > 1) paste(input_d, collapse = getOption("SYMBOL_DELIM"))
      }
    )

    # make dataframe from that;
    values$data <- setNames(result_df, names(inputs_simple_list)) %>%
      as_tibble()

    if (length(DBI::dbListTables(con)) == 0) {
      # если база пустая, то просто записываем данные
      write_all_to_db()
    } else if ("main" %in% DBI::dbListTables(con)) {
      # если главная таблица существует, то проверяем существование id

      # GET DATA files
      query <- glue::glue_sql("
        SELECT DISTINCT id
        FROM main
        WHERE id = {input$id}
        ", .con = con)

      # получаем список записей с данным id
      exist_main_df <- DBI::dbGetQuery(con, query)

      # проверка по наличию записей с данным ID в базе;
      if (nrow(exist_main_df) == 0) {
        # если данных нет - просто записать данные
        log_action_to_db("save", input$id, con)
        write_all_to_db()
      } else {
        # если есть выдать окно с подтверждением перезаписи
        showModal(modal_overwrite)
      }
    }
  })

  ## get list of id's from db =====================
  observeEvent(input$load_data_button, {
    con <- db$make_db_connection("load_data_button")
    on.exit(db$close_db_connection(con, "load_data_button"))

    if (length(dbListTables(con)) != 0 && "main" %in% DBI::dbListTables(con)) {
      # GET DATA files
      ids <- DBI::dbGetQuery(con, "SELECT DISTINCT id FROM main") %>%
        pull()

      output$load_menu <- renderUI({
        selectizeInput(
          inputId = "read_id_selector",
          label = NULL,
          choices = ids,
          selected = NULL,
          options = list(
            placeholder = "id пациента",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })
    } else {
      output$load_menu <- renderUI({
        h5("База данных не содержит записей")
      })
    }

    shiny::showModal(modal_load_patients)
  })

  ## load data to input forms ==================================
  observeEvent(input$read_data, {
    con <- db$make_db_connection("read_data")
    on.exit(db$close_db_connection(con, "read_data"), add = TRUE)

    # main df read
    test_read_df <- read_df_from_db_by_id("main", con)

    # transform df to list
    test_read_df <- as.list(test_read_df)

    # rewrite input forms
    purrr::walk2(
      .x = inputs_simple_list,
      .y = names(inputs_simple_list),
      .f = \(x_type, x_id) {
        if (getOption("APP.DEBUG")) {
          values_load <- test_read_df[[x_id]]
          print(paste(x_type, x_id, values_load, sep = " || "))
          print(is.na(values_load))
        }

        # updating forms with loaded data
        utils$update_forms_with_data(
          id = x_id,
          type = x_type,
          value = test_read_df[[x_id]]
        )
      }
    )

    # inline tables
    purrr::walk(
      .x = inputs_tables_list,
      .f = \(x_table_name) {
        test_read_df <- read_df_from_db_by_id(x_table_name, con)

        # если табличечки не пустые загружаем их
        if (!is.null(test_read_df) && nrow(test_read_df) != 0) {
          rhand_tables[[x_table_name]] <- subset(test_read_df, select = c(-id))
        } else {
          rhand_tables[[x_table_name]] <- inline_tables[[x_table_name]]$df_empty
        }
      }
    )
    removeModal()
    showNotification("Данные загружены!", type = "warning")
    message("load data")
    log_action_to_db("load", input$read_id_selector, con = con)
  })

  ## export to .xlsx ====
  output$downloadData <- downloadHandler(
    filename = paste0("d2tra_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      con <- db$make_db_connection("downloadData")
      on.exit(db$close_db_connection(con, "downloadData"), add = TRUE)

      # get all data
      list_of_df <- purrr::map(
        .x = purrr::set_names(c("main", inputs_tables_list)),
        .f = \(x) {
          df <- read_df_from_db_all(x, con) %>%
            tibble::as_tibble()

          # handle with data
          if (nrow(df) >= 1 && x == "main") {
            df <- df %>%
              dplyr::mutate(dplyr::across(dplyr::contains("date"), as.Date)) %>%
              print()
          }
          df
        }
      )
      # set date params
      options("openxlsx2.dateFormat" = "dd.mm.yyyy")

      print("DATA EXPORTED")
      log_action_to_db("export db", con = con)

      # pass tables to export
      openxlsx2::write_xlsx(
        purrr::compact(list_of_df),
        file,
        na.strings = "",
        as_table = TRUE,
        col_widths = 15
      )
    }
  )

  ## export to .docx ====
  output$downloadDocx <- downloadHandler(
    filename = function() {
      paste0(input$id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      # prepare YAML sections
      empty_vec <- c(
        "---",
        "output:",
        "  word_document:",
        "    reference_docx: reference.docx",
        "---",
        "\n"
      )

      # iterate by scheme parts
      purrr::walk(
        .x = unique(SCHEME_MAIN$part),
        .f = \(x_iter1) {
          # write level 1 header
          HEADER_1 <- paste("#", x_iter1, "\n")
          empty_vec <<- c(empty_vec, HEADER_1)

          # iterate by level2 headers (subgroups)
          purrr::walk(
            .x = dplyr::pull(unique(subset(SCHEME_MAIN, part == x_iter1, "subgroup"))),
            .f = \(x_iter2) {
              # get header 2 name
              HEADER_2 <- paste("##", x_iter2, "\n")

              # for some reason set litle scheme...
              litle_scheme <- subset(
                x = SCHEME_MAIN,
                subset = part == x_iter1 & subgroup == x_iter2,
                select = c("form_id", "form_label", "form_type")
              ) |>
                unique()

              # iterate by id in subgroups
              VALUES <- purrr::map_chr(
                .x = litle_scheme$form_id,
                .f = \(x_id) {
                  docx_type <- subset(litle_scheme, form_id == x_id, "form_type")
                  docx_label <- subset(litle_scheme, form_id == x_id, "form_label")

                  # logic for render documemts
                  if (docx_type %in% c("text", "number", "date", "select_one", "select_multiple", "radio", "checkbox")) {
                    docx_value <- input[[x_id]]

                    # if more than two objects: collapse
                    if (length(docx_value) > 1) docx_value <- paste(docx_value, collapse = ", ")

                    # if non empty data - add string
                    if (!check_for_empty_data(docx_value)) paste0("**", docx_label, "**: ", docx_value, "\n") else NA
                  } else if (docx_type == "description") {
                    # treat description label as citation text
                    paste0(">", docx_label, "\n")
                  } else {
                    paste0(docx_label, ": ", "NOT IMPLEMENTED YET", "\n")
                  }
                }
              )
              # append to vector parsed data
              empty_vec <<- (c(empty_vec, HEADER_2, VALUES))
            }
          )
        }
      )

      # set temp folder and names
      temp_folder <- tempdir()
      temp_report <- file.path(temp_folder, "rmarkdown_output.Rmd")
      temp_template <- file.path(temp_folder, "reference.docx")

      # clean from NA strings
      empty_vec <- empty_vec[!is.na(empty_vec)]

      # write vector to temp .Rmd file
      writeLines(empty_vec, temp_report, sep = "\n")
      # copy template .docx file
      file.copy("references/reference.docx", temp_template, overwrite = TRUE)

      # render file via pandoc
      rmarkdown::render(
        temp_report,
        output_file = file,
        output_format = "word_document",
        envir = new.env(parent = globalenv())
      )
    }
  )

  ## trigger saving function =============
  observeEvent(input$data_save, {
    con <- db$make_db_connection("saving data (from modal conf)")
    on.exit(db$close_db_connection(con, "saving data (from modal conf)"), add = TRUE)

    # убираем плашку
    removeModal()

    # записываем данные
    write_all_to_db()
    log_action_to_db("overwrite", input$id, con = con)
  })

  ## cancel ==========================
  observeEvent(input$cancel_button, {
    # убираем плашку
    removeModal()
  })

  # FUNCTIONS ==============================
  ## write all inputs to db ================
  write_all_to_db <- function() {
    con <- db$make_db_connection("fn call `write_all_to_db()`")
    # on.exit(close_db_connection("fn call `write_all_to_db()`"), add = TRUE)

    # write main
    write_df_to_db(values$data, "main", con)

    # write inline tables
    for (i in inputs_tables_list) {
      df <- tryCatch(
        # проверка выражения
        expr = {
          hot_to_r(input[[i]])
        },
        # действия в случае ошибки
        error = function(e) {
          message(e$message)
          showNotification(
            glue::glue("Невозможно сохранить таблицу `{i}`! Данная ошибка может возникать в случае, если в таблице находятся пустые строки. Попробуйте удалить пустые строки и повторить сохранение."), # nolint
            duration = NULL,
            closeButton = FALSE,
            id = paste0(i, "error_inline_tables"),
            type = "error"
          )
          tibble()
        }
      )

      df <- df %>%
        dplyr::as_tibble() %>%
        janitor::remove_empty(which = c("rows")) %>%
        # adding id to dbs
        dplyr::mutate(id = input$id, .before = 1)

      # если таблица содержит хоть одну строку - сохранить таблицу в базу данных
      if (nrow(df) != 0) {
        write_df_to_db(df, i, con)
        removeNotification(paste0(i, "error_inline_tables"))
      }
    }

    showNotification(
      glue::glue("Данные пациента {input$id} сохранены!"),
      type = "warning"
    )
  }

  ## helper function writing dbs ========
  write_df_to_db <- function(df, table_name, con) {
    # disconnecting on parent function

    # delete exists data for this id
    if (table_name %in% dbListTables(con)) {
      del_query <- glue::glue("DELETE FROM {table_name} WHERE id = '{input$id}'")
      DBI::dbExecute(con, del_query)
    }
    # записать данные
    DBI::dbWriteTable(con, table_name, df, append = TRUE)
  }

  ## reading tables from db by name and id ========
  read_df_from_db_by_id <- function(table_name, con) {
    # DBI::dbConnect(RSQLite::SQLite(), dbfile)
    # on.exit(DBI::dbDisconnect(con), add = TRUE)

    # check if this table exist
    if (table_name %in% dbListTables(con)) {
      # prepare query
      query <- glue::glue("
        SELECT * FROM {table_name}
        WHERE id = '{input$read_id_selector}'
      ")

      # get table as df
      DBI::dbGetQuery(con, query)
    }
  }

  ## reading tables from db all ========
  read_df_from_db_all <- function(table_name, con) {
    # DBI::dbConnect(RSQLite::SQLite(), dbfile)
    # on.exit(DBI::dbDisconnect(con), add = TRUE)

    # check if this table exist
    if (table_name %in% dbListTables(con)) {
      # prepare query
      query <- glue::glue("
        SELECT * FROM {table_name}
      ")

      # get table as df
      DBI::dbGetQuery(con, query)
    }
  }

  ## LOGGING ACTIONS
  log_action_to_db <- function(action, pat_id = as.character(NA), con) {
    # DBI::dbConnect(RSQLite::SQLite(), dbfile)
    # on.exit(DBI::dbDisconnect(con), add = TRUE)

    action_row <- tibble(
      user = ifelse(AUTH_ENABLED, res_auth$user, "anonymous"),
      action = action,
      id = pat_id,
      date = Sys.time()
    )
    DBI::dbWriteTable(con, "log", action_row, append = TRUE)
  }
}

options(shiny.port = config$shiny_port)
options(shiny.host = config$shiny_host)

app <- shinyApp(ui = ui, server = server)

runApp(app, launch.browser = TRUE)
