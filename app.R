suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(tibble)
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

# SOURCE FILES ============================
config <- config::get(file = "configs/config.yml")

folder_with_schemas <- fs::path("configs/schemas")
FILE_SCHEME         <- fs::path(folder_with_schemas, "main.xlsx")
dbfile              <- fs::path("data.sqlite")
DEBUG <- FALSE

# TEMP ! NEED TO HANDLE
rmarkdown::find_pandoc(dir = "/opt/homebrew/bin/")

# CHECK FOR PANDOC
# TODO: dynamic button render depend on pandoc installation
if (!rmarkdown::pandoc_available()) warning("Can't find pandoc!")


# SCHEME_MAIN UNPACK ==========================
# load scheme
SCHEME_MAIN <- readxl::read_xlsx(FILE_SCHEME) %>%
  # fill NA down
  fill(c(part, subgroup, form_id, form_label, form_type), .direction = "down") %>%
  group_by(form_id) %>%
  fill(condition, .direction = "down") %>%
  ungroup()

# get list of simple inputs
inputs_simple_list <- SCHEME_MAIN %>%
  filter(!form_type %in% c("inline_table", "description")) %>%
  distinct(form_id, form_type) %>%
  deframe

# get list of inputs with inline tables
inputs_tables_list <- SCHEME_MAIN %>%
  filter(form_type == "inline_table") %>%
  distinct(form_id) %>%
  deframe


# SETUP DB ==========================
#' @description Function to open connection to db, disigned to easy dubugging.
make_db_connection <- function(where = "") {
  if (DEBUG) message("=== DB CONNECT ", where)
  DBI::dbConnect(RSQLite::SQLite(), dbfile)
}

#' @description Function to close connection to db, disigned to easy dubugging and
#' hide warnings.
close_db_connection <- function(where = "") {
  tryCatch(
    expr = DBI::dbDisconnect(con),
    error = function(e) print(e),
    warning = function(w) if (DEBUG) message("=!= ALREADY DISCONNECTED ", where),
    finally = if (DEBUG) message("=/= DB DISCONNECT ", where)
  )
}

# establish connection
con <- make_db_connection()

# init DB (write dummy data to "main" table)
if (!"main" %in% dbListTables(con)) {

  dummy_df <- mutate(get_dummy_df(), id = "dummy")

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
      df_to_rewrite <- df_to_rewrite %>%
        mutate(!!sym(i) := get_empty_data(inputs_simple_list[i]))
    }

    # reorder due to scheme
    df_to_rewrite <- df_to_rewrite %>%
      select(all_of(names(inputs_simple_list)))

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
close_db_connection()


# INLINE TABLES =====================
# создаем для каждой таблицы объект
inline_tables <- purrr::map(
  .x = purrr::set_names(inputs_tables_list),
  .f = \(x_inline_table_name) {

    # получить имя файла со схемой
    file_name <- SCHEME_MAIN %>%
      filter(form_id == x_inline_table_name) %>%
      pull(choices)

    # load scheme
    schemaaa <- readxl::read_xlsx(fs::path(folder_with_schemas, file_name)) %>%
      fill(everything(), .direction = "down")

    # список форм в схеме
    inline_forms <- schemaaa %>%
      distinct(form_id) %>%
      pull

    # макет таблицы (пустой)
    DF_gen <- as.list(setNames(rep(as.character(NA), length(inline_forms)), inline_forms)) |>
      as.data.frame()

    # make 12 more empty rows
    DF_gen <- rbind(DF_gen, DF_gen[rep(1, 12), ])
    rownames(DF_gen) <- NULL

    list(schema = schemaaa, df_empty = DF_gen)
  }
)


# создание объектов для ввода
# функция
create_forms <- function(form_id, form_label, form_type) {

  # check if have condition
  condition <- filter(SCHEME_MAIN, form_id == {{form_id}}) %>% distinct(condition) %>% pull
  choices   <- filter(SCHEME_MAIN, form_id == {{form_id}}) %>% pull(choices)

  # simple text or number input
  if (form_type %in% c("text", "number")) {
    form <- shiny::textAreaInput(
      inputId = form_id,
      label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
      rows = 1
    )
  }

  # simple date input
  if (form_type == "date") {
    # supress warning while trying keep data form empty by default
    suppressWarnings({
      form <- dateInput(
        inputId = form_id,
        label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
        value = NA, # keep empty
        format = "dd.mm.yyyy",
        weekstart = 1,
        language = "ru"
      )
    })
  }

  # еденичный выбор
  if (form_type == "select_one") {
    form <- selectizeInput(
      inputId = form_id,
      label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
      choices = choices,
      selected = NULL,
      options = list(
        create = FALSE,
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }

  # множественный выбор
  if (form_type == "select_multiple") {
    form <- selectizeInput(
      inputId = form_id,
      label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      options = list(
        create = FALSE,
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  }

  # множественный выбор
  if (form_type == "radio") {
    form <- radioButtons(
      inputId = form_id,
      label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
      choices = choices,
      selected = character(0)
    )
  }

  if (form_type == "checkbox") {
    form <- checkboxGroupInput(
      inputId = form_id,
      # label = tags$span(style = "color: #444444; font-weight: 550;", form_label),
      label = h6(form_label),
      choices = choices,
      selected = character(0)
    )
  }

  # вложенная таблица
  if (form_type == "inline_table") {
    form <- rHandsontableOutput(outputId = form_id)
  }

  # description part
  if (form_type == "description") {
    form <- div(HTML(form_label), style = "color:Gray;font-size: 90%;")
  }

  # если есть условие создать кондитионал панель
  if (!is.na(condition)) {
    form <- conditionalPanel(
      condition = condition,
      form
    )
  }

  form
}



# GENERATE UI ==================================
# functions for making cards
make_cards_fn <- function(sub_group) {

  subgroups_inputs <- df_forms %>%
    filter(subgroup == {{sub_group}}) %>%
    distinct(form_id, form_label, form_type)

  card(
    card_header(sub_group, container = htmltools::h5),
    full_screen = TRUE,
    width = "4000px",
    card_body(
      fill = TRUE,
      # передаем все аргументы в функцию для создания елементов
      purrr::pmap(subgroups_inputs, create_forms)
    )
  )
}

# get pages list
pages_list <- unique(SCHEME_MAIN$part)

# get all forms df
df_forms <- SCHEME_MAIN %>%
  distinct(part, subgroup, form_id, form_label, form_type)

# generate nav panels
nav_panels_list <- purrr::map(
  .x = pages_list,
  .f = \(x_page) {

    # get info about inputs for current page
    page_forms <- SCHEME_MAIN %>%
      filter(part == {{x_page}}) %>%
      distinct(subgroup, form_id, form_label, form_type)

    # get list of columns
    cols_list <- unique(page_forms$subgroup)

    # making cards
    cards <- purrr::map(
      .x = cols_list,
      .f = make_cards_fn
    )

    # make page wrap
    page_wrap <- layout_column_wrap(
      width = "350px", height = NULL, #was 800
      fixed_width = TRUE,
      !!!cards # unpack list of cards
    )

    # add panel wrap to nav_panel
    nav_panel(x_page, page_wrap)
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
    downloadButton("downloadDocx", "get .docx (test only)")
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
ui <- shinymanager::secure_app(ui, enable_admin = TRUE)

# SERVER LOGIC =============================
server <- function(input, output) {

  # AUTH SETUP ========================================
  # check_credentials directly on sqlite db
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials(
      db = "auth.sqlite",
      passphrase = Sys.getenv("AUTH_DB_KEY")
    ),
    keep_token = TRUE
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # REACTIVE VALUES =================================
  # Create a reactive values object to store the input data
  values       <- reactiveValues(data = NULL)
  rhand_tables <- reactiveValues()

  # VALIDATIONS ============================
  # create new validator
  iv <- shinyvalidate::InputValidator$new()

  # add rules to all inputs
  purrr::walk(
    .x = names(inputs_simple_list),
    .f = \(x_input_id) {

      form_type    <- inputs_simple_list[[x_input_id]]
      choices      <- filter(SCHEME_MAIN, form_id == {{x_input_id}}) %>% pull(choices)
      val_required <- filter(SCHEME_MAIN, form_id == {{x_input_id}}) %>% distinct(required) %>% pull(required)

      # for `number` type: if in `choices` column has values then parsing them to range validation
      # value `0; 250` -> transform to rule validation value from 0 to 250
      if (form_type == "number") {
        iv$add_rule(x_input_id, function(x) {
          # exit if empty
          if (check_for_empty_data(x)) return(NULL)
          # check for numeric
          if (grepl("^[-]?(\\d*\\,\\d+|\\d+\\,\\d*|\\d+)$", x)) NULL else "Значение должно быть числом."
        })

        if (!is.na(choices)) {
          # разделить на числа
          ranges <- as.integer(stringr::str_split_1(choices, "; "))

          # проверка на кол-во значений
          if (length(ranges) > 3) {
            warning("Количество переданных элементов'", x_input_id, "' > 2")
          } else {
            iv$add_rule(
              x_input_id,
              function(x) {
                # exit if empty
                if (check_for_empty_data(x)) return(NULL)
                # check for currect value
                if (between(as.integer(x), ranges[1], ranges[2])) {
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
        schema_comp <- schema %>%
          distinct(form_id, form_label, form_type)

        # заголовки
        headers <- pull(schema_comp, form_label)

        # fixes empty rows error
        rownames(rhand_tables[[x]]) <- NULL

        # создать объект рандсонтебл
        rh_tabel <- rhandsontable(
          rhand_tables[[x]],
          colHeaders = headers,
          rowHeaders = NULL,
          height = 400,
        ) %>%
          hot_cols(colWidths = 120, manualColumnResize = TRUE, columnSorting = TRUE)

        # циклом итерируемся по индексу;
        for (i in seq(1, length(schema_comp$form_id))) {

          # получаем информацию о типе столбца
          type <- filter(schema_comp, form_id == schema_comp$form_id[i]) %>% pull(form_type)

          # информация о воможных вариантнах выбора
          choices <- filter(schema, form_id == schema_comp$form_id[i]) %>% pull(choices)

          ## проверки
          # текстовое поле
          if (type == "text") {
            rh_tabel <- rh_tabel %>%
              hot_col(col = headers[i], type = "autocomplete")
          }

          # выбор из списка
          if (type == "select_one") {
            rh_tabel <- rh_tabel %>%
              hot_col(col = headers[i], type = "dropdown", source = choices)
          }

          # дата
          if (type == "date") {
            rh_tabel <- rh_tabel %>%
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
        update_forms_with_data(
          id = x_id,
          type = x_type,
          value = get_empty_data(x_type)
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
    con <- make_db_connection("save_data_button")
    on.exit(close_db_connection("save_data_button"), add = TRUE)

    ## MAIN
    # собрать все значения по введенным данным;
    result_df <- purrr::map(
      .x = names(inputs_simple_list),
      .f = \(x) {
        type    <- inputs_simple_list[[x]]
        input_d <- input[[x]]

        # return empty if 0 element
        if (length(input_d) == 0) return(get_empty_data(type))
        # return element if there one
        if (length(input_d) == 1) return(input_d)
        # если елементов больше одного - объединять через ";"
        if (length(input_d) > 1) paste(input_d, collapse = "; ")
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
    con <- make_db_connection("load_data_button")
    on.exit(close_db_connection("load_data_button"))

    if (length(dbListTables(con)) != 0 && "main" %in% DBI::dbListTables(con)) {
      # GET DATA files
      ids <- DBI::dbGetQuery(con, "SELECT DISTINCT id FROM main") %>%
        pull

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
    con <- make_db_connection("read_data")
    on.exit(close_db_connection("read_data"), add = TRUE)

    # main df read
    test_read_df <- read_df_from_db_by_id("main", con)

    # transform df to list
    test_read_df <- as.list(test_read_df)

    # rewrite input forms
    purrr::walk2(
      .x = inputs_simple_list,
      .y = names(inputs_simple_list),
      .f = \(x_type, x_id) {

        if (DEBUG) {
          values_load <- test_read_df[[x_id]]
          print(paste(x_type, x_id, values_load, sep = " || "))
          print(is.na(values_load))
        }

        # using function to update forms
        update_forms_with_data(
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
      con <- make_db_connection("downloadData")
      on.exit(close_db_connection("downloadData"), add = TRUE)

      # get all data
      list_of_df <- purrr::map(
        .x = purrr::set_names(c("main", inputs_tables_list)),
        .f = \(x) {
          df <- read_df_from_db_all(x, con) %>%
            tibble::as_tibble()

          # handle with data
          if (nrow(df) >= 1 && x == "main") {
            df <- df %>%
              mutate(across(contains("date"), as.Date)) %>%
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
            .x = pull(unique(subset(SCHEME_MAIN, part == x_iter1, "subgroup"))),
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

                  docx_type  <- subset(litle_scheme, form_id == x_id, "form_type")
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
      temp_folder   <- tempdir()
      temp_report   <- file.path(temp_folder, "rmarkdown_output.Rmd")
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
    con <- make_db_connection("saving data (from modal conf)")
    on.exit(close_db_connection("saving data (from modal conf)"), add = TRUE)

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
    con <- make_db_connection("fn call `write_all_to_db()`")
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
        as_tibble() %>%
        janitor::remove_empty(which = c("rows")) %>%
        # adding id to dbs
        mutate(id = input$id, .before = 1)

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
      user = res_auth$user,
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