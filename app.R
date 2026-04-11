suppressPackageStartupMessages({
  library(DBI)
  library(tidyr)
  library(dplyr)
  library(purrr)
  library(magrittr)
  library(shiny)
  library(bslib)
  library(shinymanager)
})

source("helpers/functions.R")
source("helpers/scheme_generator.R")

# box::purge_cache()
# box::use(./helpers/db)

# SOURCE FILES ============================
FILE_SCHEME <- fs::path("configs/schemas", "schema.xlsx")
HEADER_TEXT <- sprintf("%s (%s)", Sys.getenv("FORM_TITLE", "NA"), Sys.getenv("FORM_VERSION", "NA"))

box::purge_cache()
box::use(
  modules/utils,
  modules/global_options,
  modules/db,
  modules/data_validation
)

global_options$set_global_options(
  shiny.host = "0.0.0.0"
)

# SETTINGS ================================
AUTH_ENABLED <- Sys.getenv("FORM_AUTH_ENABLED", FALSE)

# CHECK FOR PANDOC
# TEMP ! NEED TO HANDLE
rmarkdown::find_pandoc(dir = "/opt/homebrew/bin/")

# TODO: dynamic button render depend on pandoc installation
if (!rmarkdown::pandoc_available()) warning("Can't find pandoc!")

# SCHEME_MAIN UNPACK ==========================
schm <- scheme_R6$new(FILE_SCHEME)
object.size(schm)
schm$get_key_id("main")
schm$get_forms_ids("main")
schm$get_all_ids("main")

schm$get_schema("main")

schm$get_id_type_list("allergo_anamnesis")

# active
schm$get_main_key_id
schm$all_tables_names
# ----------------------------

# establish connection
con <- db$make_db_connection()

# init DB (write dummy data to "main" table)
# db$check_if_table_is_exist_and_init_if_not("main", main_id_and_types_list)
db$check_if_table_is_exist_and_init_if_not(schm, con)

# close connection to prevent data loss
db$close_db_connection(con)

# generate nav panels for each page
nav_panels_list <- purrr::map(
  .x = unique(schm$get_schema("main")$part),
  .f = \(page_name) {

    # отделить схему для каждой страницы
    this_page_panels_scheme <- schm$get_schema("main") |>
      dplyr::filter(!form_id %in% schm$get_main_key_id) |>
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
  title = HEADER_TEXT,
  theme = bs_theme(version = 5, preset = "bootstrap"),
  sidebar = sidebar(
    actionButton("add_new_main_key_button", "Добавить новую запись", icon("plus", lib = "font-awesome")),
    actionButton("save_data_button", "Сохранить данные", icon("floppy-disk", lib = "font-awesome")),
    actionButton("clean_data_button", "Очистить данные", icon("user-plus", lib = "font-awesome")),
    actionButton("load_data_button", "Загрузить данные", icon("pencil", lib = "font-awesome")),
    downloadButton("downloadDocx", "get .docx (test only)"),
    textOutput("status_message"),
    textOutput("status_message2"),
    uiOutput("admin_buttons_panel"),
    uiOutput("display_log"),
    position = "left",
    open = list(mobile = "always")
  ),
  as_fill_carrier(uiOutput("main_ui_navset"))
)

# MODALS ========================
# окно для подвтерждения очищения данных

# init auth =======================
if (AUTH_ENABLED) {
  # shinymanager::set_labels("en", "Please authenticate" = "aboba")
  ui <- ui |> 
    shinymanager::secure_app(
      status = "primary",
      tags_top = tags$div(
        tags$h3(HEADER_TEXT, style = "align:center"),
        # tags$img(
        #   src = "https://www.r-project.org/logo/Rlogo.png", width = 100
        # )
      ), 
      tags_bottom = tags$div(
        tags$p(
          "For any question, please  contact ",
          tags$a(
            href = "mailto:someone@example.com?Subject=Shiny%20aManager",
            target="_top", "administrator"
          )
        )
      ),
      enable_admin = TRUE,
      language = "en"
    )
}

# SERVER LOGIC =============================
server <- function(input, output, session) {
  
  # AUTH SETUP ========================================
  res_auth <- if (AUTH_ENABLED) {
    # check_credentials directly on sqlite db
    shinymanager::secure_server(
      check_credentials = check_credentials(
        db = "auth.sqlite",
        passphrase = Sys.getenv("AUTH_DB_KEY")
      ),
      keep_token = TRUE
    )
  } else {
    NULL
  }

  output$admin_buttons_panel <- renderUI({

    showing_buttons <- TRUE

    if (AUTH_ENABLED) {
      reactiveValuesToList(res_auth)
      if (res_auth$admin) {
        print("admin")
      } else {
        print("not_admin")
        showing_buttons <- FALSE
      }
    }

    if (showing_buttons) {
      fluidRow(
        downloadButton("downloadData", "Экспорт в .xlsx"),
        p(""), # separate buttons
        actionButton("button_upload_data_from_xlsx", "импорт!", icon("file-import", lib = "font-awesome"))
      )
    }
  })

  # REACTIVE VALUES =================================
  # Create a reactive values object to store the input data
  values <- reactiveValues(
    data     = NULL,
    main_key = NULL,
    nested_key = NULL,
    nested_form_id = NULL
  )

  # VALIDATIONS ============================
  # create new validator
  # TODO: как осуществить инициализацию валидатора после
  iv_main <- data_validation$init_val(schm$get_schema("main"))
  iv_main$enable()

  # динамический рендеринг --------------------------
  output$main_ui_navset <- renderUI({

    shiny::validate(
      need(values$main_key, "Для начала работы нужно создать новую запись или загрузить существующую!")
    )

    # list of rendered panels
    navset_card_underline(
      id = "main",
      !!!nav_panels_list,
      header = NULL,
      height = NULL
    )
  })

  # ==========================================
  # ОБЩИЕ ФУНКЦИИ ============================
  # ==========================================

  ## перенос данных из датафрейма в форму -----------------------
  load_data_to_form <- function(
    df,
    table_name = "main",
    schm,
    ns
  ) {

    input_types <- unname(schm$get_id_type_list(table_name))
    input_ids   <- names(schm$get_id_type_list(table_name))
    if (missing(ns)) ns <- NULL

    # transform df to list
    loaded_df_for_id <- as.list(df)
    loaded_df_for_id <- df[input_ids]

    # rewrite input forms
    purrr::walk2(
      .x = input_types,
      .y = input_ids,
      .f = \(x_type, x_id) {

        # updating forms with loaded data
        utils$update_forms_with_data(
          form_id = x_id,
          form_type = x_type,
          value = df[[x_id]],
          scheme = schm$get_schema(table_name),
          ns = ns
        )
      }
    )
  }

  ## сохранение данных из форм в базу данных --------
  save_inputs_to_db <- function(
    table_name, 
    id_and_types_list, 
    ns, 
    con
  ) {

    nested_key_id <- schm$get_key_id(table_name)
    input_types <- unname(id_and_types_list)
    input_ids   <- names(id_and_types_list)

    if (missing(ns)) ns <- NULL

    # собрать все значения по введенным данным;
    exported_values <- purrr::map2(
      .x = input_ids,
      .y = input_types,
      .f = \(x_id, x_type) {

        if (!is.null(ns)) x_id <- ns(x_id)
        input_d <- input[[x_id]]

        # return empty if 0 element
        if (length(input_d) == 0) {
          return(utils$get_empty_data(x_type))
        }

        # return element if there one
        if (length(input_d) == 1) {
          return(input_d)
        }

        # если елементов больше одного - объединять через ";"
        if (length(input_d) > 1) paste(input_d, collapse = getOption("SYMBOL_DELIM"))
      }
    )

    exported_df <- setNames(exported_values, input_ids) |>
        as_tibble()

    # пайплайн для главной таблицы
    if (table_name == "main") {
      exported_df <- exported_df |>
        mutate(
          !!dplyr::sym(schm$get_main_key_id) := values$main_key,
          .before = 1
        )
    }

    # для всех остальных таблицы (вложенные)
    if (table_name != "main") {
      exported_df <- exported_df |>
        mutate(
          !!dplyr::sym(schm$get_main_key_id) := values$main_key,
          !!dplyr::sym(nested_key_id) := values$nested_key,
          .before = 1
        )
    }

    # если данных нет - просто записать данные
    log_action_to_db("saving data", values$main_key, con)

    db$write_df_to_db(
      df = exported_df,
      table_name = table_name,
      schm = schm,
      main_key_value = values$main_key,
      nested_key_value = values$nested_key,
      con = con
    )
  }

  # ====================================
  # NESTED FORMS =======================
  # ====================================
  ## кнопки для каждой вложенной таблицы -------------------------------
  purrr::walk(
    .x = schm$nested_tables_names,
    .f = \(nested_form_id) {

      observeEvent(input[[nested_form_id]], {
        req(values$main_key)

        con <- db$make_db_connection("nested_tables")
        on.exit(db$close_db_connection(con, "nested_tables"), add = TRUE)

        values$nested_form_id <- nested_form_id
        values$nested_key     <- NULL # для нормальной работы реактивных значений
        show_modal_for_nested_form(con)

      })
    }
  )

  ## функция отображения вложенной формы для выбранной таблицы --------
  show_modal_for_nested_form <- function(con) {

    ns <- NS(values$nested_form_id)
    key_id <- schm$get_key_id(values$nested_form_id)

    # загрузка схемы для данной вложенной формы
    this_nested_form_scheme <-  schm$get_schema(values$nested_form_id)

    # мини-схема для ключа
    this_nested_form_key_scheme <- subset(this_nested_form_scheme, form_id == key_id)
    if (nrow(this_nested_form_key_scheme) > 1) cli::cli_abort("количество строк не может быть больше одного для ключа")

    # выбираем все ключи из баз данных
    kyes_for_this_table <- db$get_nested_keys_from_table(values$nested_form_id, schm, values$main_key, con)
    kyes_for_this_table <- unique(c(values$nested_key, kyes_for_this_table))
    kyes_for_this_table <- sort(kyes_for_this_table)
    values$nested_key <- if (length(kyes_for_this_table) == 0) NULL else kyes_for_this_table[[1]]

    # если ключ в формате даты - дать человекочитаемые данные
    if (this_nested_form_key_scheme$form_type == "date") {
      kyes_for_this_table <- setNames(
        kyes_for_this_table,
        format(as.Date(kyes_for_this_table), "%d.%m.%Y")
      )
    }

    # nested ui
    if (!is.null(values$nested_key)) {
      yay_its_fun <- purrr::map(
        .x = unique(this_nested_form_scheme$subgroup),
        .f = \(subgroup) {

          subroup_scheme <- this_nested_form_scheme |>
            dplyr::filter(subgroup == {{subgroup}}) |>
            dplyr::filter(form_id != key_id)

          bslib::nav_panel(
            title = subgroup,
            purrr::pmap(
              .l = dplyr::distinct(subroup_scheme, form_id, form_label, form_type),
              .f = utils$render_forms,
              main_scheme = subroup_scheme,
              ns = ns
            )
          )
        }
      )
    } else {
      yay_its_fun <- list(bslib::nav_panel("", "empty"))
    }
    # yay_its_fun <- !!!yay_its_fun

    # ui для всплывающего окна 
    ui_for_inline_table <- navset_card_underline(
      sidebar = sidebar(
        width = 300,
        selectizeInput(
          inputId = "nested_key_selector",
          label = this_nested_form_key_scheme$form_label, 
          choices = kyes_for_this_table,
          selected = values$nested_key,
          # options = list(placeholder = "действие комиссии", create = FALSE, onInitialize = I('function() { this.setValue(""); }'))
        ),
        actionButton("add_new_nested_key_button", "add"),
        actionButton("nested_form_save_button", "save"),
        actionButton("nested_form_dt_button", "dt"),
        open = list(mobile = "always-above")
      ),
      # if (!is.null(values$nested_key)) {rlang::syms(!!!yay_its_fun)} else bslib::nav_panel("empty")
      !!!yay_its_fun
    )

    # проверка данных для внутренних таблиц
    iv_inner <- data_validation$init_val(this_nested_form_scheme, ns)
    iv_inner$enable()

    showModal(modalDialog(
      ui_for_inline_table,
      footer = actionButton("close_modal_button", "Закрыть"),
      size = "l"
    ))
  }

  ## DT (nested) ---------------------------------
  ### функция для отображения DT-таблицы для выбранной вложенной формы --------
  show_modal_for_nested_form_dt <- function(con) {

    key_id <- schm$get_key_id(values$nested_form_id)

    # получение дата-фрейма
    values$data <- db$read_df_from_db_by_id(
      table_name = values$nested_form_id, 
      schm,
      main_key_value = values$main_key,
      con = con
    )

    col_types <- schm$get_schema(values$nested_form_id) |>
      dplyr::distinct(form_id, form_type, form_label)

    date_cols <- subset(col_types, form_type == "date", form_id, drop = TRUE)

    values$data <- values$data |>
      select(-schm$get_main_key_id) |>
      mutate(
        dplyr::across(tidyselect::all_of({{date_cols}}), as.Date)
      ) |>
      arrange({{key_id}})

    output$dt_nested <- DT::renderDataTable(
      DT::datatable(
        values$data,
        caption = 'Table 1: This is a simple caption for the table.',
        rownames = FALSE,
        # colnames = dplyr::pull(col_types, form_id, form_label),
        extensions = c('KeyTable', "FixedColumns"),
        editable = 'cell',
        options = list(
          dom = 'tip',
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1),
          keys = TRUE
        )
      ) |>
        DT::formatDate(date_cols, "toLocaleDateString")
    )

    showModal(modalDialog(
      DT::dataTableOutput("dt_nested"),
      size = "xl",
      footer = tagList(
        actionButton("nested_form_dt_save", "сохранить изменения")
      ),
      easyClose = TRUE
    ))
    
  }

  ### обновление данных при изменении --------------------
  observeEvent(input$dt_nested_cell_edit, {
    values$data <- DT::editData(values$data, input$dt_nested_cell_edit, 'dt_nested', rownames = FALSE)
  })

  ### кнопка: отображение -----------------------------
  observeEvent(input$nested_form_dt_button, {
    con <- db$make_db_connection("nested_form_save_button")
    on.exit(db$close_db_connection(con, "nested_form_save_button"), add = TRUE)

    removeModal()
    show_modal_for_nested_form_dt(con)
  })

  ### кнопка: сохранить изменения --------------------
  observeEvent(input$nested_form_dt_save, {
  
    con <- db$make_db_connection("nested_form_dt_save")
    on.exit(db$close_db_connection(con, "nested_form_dt_save"), add = TRUE)

    export_df <- values$data |>
      dplyr::distinct() |>
      dplyr::mutate(!!dplyr::sym(schm$get_main_key_id) := values$main_key, .before = 1)

    db$write_df_to_db(
      df = export_df,
      table_name = values$nested_form_id,
      schm,
      main_key_value = values$main_key,
      nested_key_value = NULL,
      con = con
    )

    log_action_to_db("saving data (gt)", values$main_key, con)

  })

  ## сохранение данных из вложенной формы ---------------
  observeEvent(input$nested_form_save_button, {
    req(values$nested_form_id)

    con <- db$make_db_connection("nested_form_save_button")
    on.exit(db$close_db_connection(con, "nested_form_save_button"), add = TRUE)
    
    # сохраняем данные основной формы!!!
    save_inputs_to_db(
      table_name        = "main",
      id_and_types_list = schm$get_id_type_list("main"),
      con               = con
    )

    # сохраняем данные текущей вложенной таблицы
    save_inputs_to_db(
      table_name        = values$nested_form_id,
      id_and_types_list = schm$get_id_type_list(values$nested_form_id),
      ns                = NS(values$nested_form_id),
      con = con
    )

    log_action_to_db("saving data", values$main_key, con)

    showNotification(
      "Данные успешно сохраннены",
      type = "message"
    )
  })

  ## обновление данных при переключении ключей ------------
  observeEvent(input$nested_key_selector, {
    req(input$nested_key_selector)
    req(values$main_key)

    # выбранный ключ в форме - перемещаем в RV
    values$nested_key <- input$nested_key_selector

  })

  observeEvent(values$nested_key, {

    con <- db$make_db_connection("nested_tables")
    on.exit(db$close_db_connection(con, "nested_tables"), add = TRUE)

    kyes_for_this_table <- db$get_nested_keys_from_table(values$nested_form_id, schm, values$main_key, con)

    if (values$nested_key %in% kyes_for_this_table) {

      # выгрузка датафрейма по общим и вложенным ключам
      df <- db$read_df_from_db_by_id(
        table_name = values$nested_form_id,
        schm,
        main_key_value   = values$main_key,
        nested_key_value = values$nested_key,
        con = con
      )

      # загрузка данных в формы
      load_data_to_form(
        df                = df,
        table_name = values$nested_form_id,
        schm,
        ns                = NS(values$nested_form_id)
      )
    }
  })

  ## добавление нового вложенного ключа -------------------
  observeEvent(input$add_new_nested_key_button, {

    removeModal()

    # та самая форма для ключа
    scheme_for_key_input <- schm$get_schema(values$nested_form_id) |>
      dplyr::filter(form_id == schm$get_key_id(values$nested_form_id))

    ui1 <- rlang::exec(
      .fn = utils$render_forms, 
      !!!distinct(scheme_for_key_input, form_id, form_label, form_type),
      main_scheme = scheme_for_key_input
    )

    showModal(modalDialog(
      title = "Создать новую запись",
      ui1,
      footer = tagList(
        actionButton("confirm_create_new_nested_key", "Создать")
      ),
      easyClose = TRUE
    ))

  })

  # действие при подтверждении создания новой записи
  observeEvent(input$confirm_create_new_nested_key, {
    req(input[[schm$get_key_id(values$nested_form_id)]])

    con <- db$make_db_connection("confirm_create_new_key")
    on.exit(db$close_db_connection(con, "confirm_create_new_key"), add = TRUE)

    existed_key <- db$get_nested_keys_from_table(
      table_name = values$nested_form_id,
      schm,
      main_key_value = values$main_key,
      con
    )

    if (input[[schm$get_key_id(values$nested_form_id)]] %in% existed_key) {
      showNotification(
        sprintf("В базе уже запись с данным ключем."),
        type = "error"
      )
      return()
    }
    
    values$nested_key <- input[[schm$get_key_id(values$nested_form_id)]]
    utils$clean_forms(values$nested_form_id, schm, NS(values$nested_form_id))
    removeModal()
    show_modal_for_nested_form(con)

  })

  # STATUSES ===============================
  # вывести отображение что что-то не так
  output$status_message <- renderText({
    shiny::validate(
      need(values$main_key, "⚠️ Необходимо указать id пациента!")
    )
    paste0("ID: ", values$main_key)
  })

  output$status_message2 <- renderText({
    iv_main$is_valid()
  })

  # =========================================
  # MAIN BUTTONS LOGIC ======================
  # =========================================
  ## добавить новый главный ключ  ------------------------
  observeEvent(input$add_new_main_key_button, {

    # данные для главного ключа
    scheme_for_key_input <- schm$get_schema("main") |>
      dplyr::filter(form_id == schm$get_main_key_id)

    # создать форму для выбора ключа
    ui1 <- rlang::exec(
      .fn = utils$render_forms, 
      !!!distinct(scheme_for_key_input, form_id, form_label, form_type),
      main_scheme = scheme_for_key_input
    )

    # даилог создания нового ключа
    showModal(modalDialog(
      title = "Создать новую запись",
      ui1,
      footer = tagList(
        actionButton("confirm_create_new_main_key", "Создать")
      ),
      easyClose = TRUE
    ))

  })

  ## действие при подтверждении (проверка нового создаваемого ключа) -------
  observeEvent(input$confirm_create_new_main_key, {
    req(input[[schm$get_main_key_id]])

    con <- db$make_db_connection("confirm_create_new_main_key")
    on.exit(db$close_db_connection(con, "confirm_create_new_key"), add = TRUE)

    new_main_key <- trimws(input[[schm$get_main_key_id]])

    existed_key <- db$get_keys_from_table("main", schm, con)

    # если введенный ключ уже есть в базе
    if (new_main_key %in% existed_key) {
      showNotification(
        sprintf("В базе уже запись с данным ключем."),
        type = "error"
      )
      return()
    }
    
    values$main_key <- new_main_key
    log_action_to_db("creating new key", values$main_key, con)
    utils$clean_forms("main", schm)

    removeModal()
  })

  ## очистка всех полей -----------------------
  # show modal on click of button
  observeEvent(input$clean_data_button, {
    showModal(modalDialog(
      "Данное действие очистит все заполненные данные. Убедитесь, что нужные данные сохранены.",
      title = "Очистить форму?",
      footer = tagList(
        actionButton("close_modal_button", "Отмена"),
        actionButton("clean_all_action", "Очистить.", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  # when action confirm - perform action
  observeEvent(input$clean_all_action, {
    
    # rewrite all inputs with empty data
    utils$clean_forms("main", schm)
    values$main_key <- NULL

    removeModal()
    showNotification("Данные очищены!", type = "warning")
  })

  ## сохранение даннных -------------------------------
  observeEvent(input$save_data_button, {
    req(values$main_key)

    con <- db$make_db_connection("save_data_button")
    on.exit(db$close_db_connection(con, "save_data_button"), add = TRUE)

    save_inputs_to_db(
      table_name = "main",
      id_and_types_list = schm$get_id_type_list("main"),
      con = con
    )

    log_action_to_db("saving data", values$main_key, con = con)
    showNotification(
      "Данные успешно сохранены",
      type = "message"
    )
  })

  ## список ключей для загрузки данных -------------------
  observeEvent(input$load_data_button, {

    con <- db$make_db_connection("load_data_button")
    on.exit(db$close_db_connection(con, "load_data_button"))

    if (length(dbListTables(con)) != 0 && "main" %in% DBI::dbListTables(con)) {

      # GET DATA files
      ids <- db$get_keys_from_table("main", schm, con)

      ui_load_menu <- renderUI({
        selectizeInput(
          inputId = "load_data_key_selector",
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
      ui_load_menu <- renderUI({
        h5("База данных не содержит записей")
      })
    }

    shiny::showModal(
      modalDialog(
        "Загрузить данные",
        ui_load_menu,
        title = "Загрузить имеющиеся данные",
        footer = tagList(
          actionButton("close_modal_button", "Отмена", class = "btn btn-danger"),
          actionButton("load_data", "Загрузить данные"),
        ),
        easyClose = TRUE
      )
    )
  })

  ## загрузка данных по главному ключу ------------------
  observeEvent(input$load_data, {
    req(input$load_data_key_selector)

    con <- db$make_db_connection("load_data")
    on.exit(db$close_db_connection(con, "load_data"), add = TRUE)

    df <- db$read_df_from_db_by_id(
      table_name = "main", 
      schm = schm,
      main_key_value = input$load_data_key_selector,
      con = con
    )

    load_data_to_form(
      df = df,
      table_name = "main",
      schm
    )
    
    values$main_key <- input$load_data_key_selector
    log_action_to_db("loading data", values$main_key, con = con)
    removeModal()

  })

  ## export to .xlsx ====
  output$downloadData <- downloadHandler(
    filename = paste0("test_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      con <- db$make_db_connection("downloadData")
      on.exit(db$close_db_connection(con, "downloadData"), add = TRUE)

      # get all data
      list_of_df <- purrr::map(
        .x = purrr::set_names(schm$all_tables_names),
        .f = \(x) {

          df <- read_df_from_db_all(x, con) |>
            tibble::as_tibble()

          # handle with data
          scheme <- schm$get_schema(x)

          date_columns   <- subset(scheme, form_type == "date", form_id, drop = TRUE)
          number_columns <- subset(scheme, form_type == "number", form_id, drop = TRUE)

          df <- df |>
            dplyr::mutate(
              # даты - к единому формату
              dplyr::across(tidyselect::all_of({{date_columns}}), as.Date),
              # числа - к единому формату десятичных значений
              dplyr::across(tidyselect::all_of({{number_columns}}), ~ gsub("\\.", "," , .x)),
            )

          df
        }
      )

      # set date params
      options("openxlsx2.dateFormat" = "dd.mm.yyyy")

      cli::cli_alert_success("База успешно экспортирована")
      showNotification("База успешно экспортирована", type = "message")
      log_action_to_db("exporting data to xlsx", con = con)

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
      paste0(values$main_key, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
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
        .x = unique(schm$get_schema("main")$part),
        .f = \(x_iter1) {
          # write level 1 header
          HEADER_1 <- paste("#", x_iter1, "\n")
          empty_vec <<- c(empty_vec, HEADER_1)

          # iterate by level2 headers (subgroups)
          purrr::walk(
            .x = dplyr::pull(unique(subset(schm$get_schema("main"), part == x_iter1, "subgroup"))),
            .f = \(x_iter2) {
              # get header 2 name
              HEADER_2 <- paste("##", x_iter2, "\n")

              # for some reason set litle scheme...
              litle_scheme <- subset(
                x = schm$get_schema("main"),
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

  ## import data from xlsx ----------------------
  observeEvent(input$button_upload_data_from_xlsx, {

    showModal(modalDialog(
      title = "Добавить пациентов к текущему списку...",
      fileInput(
        "upload_xlsx",
        NULL,
        buttonLabel = "Выбрать файлы...",
        placeholder = "No file selected",
        multiple = TRUE,
        accept = ".xlsx",
        width = 450
      ),
      checkboxInput("upload_data_from_xlsx_owerwrite_all_data", "перезаписать все данные", width = 450),
      footer = tagList(
        modalButton("Отмена"),
        actionButton("button_upload_data_from_xlsx_confirm", "Добавить")
      ),
      easyClose = FALSE
    ))

  })

  observeEvent(input$button_upload_data_from_xlsx_confirm, {
    req(input$upload_xlsx)

    con <- db$make_db_connection("button_upload_data_from_xlsx_confirm")
    on.exit(db$close_db_connection(con, "button_upload_data_from_xlsx_confirm"), add = TRUE)

    file <- input$upload_xlsx$datapath
    wb <- openxlsx2::wb_load(file)

    main_key_id <- schm$get_main_key_id

    # проверка на наличие всех листов в файле
    if (!all(schm$all_tables_names %in% openxlsx2::wb_get_sheet_names(wb))) {
      cli::cli_alert_warning("данные в файле '{file} не соответствуют схеме'")
      return()
    }

    # проверка схемы --------------
    for (table_name in schm$all_tables_names) {

      df     <- openxlsx2::read_xlsx(wb, table_name) 
      scheme <- schm$get_schema(table_name) |>
        filter(!form_type %in% c("description", "nested_forms"))

      # столбцы в таблицы и схема
      df_to_schema_compare <- setdiff(colnames(df), unique(scheme$form_id))
      schema_to_df_compare <- setdiff(unique(scheme$form_id), colnames(df))

      if (length(schema_to_df_compare) > 0 ) {
        cli::cli_warn(c("в схеме для '{table_name}' нет следующих столбцов:", paste("- ", df_to_schema_compare)))
      }

      # схема и столбцы в таблице
      schema_to_df_compare <- setdiff(unique(scheme$form_id), colnames(df))
      if (length(schema_to_df_compare) > 0 ) {

        message <- glue::glue("столбцы в таблице '{table_name}' не соответсвуют схеме")
        cli::cli_warn(c(message, paste("- ", schema_to_df_compare)))
        showNotification(message, type = "error")

        return()
      }
    }

    # обновление данных
    for (table_name in schm$all_tables_names) {

      df     <- openxlsx2::read_xlsx(wb, table_name) 
      scheme <- schm$get_schema(table_name) |>
        filter(!form_type %in% c("description", "nested_forms"))

      date_columns <- subset(scheme, form_type == "date", form_id, drop = TRUE)
      number_columns <- subset(scheme, form_type == "number", form_id, drop = TRUE)

      # функция для преобразование числовых значений и сохранения "NA"
      num_converter <- function(old_col) {
        vec_with_na <- which(old_col == "NA")

        # текстовые и числовые значения в текст: '24.0', '24,5' > '24', '24,5' (также обрезеаются десятичные значения где не нужно)
        new_col <- suppressWarnings(as.character(as.double(gsub(",", "\\.", old_col))))

        # значения где были явно указаны 'NA' остаются с текстом 'NA'
        new_col[which(old_col == "NA")] <- "NA"

        gsub("\\.", ",", new_col)
      }

      df <- df |>
        dplyr::mutate(
          # даты - к единому формату
          dplyr::across(tidyselect::all_of({{date_columns}}), \(x) purrr::map_chr(x, db$excel_to_db_dates_converter)),
          dplyr::across(tidyselect::all_of({{number_columns}}), num_converter),
        ) |>
        select(all_of(unique(c(main_key_id, scheme$form_id))))

      df_original <- DBI::dbReadTable(con, table_name) |>
        as_tibble()

      if (input$upload_data_from_xlsx_owerwrite_all_data == TRUE) cli::cli_abort("not implemented yet")

      walk(
        .x = unique(df[[main_key_id]]),
        .f = \(main_key) {

          if (main_key %in% unique(df_original[[main_key_id]])) {
            DBI::dbExecute(con, glue::glue("DELETE FROM {table_name} WHERE {main_key_id} = '{main_key}'"))
          }
        }
      )

      DBI::dbWriteTable(
        con,
        name = table_name,
        value = df,
        append = TRUE
      )

      message <- glue::glue("Данные таблицы '{table_name}' успешно обновлены")
      showNotification(
        message,
        type = "message"
      )
      cli::cli_alert_success(message)
    }
    log_action_to_db("importing data from xlsx", con = con)
    removeModal()
  })

  ## cancel ==========================
  observeEvent(input$close_modal_button, {
    removeModal()
  })

  # FUNCTIONS ==============================
  ## reading tables from db all ========
  read_df_from_db_all <- function(table_name, con) {

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
  log_action_to_db <- function(
    action = c(
      "saving data",
      "saving data (gt)",
      "loading data",
      "creating new key",
      "exporting data to xlsx",
      "importing data from xlsx"
    ),
    key = NULL,
    con
  ) {

    action <- match.arg(action)

    action_row <- tibble(
      date        = Sys.time(),
      user        = ifelse(AUTH_ENABLED, res_auth$user, "anonymous"),
      remote_addr = session$request$REMOTE_ADDR,
      key         = key,
      action      = action,
    )

    DBI::dbWriteTable(con, "log", action_row, append = TRUE)
  }

  # КРАТКАЯ СВОДКА ПРО ЛОГГИНГ ------------------
  observe({

    output$display_log <- renderUI({

      con <- db$make_db_connection("display_log")
      on.exit(db$close_db_connection(con, "display_log"), add = TRUE)

      query <- if (!is.null(values$main_key)) {
        sprintf("SELECT * FROM log WHERE key = '%s'", values$main_key)
      } else {
        "SELECT * FROM log"
      }

      log_rows <- DBI::dbGetQuery(con, query)

      if (nrow(log_rows) > 0) {

        lines <- log_rows |>
          mutate(date = as.POSIXct(date)) |>
          mutate(
            # date = date + lubridate::hours(3), # fix datetime
            date_day = as.Date(date)
          ) |>
          mutate(cons_actions = dplyr::consecutive_id(action, user)) |>
          mutate(n_actions = row_number(), .by = c(cons_actions, user, action, date_day)) |>
          slice(which.max(n_actions), .by = c(user, action, date_day)) |>
          mutate(string_to_print = sprintf(
            "<b>[%s %s]</b>: %s - %s (%s)",
            format(date, "%d.%m.%y"),
            format(date, "%H:%M"),
            user,
            action,
            n_actions
          )) |>
          pull(string_to_print) |>
          paste(collapse = "</br>")

      } else {
        lines <- ""
      }

      tagList(
        paste0("ID: ", values$main_key),
        br(),
        p(
          HTML(lines),
          style = "font-size:10px;"
        )
      )
    })
  })
}


app <- shinyApp(ui = ui, server = server)

runApp(app, launch.browser = TRUE)
