suppressPackageStartupMessages({
  library(DBI)
  # library(RSQLite)
  library(tidyr)
  library(dplyr)
  library(purrr)
  library(magrittr)
  library(shiny)
  library(bslib)
  # library(rhandsontable)
  library(shinymanager)
})

source("helpers/functions.R")

# box::purge_cache()
# box::use(./helpers/db)

# SOURCE FILES ============================
config <- config::get(file = "configs/config.yml")

folder_with_schemas <- fs::path("configs/schemas")
FILE_SCHEME         <- fs::path(folder_with_schemas, "schema.xlsx")
# dbfile              <- fs::path("data.sqlite")

# options(box.path = getwd())
box::purge_cache()
box::use(
  modules/utils,
  modules/global_options,
  modules/db,
  modules/data_validation
)

global_options$set_global_options()

# SETTINGS ================================
AUTH_ENABLED <- config$auth_module

# CHECK FOR PANDOC
# TEMP ! NEED TO HANDLE
rmarkdown::find_pandoc(dir = "/opt/homebrew/bin/")

# TODO: dynamic button render depend on pandoc installation
if (!rmarkdown::pandoc_available()) warning("Can't find pandoc!")

load_scheme_from_xlsx <- function(
  sheet_name,
  colnames = c("part", "subgroup", "form_id", "form_label", "form_type")
) {

  readxl::read_xlsx(FILE_SCHEME, sheet = sheet_name) |>
    # fill NA down
    tidyr::fill(all_of(colnames), .direction = "down") |>
    dplyr::group_by(form_id) |>
    tidyr::fill(c(condition, required), .direction = "down") |>
    dplyr::ungroup()

}

extract_forms_id_and_types_from_scheme <- function(scheme, key = c("main_key", "nested_key")) {

  key <- match.arg(key)

  form_id_and_types_list <- scheme |>
    dplyr::filter(!form_type %in% c("inline_table", "nested_forms","description", "description_header")) |>
    dplyr::distinct(form_id, form_type) |>
    tibble::deframe()

  if(!key %in% names(form_id_and_types_list)) cli::cli_abort("в схеме должно быть поле с ключем (key)")
  form_id_and_types_list[names(form_id_and_types_list) != key]

}

# SCHEME_MAIN UNPACK ==========================
# load scheme
SCHEME_MAIN <- load_scheme_from_xlsx("main")

# get list of simple inputs
main_id_and_types_list <- extract_forms_id_and_types_from_scheme(SCHEME_MAIN)

# # get list of inputs with inline tables
# inputs_tables_list <- SCHEME_MAIN |>
#   dplyr::filter(form_type == "inline_table") |>
#   dplyr::distinct(form_id) |>
#   tibble::deframe()

# 
nested_forms_df <- SCHEME_MAIN |>
  dplyr::filter(form_type == "nested_forms") |>
  dplyr::distinct(form_id, .keep_all = TRUE)

# лист со схемами для всех вложенных формы
nested_forms_schemas_list <- purrr::map(

  .x = purrr::set_names(unique(nested_forms_df$form_id)),
  .f = \(nested_form_id) {

    nested_form_scheme_sheet_name <- nested_forms_df |>
      dplyr::filter(form_id == {nested_form_id}) |>
      dplyr::pull(choices)

    # загрузка схемы для данной вложенной формы
    load_scheme_from_xlsx(nested_form_scheme_sheet_name, colnames = c("subgroup","form_id", "form_label", "form_type"))

  }
)

# establish connection
con <- db$make_db_connection()

# init DB (write dummy data to "main" table)
db$check_if_table_is_exist_and_init_if_not("main", main_id_and_types_list)

purrr::walk(
  .x = unique(nested_forms_df$form_id),
  .f = \(table_name) {

    this_inline_table2_info <- nested_forms_df |>
      dplyr::filter(form_id == {table_name})

    # получение имя файла с таблицой
    nested_form_scheme_sheet_name <- this_inline_table2_info$choices

    # загрузка схемы для данной вложенной формы
    this_nested_form_scheme <- load_scheme_from_xlsx(nested_form_scheme_sheet_name, colnames = c("subgroup","form_id", "form_label", "form_type"))

    this_table_id_and_types_list <- extract_forms_id_and_types_from_scheme(this_nested_form_scheme, "nested_key")

    db$check_if_table_is_exist_and_init_if_not(
      table_name,
      this_table_id_and_types_list,
      con = con
    )

  }
)

# close connection to prevent data loss
db$close_db_connection(con)

# # INLINE TABLES =====================
# # создаем для каждой таблицы объект
# inline_tables <- purrr::map(
#   .x = purrr::set_names(inputs_tables_list),
#   .f = \(x_inline_table_name) {

#     # получить имя файла со схемой
#     file_name <- SCHEME_MAIN |>
#       dplyr::filter(form_id == x_inline_table_name) |>
#       dplyr::pull(choices)

#     # load scheme
#     schemaaa <- readxl::read_xlsx(fs::path(folder_with_schemas, file_name)) |>
#       tidyr::fill(dplyr::everything(), .direction = "down")

#     # список форм в схеме
#     inline_forms <- schemaaa |>
#       dplyr::distinct(form_id) |>
#       dplyr::pull()

#     # макет таблицы (пустой)
#     DF_gen <- as.list(setNames(rep(as.character(NA), length(inline_forms)), inline_forms)) |>
#       as.data.frame()

#     # make 12 more empty rows
#     DF_gen <- rbind(DF_gen, DF_gen[rep(1, 12), ])
#     rownames(DF_gen) <- NULL

#     list(schema = schemaaa, df_empty = DF_gen)
#   }
# )

# generate nav panels for each page
nav_panels_list <- purrr::map(
  .x = unique(SCHEME_MAIN$part),
  .f = \(page_name) {

    # отделить схему для каждой страницы
    this_page_panels_scheme <- SCHEME_MAIN |>
      dplyr::filter(!form_id %in% c("main_key", "nested_key")) |>
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
    actionButton("add_new_main_key_button", "ДОБАВИТЬ ЧТО_ТО", icon("floppy-disk", lib = "font-awesome")),
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
    id = "main",
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


# init auth =======================
if (AUTH_ENABLED) ui <- shinymanager::secure_app(ui, enable_admin = TRUE)

# SERVER LOGIC =============================
server <- function(input, output, session) {
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
  values <- reactiveValues(
    data = NULL,
    main_key = NULL,
    nested_key = NULL,
    nested_form_id = NULL,
    nested_id_and_types = NULL
  )
  rhand_tables <- reactiveValues()

  # ==========================================
  # ОБЩИЕ ФУНКЦИИ ============================
  # ==========================================

  ## перенос данных из датафрейма в форму -----------------------
  load_data_to_form <- function(df, id_and_types_list, ns) {

    input_types <- unname(id_and_types_list)
    input_ids   <- names(id_and_types_list)
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
          ns = ns
        )
      }
    )

    # inline tables
    # purrr::walk(
    #   .x = inputs_tables_list,
    #   .f = \(x_table_name) {
    #     loaded_df_for_id <- read_df_from_db_by_id(x_table_name, con)

    #     # если табличечки не пустые загружаем их
    #     if (!is.null(loaded_df_for_id) && nrow(loaded_df_for_id) != 0) {
    #       rhand_tables[[x_table_name]] <- subset(loaded_df_for_id, select = c(-key))
    #     } else {
    #       rhand_tables[[x_table_name]] <- inline_tables[[x_table_name]]$df_empty
    #     }
    #   }
    # )

    # showNotification("Данные загружены!", type = "message")
    # cli::cli_alert_success("данные для '{main_key}' из таблицы {table_name} успешно загружены")

    # log_action_to_db("load", main_key, con = con)
  }

  ## сохранение данных из форм в базу данных --------
  save_inputs_to_db <- function(
    table_name, 
    id_and_types_list, 
    ns, 
    con
  ) {
    
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
          main_key = values$main_key,
          .before = 1
        )
    }

    # для всех остальных таблицы (вложенные)
    if (table_name != "main") {
      exported_df <- exported_df |>
        mutate(
          main_key   = values$main_key,
          nested_key = values$nested_key,
          .before = 1
        )
    }

    # если данных нет - просто записать данные
    log_action_to_db("save", values$main_key, con)

    db$write_df_to_db(
      df = exported_df,
      table_name = table_name,
      main_key = values$main_key,
      nested_key = values$nested_key,
      con = con
    )
  }

  # ====================================
  # NESTED FORMS =======================
  # ====================================
  ## кнопки для каждой вложенной таблицы -------------------------------
  purrr::walk(
    .x = nested_forms_df$form_id,
    .f = \(nested_form_id) {

      observeEvent(input[[nested_form_id]], {
        req(values$main_key)

        con <- db$make_db_connection("nested_tables")
        on.exit(db$close_db_connection(con, "nested_tables"), add = TRUE)

        values$nested_form_id <- nested_form_id
        values$nested_key <- NULL # для нормальной работы реактивных значений

        show_modal_for_nested_form(con)

      })
    }
  )

  ## функция отображения вложенной формы для выбранной таблицы --------
  show_modal_for_nested_form <- function(con) {

    ns <- NS(values$nested_form_id)

    # загрузка схемы для данной вложенной формы
    this_nested_form_scheme <- nested_forms_schemas_list[[values$nested_form_id]]

    values$nested_id_and_types <- extract_forms_id_and_types_from_scheme(this_nested_form_scheme, "nested_key")

    # выбираем все ключи из баз данных
    kyes_for_this_table <- db$get_nested_keys_from_table(values$nested_form_id, values$main_key, con)
    kyes_for_this_table <- unique(c(values$nested_key, kyes_for_this_table))
    values$nested_key <- kyes_for_this_table[[1]]

    # nested ui
    yay_its_fun <- purrr::map(
      .x = unique(this_nested_form_scheme$subgroup),
      .f = \(subgroup) {

        subroup_scheme <- this_nested_form_scheme |>
          dplyr::filter(subgroup == {{subgroup}}) |>
          dplyr::filter(!form_id %in% c("main_key", "nested_key"))

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

    # ui для всплывающего окна 
    ui_for_inline_table <- navset_card_underline(
        sidebar = sidebar(
          width = 300,
          selectizeInput(
            inputId = "nested_key_selector",
            label = "nested_key label", 
            choices = kyes_for_this_table,
            selected = values$nested_key,
            # options = list(placeholder = "действие комиссии", create = FALSE, onInitialize = I('function() { this.setValue(""); }'))
          ),
          actionButton("add_new_nested_key_button", "add"),
          actionButton("nested_form_save_button", "save")
        ),
        !!!yay_its_fun
      )


    # проверка данных для внутренних таблиц
    iv_inner <- data_validation$init_val(this_nested_form_scheme, ns)
    iv_inner$enable()

    showModal(modalDialog(
      ui_for_inline_table,
      footer = actionButton("nested_form_close_button", "Закрыть"),
      size = "l"
    ))
  }

  observeEvent(input$nested_form_close_button, {
    removeModal()
  })

  ## сохранение данных из вложенной формы ---------------
  observeEvent(input$nested_form_save_button, {
    req(values$nested_form_id)

    con <- db$make_db_connection("nested_form_save_button")
    on.exit(db$close_db_connection(con, "nested_form_save_button"), add = TRUE)

    # сохраняем данные основной формы!!!
    save_inputs_to_db(
      table_name        = "main",
      id_and_types_list = main_id_and_types_list,
      con               = con
    )

    # сохраняем данные текущей вложенной таблицы
    save_inputs_to_db(
      table_name        = values$nested_form_id,
      id_and_types_list = values$nested_id_and_types,
      ns                = NS(values$nested_form_id),
      con = con
    )

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

    kyes_for_this_table <- db$get_nested_keys_from_table(values$nested_form_id, values$main_key, con)

    if (values$nested_key %in% kyes_for_this_table) {

      # выгрузка датафрейма по общим и вложенным ключам
      df <- db$read_df_from_db_by_id(
        table_name = values$nested_form_id, 
        main_key = values$main_key,
        nested_key = values$nested_key,
        con = con
      )

      # загрузка данных в формы
      load_data_to_form(
        df                = df,
        id_and_types_list = values$nested_id_and_types,
        ns                = NS(values$nested_form_id)
      )
    }
  })

  ## добавление нового вложенного ключа -------------------
  observeEvent(input$add_new_nested_key_button, {

    removeModal()

    # та самая форма для ключа
    scheme_for_key_input <- nested_forms_schemas_list[[values$nested_form_id]] |>
      dplyr::filter(form_id %in% c("nested_key"))

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
    req(input$nested_key)

    con <- db$make_db_connection("confirm_create_new_key")
    on.exit(db$close_db_connection(con, "confirm_create_new_key"), add = TRUE)

    existed_key <- db$get_nested_keys_from_table(
      table_name = values$nested_form_id, 
      main_key = values$main_key, 
      con
    )

    if (input$nested_key %in% existed_key) {
      showNotification(
        sprintf("В базе уже запись с данным ключем."),
        type = "error"
      )
      return()
    }
    
    values$nested_key <- input$nested_key
    utils$clean_forms(values$nested_id_and_types, NS(values$nested_form_id))
    removeModal()
    show_modal_for_nested_form(con)

  })

  # VALIDATIONS ============================
  # create new validator
  iv_main <- data_validation$init_val(SCHEME_MAIN)
  iv_main$enable()

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

  # CREATE RHANDSOME TABLES =====================
  # записать массив пустых табличек в rhands_tables
  # purrr::walk(
  #   .x = purrr::set_names(inputs_tables_list),
  #   .f = \(x_inline_table) {
  #     rhand_tables[[x_inline_table]] <- inline_tables[[x_inline_table]]$df_empty
  #   }
  # )

  # # render tables
  # observe({
  #   # MESSAGE
  #   purrr::walk(
  #     .x = inputs_tables_list,
  #     .f = \(x) {
  #       # вытаскиваем схемы из заготовленного ранее списка
  #       schema <- inline_tables[[x]]$schema

  #       # убрать дубликаты
  #       schema_comp <- schema |>
  #         dplyr::distinct(form_id, form_label, form_type)

  #       # заголовки
  #       headers <- dplyr::pull(schema_comp, form_label)

  #       # fixes empty rows error
  #       rownames(rhand_tables[[x]]) <- NULL

  #       # создать объект рандсонтебл
  #       rh_tabel <- rhandsontable::rhandsontable(
  #         rhand_tables[[x]],
  #         colHeaders = headers,
  #         rowHeaders = NULL,
  #         height = 400,
  #       ) |>
  #         rhandsontable::hot_cols(
  #           colWidths = 120,
  #           manualColumnResize = TRUE,
  #           columnSorting = TRUE
  #         )

  #       # циклом итерируемся по индексу;
  #       for (i in seq(1, length(schema_comp$form_id))) {
  #         # получаем информацию о типе столбца
  #         type <- dplyr::filter(schema_comp, form_id == schema_comp$form_id[i]) |>
  #           dplyr::pull(form_type)

  #         # информация о воможных вариантнах выбора
  #         choices <- dplyr::filter(schema, form_id == schema_comp$form_id[i]) |>
  #           dplyr::pull(choices)

  #         ## проверки
  #         # текстовое поле
  #         if (type == "text") {
  #           rh_tabel <- rh_tabel |>
  #             hot_col(col = headers[i], type = "autocomplete")
  #         }

  #         # выбор из списка
  #         if (type == "select_one") {
  #           rh_tabel <- rh_tabel |>
  #             hot_col(col = headers[i], type = "dropdown", source = choices)
  #         }

  #         # дата
  #         if (type == "date") {
  #           rh_tabel <- rh_tabel |>
  #             hot_col(col = headers[i], type = "date", dateFormat = "DD.MM.YYYY", language = "ru-RU")
  #         }
  #       }

  #       # передаем в оутпут полученный объект
  #       output[[x]] <- renderRHandsontable({
  #         rh_tabel
  #       })
  #     }
  #   )
  # })

  # =========================================
  # MAIN BUTTONS LOGIC ======================
  # =========================================
  ## добавить новый главный ключ  ------------------------
  observeEvent(input$add_new_main_key_button, {

    # данные для главного ключа
    scheme_for_key_input <- SCHEME_MAIN |>
      dplyr::filter(form_id == "main_key")

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
    req(input$main_key)

    con <- db$make_db_connection("confirm_create_new_main_key")
    on.exit(db$close_db_connection(con, "confirm_create_new_key"), add = TRUE)

    existed_key <- db$get_keys_from_table("main", con)
    
    # если введенный ключ уже есть в базе
    if (input$main_key %in% existed_key) {
      showNotification(
        sprintf("В базе уже запись с данным ключем."),
        type = "error"
      )
      return()
    }
    
    values$main_key <- input$main_key
    utils$clean_forms(main_id_and_types_list)

    removeModal()
  })

  ## очистка всех полей -----------------------
  # show modal on click of button
  observeEvent(input$clean_data_button, {
    showModal(modal_clean_all)
  })

  # when action confirm - perform action
  observeEvent(input$clean_all_action, {
    
    # rewrite all inputs with empty data
    utils$clean_forms(main_id_and_types_list)

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
      id_and_types_list = main_id_and_types_list,
      con = con
    )

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
      ids <- db$get_keys_from_table("main", con)

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
          actionButton("cancel_button", "Отмена", class = "btn btn-danger"),
          actionButton("load_data", "Загрузить данные"),
        ),
        easyClose = TRUE
      )
    )
  })

  ## загрузка данных по главному ключу ------------------
  observeEvent(input$load_data, {

    con <- db$make_db_connection("load_data")
    on.exit(db$close_db_connection(con, "load_data"), add = TRUE)

    df <- db$read_df_from_db_by_id(
      table_name = "main", 
      main_key = input$load_data_key_selector,
      con = con
    )

    load_data_to_form(
      df = df,
      id_and_types_list = main_id_and_types_list
    )
  
    values$main_key <- input$load_data_key_selector

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
        .x = purrr::set_names(c("main", unique(nested_forms_df$form_id))),
        .f = \(x) {

          df <- read_df_from_db_all(x, con) %>%
            tibble::as_tibble()

          # handle with data
          scheme <- if (x == "main") SCHEME_MAIN else nested_forms_schemas_list[[x]]

          data_columns   <- subset(scheme, form_type == "date", form_id, drop = TRUE)
          number_columns <- subset(scheme, form_type == "number", form_id, drop = TRUE)

          df <- df |>
            dplyr::mutate(
              # даты - к единому формату
              dplyr::across(tidyselect::all_of({{data_columns}}), as.Date),
              # числа - к единому формату десятичных значений
              dplyr::across(tidyselect::all_of({{number_columns}}), ~ gsub("\\.", "," , .x)),
            )

          df
        }
      )

      # set date params
      options("openxlsx2.dateFormat" = "dd.mm.yyyy")

      cli::cli_alert_success("DATA EXPORTED")
      showNotification("База успешно экспортирована", type = "message")

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
  # observeEvent(input$overwrite_data_confirm, {

  #   con <- db$make_db_connection("saving data (from modal conf)")
  #   on.exit(db$close_db_connection(con, "saving data (from modal conf)"), add = TRUE)

  #   # убираем плашку
  #   removeModal()

  #   # записываем данные
  #   db$write_df_to_db(
  #     df = exported_df,
  #     table_name = "main",
  #     main_key = values$main_key,
  #     con = con
  #   )

  #   log_action_to_db("overwrite", values$main_key, con = con)
  # })

  ## cancel ==========================
  observeEvent(input$cancel_button, {
    removeModal()
  })

  # FUNCTIONS ==============================
  ## write all inputs to db ================
  # write_all_to_db <- function() {

  #   con <- db$make_db_connection("fn call `write_all_to_db()`")
  #   # on.exit(close_db_connection("fn call `write_all_to_db()`"), add = TRUE)

  #   # write main
  #   write_df_to_db(exported_df, "main", con)

  #   # write inline tables
  #   for (i in inputs_tables_list) {
  #     df <- tryCatch(
  #       # проверка выражения
  #       expr = {
  #         hot_to_r(input[[i]])
  #       },
  #       # действия в случае ошибки
  #       error = function(e) {
  #         message(e$message)
  #         showNotification(
  #           glue::glue("Невозможно сохранить таблицу `{i}`! Данная ошибка может возникать в случае, если в таблице находятся пустые строки. Попробуйте удалить пустые строки и повторить сохранение."), # nolint
  #           duration = NULL,
  #           closeButton = FALSE,
  #           id = paste0(i, "error_inline_tables"),
  #           type = "error"
  #         )
  #         tibble()
  #       }
  #     )

  #     df <- df %>%
  #       dplyr::as_tibble() %>%
  #       janitor::remove_empty(which = c("rows")) %>%
  #       # adding id to dbs
  #       dplyr::mutate(key = input$main_key, .before = 1)

  #     # если таблица содержит хоть одну строку - сохранить таблицу в базу данных
  #     if (nrow(df) != 0) {
  #       write_df_to_db(df, i, con)
  #       removeNotification(paste0(i, "error_inline_tables"))
  #     }
  #   }

  #   showNotification(
  #     glue::glue("Данные пациента {input$main_key} сохранены!"),
  #     type = "warning"
  #   )
  # }

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
