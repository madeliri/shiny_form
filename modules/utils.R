#' @export
make_panels <- function(scheme) {

  cards <- purrr::map(
    .x = unique(scheme$subgroup),
    .f = \(sub_group) {

      this_column_cards_scheme <- scheme |>
        dplyr::filter(subgroup == {{sub_group}})

      bslib::card(
        bslib::card_header(sub_group, container = htmltools::h5),
        full_screen = TRUE,
        fill = TRUE,
        width = "4000px",
        bslib::card_body(
          fill = TRUE,
          # передаем все аргументы в функцию для создания елементов
          purrr::pmap(
            .l = dplyr::distinct(this_column_cards_scheme, form_id, form_label, form_type),
            .f = render_forms,
            main_scheme = scheme
          )
        )
      )
    }
  )

  # make page wrap
  page_wrap <- bslib::layout_column_wrap(
    # width = "350px", height = NULL, #was 800
    width = 1 / 4, height = NULL, # was 800
    fixed_width = TRUE,
    # unpack list of cards
    !!!cards
  )
}

#' @export
render_forms <- function(
  form_id,
  form_label,
  form_type,
  main_scheme,
  ns
) {

  # заготовку для формы (проверка на выходе функции)
  form <- NULL

  # параметры только для этой формы
  filterd_line <- dplyr::filter(main_scheme, form_id == {{form_id}})

  # если передана ns() функция то подмеяем id для каждой формы в соответствии с пространством имен
  if (!missing(ns)) {
    form_id <- ns(form_id)
  }

  # отдельно извлечение параметров условного отображения
  condition <- unique(filterd_line$condition)

  # элементы выбора 
  choices <- filterd_line$choices

  # описание 
  description <- unique(filterd_line) |>
    dplyr::filter(!is.na(form_description)) |>
    dplyr::distinct(form_description) |>
    dplyr::pull()

  # описание
  if (length(description) > 1) {
    rlang::abort(sprintf(
      "%s - более чем 1 уникальный вариант описания:\n%s", form_id, paste0(description, collapse = "\n")
    ))
  } else if (length(description) == 0) {
    description <- NA
  }

  # отдельно создаем заголовки
  label <- if (is.na(description) && is.na(form_label)) {
    NULL
  } else {
    shiny::tagList(
      if (!is.na(form_label)) {
        shiny::span(form_label, style = "color: #444444; font-weight: 550; line-height: 1.4;")
        # если в схеме есть поле с описанием - добавляем его следующей строчкой
      },
      if (!is.na(description) && !is.na(form_label)) shiny::br(),
      if (!is.na(description)) {
        shiny::span(shiny::markdown(description)) |> htmltools::tagAppendAttributes(style = "color:gray; font-size:small; line-height: 1.4;")
      }
    )
  }

  # simple text or number input
  if (form_type == "text") {

    # get info how much rows to render
    rows_to_show <- ifelse(!is.na(choices), as.integer(choices), 1)

    form <- shiny::textAreaInput(
      inputId = form_id,
      label = label,
      rows = rows_to_show
    )
  }

  if (form_type == "number") {
    form <- shiny::textAreaInput(
      inputId = form_id,
      label = label,
      rows = 1
    )
  }

  # simple date input
  if (form_type == "date") {
    # supress warning while trying keep data form empty by default
    suppressWarnings({
      form <- shiny::dateInput(
        inputId = form_id,
        label = label,
        value = NA, # keep empty
        format = "dd.mm.yyyy",
        weekstart = 1,
        language = "ru"
      )
    })
  }

  # единичный выбор
  if (form_type == "select_one") {
    form <- shiny::selectizeInput(
      inputId = form_id,
      label = label,
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
    form <- shiny::selectizeInput(
      inputId = form_id,
      label = label,
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
    form <- shiny::radioButtons(
      inputId = form_id,
      label = label,
      choices = choices,
      selected = character(0)
    )
  }

  if (form_type == "checkbox") {
    form <- shiny::checkboxGroupInput(
      inputId = form_id,
      # label = label,
      label = shiny::h6(form_label),
      choices = choices,
      selected = character(0)
    )
  }

  # вложенная таблица
  if (form_type == "inline_table") {
    form <- rhandsontable::rHandsontableOutput(outputId = form_id)
  }

  if (form_type == "inline_table2") {
    form <- shiny::actionButton(inputId = form_id, label = label)
  }

  # description part
  if (form_type == "description") {
    if(is.na(form_label)) {
      form <- shiny::hr(style = "margin-bottom: -3px;")
    } else {
      form <- shiny::div(shiny::HTML(form_label), style = "color: Gray; font-size: 90%;")
    }
  }

  if (form_type == "description_header") {
    form <- shiny::h5(
      label,
      style = "margin-bottom: -8px; margin-top: 10px;"
    )
  }

  # если есть условие создать кондитионал панель
  if (!is.na(condition)) {
    form <- shiny::conditionalPanel(
      condition = condition,
      form,
      ns = ifelse(missing(ns), shiny::NS(NULL), ns)
    )
  }

  if (is.null(form)) cli::cli_abort("невозможно создать форму типа '{form_type}' (id: '{form_id}') !")
  form
}


# SERVER LOGIC ==========================
#' @export
#' @description
#' Функция возращает пустое значение для каждого типа формы
get_empty_data <- function(type) {
  if (type %in% c("text", "select_one", "select_multiple")) return(as.character(NA))
  if (type %in% c("radio", "checkbox")) return(as.character(NA))
  if (type %in% c("date")) return(as.Date(NA))
  if (type %in% c("number")) as.character(NA)
}

#' @export
#' @description Function to update input forms (default variants only)
#' @param id - input form id;
#' @param type - type of form;
#' @param value - value to update;
#' @param local_delimeter - delimeter to split file
update_forms_with_data <- function(
  id,
  type,
  value,
  local_delimeter = getOption("SYMBOL_DELIM")
) {

  if (type == "text") {
    shiny::updateTextAreaInput(inputId = id, value = value)
  }

  if (type == "number") {
    shiny::updateTextAreaInput(inputId = id, value = value)
  }

  # supress warnings when applying NA or NULL to date input form
  if (type == "date") {
    suppressWarnings(
      shiny::updateDateInput(inputId = id, value = value)
    )
  }

  # select_one
  if (type == "select_one") {
    # update choices
    # old_choices <- subset(scheme, form_id == id, choices) |> dplyr::pull()
    # new_choices <- unique(c(old_choices, value))
    # new_choices <- new_choices[!is.na(new_choices)]

    # shiny::updateSelectizeInput(inputId = id, selected = value, choices = new_choices)
    shiny::updateSelectizeInput(inputId = id, selected = value)
  }

  # select_multiple
  # check if value is not NA and split by delimetr
  if (type == "select_multiple" && !is.na(value)) {
    vars <- stringr::str_split_1(value, local_delimeter)

    # update choices
    # old_choices <- subset(scheme, form_id == id, choices) |> dplyr::pull()
    # new_choices <- unique(c(old_choices, vars))
    # new_choices <- new_choices[!is.na(new_choices)]

    # shiny::updateSelectizeInput(inputId = id, selected = vars, choices = new_choices)
    shiny::updateSelectizeInput(inputId = id, selected = vars)
  }

  # in other case fill with `character(0)` to proper reseting form
  if (type == "select_multiple" && is.na(value)) {
    shiny::updateSelectizeInput(inputId = id, selected = character(0))
  }

  # radio buttons
  if (type == "radio" && !is.na(value)) {
    shiny::updateRadioButtons(inputId = id, selected = value)
  }
  if (type == "radio" && is.na(value)) {
    shiny::updateRadioButtons(inputId = id, selected = character(0))
  }

  # checkboxes
  if (type == "checkbox" && !is.na(value)) {
    vars <- stringr::str_split_1(value, local_delimeter)
    shiny::updateCheckboxGroupInput(inputId = id, selected = vars)
  }
  if (type == "checkbox" && is.na(value)) {
    shiny::updateCheckboxGroupInput(inputId = id, selected = character(0))
  }

  # if (type == "inline_table") {
  #   message("EMPTY")
  # }
}
