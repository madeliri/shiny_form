# box ========
.on_load = function(ns) {
  message(
    'Loading module "', box::name(), '"\n',
    'Module path: "', basename(box::file()), '"'
  )
}
# ================

# asdasd
#' @export
make_panels <- function(page_name, main_scheme) {

  # get info about inputs for current page
  page_forms <- main_scheme |>
    dplyr::filter(part == {{page_name}}) |>
    dplyr::distinct(subgroup, form_id, form_label, form_type)

  # get list of columns
  cols_list <- unique(page_forms$subgroup)

  # making cards
  cards <- purrr::map(
    .x = cols_list,
    .f = make_cards_fn,
    main_scheme = main_scheme
  )

  # make page wrap
  page_wrap <- bslib::layout_column_wrap(
    # width = "350px", height = NULL, #was 800
    width = 1 / 4, height = NULL, # was 800
    fixed_width = TRUE,
    # unpack list of cards
    !!!cards
  )

  # add panel wrap to nav_panel
  bslib::nav_panel(
    title = page_name,
    page_wrap
  )
}

# functions for making cards
#' @export
make_cards_fn <- function(sub_group, main_scheme) {

  main_scheme <<- main_scheme
  subgroups_inputs <- main_scheme |>
    dplyr::filter(subgroup == {{sub_group}}) |>
    dplyr::distinct(form_id, form_label, form_type)

  bslib::card(
    bslib::card_header(sub_group, container = htmltools::h5),
    full_screen = TRUE,
    width = "4000px",
    bslib::card_body(
      fill = TRUE,
      # передаем все аргументы в функцию для создания елементов
      purrr::pmap(
        .l = subgroups_inputs,
        .f = create_forms,
        main_scheme = main_scheme
      )
    )
  )
}

# UI RELATED ============================
#' @export
#' @param TEST s
create_forms <- function(
  form_id,
  form_label,
  form_type,
  main_scheme
) {

  # check if have condition
  condition <- dplyr::filter(main_scheme, form_id == {{form_id}}) |>
    dplyr::distinct(condition) |>
    dplyr::pull()

  choices <- dplyr::filter(main_scheme, form_id == {{form_id}}) |>
    dplyr::pull(choices)

  # simple text or number input
  if (form_type == "text") {

    # get info how much rows to render
    rows_to_show <- ifelse(!is.na(choices), as.integer(choices), 1)

    form <- shiny::textAreaInput(
      inputId = form_id,
      label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
      rows = rows_to_show
    )
  }

  if (form_type == "number") {
    form <- shiny::textAreaInput(
      inputId = form_id,
      label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
      rows = 1
    )
  }

  # simple date input
  if (form_type == "date") {
    # supress warning while trying keep data form empty by default
    suppressWarnings({
      form <- shiny::dateInput(
        inputId = form_id,
        label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
        value = NA, # keep empty
        format = "dd.mm.yyyy",
        weekstart = 1,
        language = "ru"
      )
    })
  }

  # еденичный выбор
  if (form_type == "select_one") {
    form <- shiny::selectizeInput(
      inputId = form_id,
      label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
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
      label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
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
      label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
      choices = choices,
      selected = character(0)
    )
  }

  if (form_type == "checkbox") {
    form <- shiny::checkboxGroupInput(
      inputId = form_id,
      # label = shiny::span(style = "color: #444444; font-weight: 550;", form_label),
      label = shiny::h6(form_label),
      choices = choices,
      selected = character(0)
    )
  }

  # вложенная таблица
  if (form_type == "inline_table") {
    form <- rhandsontable::rHandsontableOutput(outputId = form_id)
  }

  # description part
  if (form_type == "description") {
    form <- shiny::div(shiny::HTML(form_label), style = "color:Gray;font-size: 90%;")
  }

  # если есть условие создать кондитионал панель
  if (!is.na(condition)) {
    form <- shiny::conditionalPanel(
      condition = condition,
      form
    )
  }
  form
}




# SERVER LOGIC ==========================
#' @export
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
