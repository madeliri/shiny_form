
#' @export
init_val <- function(scheme, ns) {

  options(box.path = here::here())
  box::use(modules/data_manipulations[check_for_empty_data])

  iv <- shinyvalidate::InputValidator$new()

  # если передана функция с пространством имен, то происходит модификация id
  if(!missing(ns)) {
    scheme <- scheme |>
      dplyr::mutate(form_id = ns(form_id))
  }

  # формируем список id - тип
  inputs_simple_list <- scheme |>
    dplyr::filter(!form_type %in% c("nested_forms","description", "description_header")) |>
    dplyr::distinct(form_id, form_type) |>
    tibble::deframe()

  # add rules to all inputs
  purrr::walk(
    .x = names(inputs_simple_list),
    .f = \(x_input_id) {

      form_type <- inputs_simple_list[[x_input_id]]

      choices <- dplyr::filter(scheme, form_id == {{x_input_id}}) |>
        dplyr::pull(choices)

      val_required <- dplyr::filter(scheme, form_id == {{x_input_id}}) |>
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
          # хак для пропуска значений
          if (x == "NA") return(NULL)

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

                # exit if empty
                if (check_for_empty_data(x)) {
                  return(NULL)
                }

                if (x == "NA") return(NULL)

                # замена разделителя десятичных цифр
                x <- stringr::str_replace(x, ",", ".")

                # check for currect value
                if (dplyr::between(as.double(x), ranges[1], ranges[2])) {
                  NULL
                } else {
                  glue::glue("Значение должно быть между {ranges[1]} и {ranges[2]}.")
                }
              }
            )
          }
        }
      }

      if (form_type %in% c("select_multiple", "select_one")) {
        iv$add_rule(x_input_id, function(x) {

          if (length(x) == 1) {
            if (check_for_empty_data(x)) return(NULL)
          }

          # проверка на соответствие вариантов схеме ---------
          compare_to_dict <- (x %in% choices)
          if (!all(compare_to_dict)) {

            text <- paste0("'",x[!compare_to_dict],"'", collapse = ", ")
            glue::glue("варианты, не соответствующие схеме: {text}")
          }
        })

      }


      # if in `required` column value is `1` apply standart validation
      if (!is.na(val_required) && val_required == 1) {
        iv$add_rule(x_input_id, shinyvalidate::sv_required(message = "Необходимо заполнить."))
      }
    }
  )
  iv
}