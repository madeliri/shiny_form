
#' @export
scheme_R6 <- R6::R6Class(
  "schemes_f",
  public = list(

    initialize = function(scheme_file_path = NULL) {
      private$scheme_file_path <- scheme_file_path

      # make list of schemas
      private$schemes_list <- list()
      private$schemes_list[["main"]] <- private$load_scheme_from_xlsx("main")

      # имена вложенных форм
      private$nested_forms_names <- private$schemes_list[["main"]] |>
        dplyr::filter(form_type == "nested_forms") |>
        dplyr::distinct(form_id) |>
        dplyr::pull(form_id)

      purrr::walk(
        .x = purrr::set_names(private$nested_forms_names),
        .f = \(nested_form_id) {

          nested_form_scheme_sheet_name <- private$schemes_list[["main"]] |>
            dplyr::filter(form_id == {{nested_form_id}}) |>
            dplyr::distinct(form_id, .keep_all = TRUE) |>
            dplyr::pull(choices)

          # загрузка схемы для данной вложенной формы
          private$schemes_list[[nested_form_id]] <<- private$load_scheme_from_xlsx(nested_form_scheme_sheet_name)
        }
      )

      # extract main key
      private$main_key_id <- self$get_key_id("main")
    },

    get_all_ids = function(table_name) {

      private$schemes_list[[table_name]] |>
        dplyr::filter(!form_type %in% private$exluded_types) |>
        dplyr::distinct(form_id) |>
        dplyr::pull(form_id)

    },
    get_key_id = function(table_name) {

      ids <- self$get_all_ids(table_name)
      ids[1]

    },
    get_forms_ids = function(table_name) {

      ids <- self$get_all_ids(table_name)
      ids[-1]

    },

    extract_forms_id_and_types_from_scheme2 = function(scheme) {

      form_id_and_types_list <- scheme |>
        dplyr::filter(!form_type %in% private$exluded_types) |>
        dplyr::distinct(form_id, form_type) |>
        tibble::deframe()

      list(
        key = form_id_and_types_list[1],
        form = form_id_and_types_list[-1]
      )
    },

    # get_key_id = function(table_name) {
    #   self$extract_forms_id_and_types_from_scheme2(private$schemes_list[[table_name]])
    # },
    get_schema = function(table_name) {
      private$schemes_list[[table_name]]
    },
    get_id_type_list = function(table_name) {
      # wo main key
      this_key_id <- self$get_key_id(table_name)

      private$schemes_list[[table_name]] |>
        dplyr::filter(!form_type %in% private$exluded_types) |>
        dplyr::filter(form_id != {{this_key_id}}) |>
        dplyr::distinct(form_id, form_type) |>
        tibble::deframe()
    }
  ),
  active = list(
    get_main_key_id = function() {
      private$main_key_id
    },
    all_tables_names = function() {
      c("main", private$nested_forms_names)
    },
    nested_tables_names = function() {
      private$nested_forms_names
    }
  ),
  private = list(
    scheme_file_path = NA,
    schemes_list = NULL,
    main_key_id = NA,
    nested_forms_names = NA,
    exluded_types = c("inline_table", "nested_forms","description", "description_header"),

    load_scheme_from_xlsx = function(sheet_name) {

      colnames <- switch(sheet_name,
        "main" = c("part", "subgroup", "form_id", "form_label", "form_type"),
        c("subgroup", "form_id", "form_label", "form_type")
      )

      readxl::read_xlsx(FILE_SCHEME, sheet = sheet_name) |>
        # fill NA down
        tidyr::fill(all_of(colnames), .direction = "down") |>
        dplyr::group_by(form_id) |>
        tidyr::fill(c(condition, required), .direction = "down") |>
        dplyr::ungroup()

    }
  )
)