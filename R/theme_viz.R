select_logic <- function(d, df, input, input_params, sing_values) {
  if (!d %in% input[["what_table_input"]]) return()
  if (is.null(df)) return()
  id_select <- input_params$selector[[d]]$id

  cond_select <- if("what" %in% names(id_select)) {
    sing_values[[id_select$what]]
  } else {
    input[[id_select]]
  }

  if (is.null(cond_select)) return()
  info_var <- input_params$selector[[d]][[cond_select]]
  var <-  if ("var_select" %in% names(info_var)) info_var$var_select
  if (is.null(var)) return()
  var_cat <- var$var_cat |> unlist(use.names = F)
  var_dat <- var$var_dat |> unlist(use.names = F)
  var_num <- var$var_num |> unlist(use.names = F)
  var_select <- unlist(var, use.names = FALSE)

  data_to_select <- df[,var_select]
  agg <- ifelse(is.null(info_var$agg), ifelse(!is.null(var$var_num), "sum", "count"), info_var$agg)

  data_to_select <- dsdataprep::aggregation_data(data = data_to_select,
                                                 agg = agg,
                                                 group_var = c(var_cat, var_dat),
                                                 to_agg = var_num,
                                                 agg_name = info_var$agg_name)
  theme <- if (!is.null(data_to_select)) list(data = data_to_select)
  if ("titles" %in% names(info_var)) {
    theme <- if (!is.null(theme)) modifyList(theme, info_var$titles) else info_var$titles
  }

  theme
}

theme_viz_func <- function(df, input, input_params, sing_values) {
  if (is.null(df)) return()
  theme_data <- NULL
  if ("selector" %in% names(input_params)) {

    theme_data <- purrr::map(names(input_params$selector),
                             select_logic,
                             df=df, input=input,
                             input_params=input_params,
                             sing_values=sing_values)

    theme_data <- purrr::flatten(theme_data)
  }
  theme_viz <- if ("theme" %in% names(input_params)) {
    input_params$theme
  } else {
    list()
  }


  if ("palette_colors" %in% names(input_params$theme)) {
    theme_viz$palette_colors <- theme_viz$palette_colors |> unlist(use.names = F)
  }
  if (!is.null(theme_data)) {
    theme_viz <- modifyList(theme_data, theme_viz)
  }

  theme_viz


}

