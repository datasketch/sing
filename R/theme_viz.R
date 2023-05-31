select_logic <- function(d, df, input, input_params, sing_values) {
  if (!d %in% input[["what_table_input"]]) return()
  if (is.null(df)) return()
  if (nrow(df) == 0) return()
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
  var_list <- purrr::map(names(var), function(v) {
    var_select <- NULL
    var_opts <- var[[v]]
    if ("what" %in% names(var_opts)) {
      var_select <- input[[var_opts$what]]
    } else {
      var_select <- var_opts
    }
  })
  names(var_list) <- names(var)
  var_cat <- var_list$var_cat |> unlist(use.names = F)
  var_dat <- var_list$var_dat |> unlist(use.names = F)
  var_num <- var_list$var_num |> unlist(use.names = F)
  var_select <- unlist(var_list, use.names = FALSE)

  data_to_select <- df[,var_select]

  agg <- ifelse(is.null(info_var$agg),
                ifelse(!is.null(var_num), "sum", "count"), info_var$agg)

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
    tv <- input_params$theme
    if ("palette_colors" %in% names(input_params$theme)) {
      tv$palette_colors <- tv$palette_colors |> unlist(use.names = F)
    }
    tv
  } else {
    theme_data
  }

  if (!is.null(theme_data)) {
    if (!identical(theme_data, theme_viz)) {
    theme_viz <- modifyList(theme_data, theme_viz)
    }
  }

  theme_viz


}

