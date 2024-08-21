get_missing_variables <- function(
  df,
  variables
) {
  # Set up a vector for missing variables
  .missing_variables <- c()
  for (variable in variables) {
    if (!variable %in% names(df)) {
      .missing_variables <- c(.missing_variables, variable)
    }
  }
  return(.missing_variables)
}

binary_to_df <- function(
  x
) {
  `%>%` <- magrittr::`%>%`
  binary_df <- as.data.frame(x)
  binary_df <- binary_df %>%
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "variable",
      values_to = "mean"
    )
  return(binary_df)
}

quantiles_to_df <- function(
  x
) {
  .quantiles <- lapply(x, \(.x) .x[["quantiles"]])
  .quantiles <- do.call(rbind, .quantiles)
  return(.quantiles)
}

continuous_to_df <- function(
  x
) {
  `%>%` <- magrittr::`%>%`
  .summaries <- lapply(x, \(.x) .x[["summary"]])
  .summaries <- do.call(rbind, .summaries) %>%
    tibble::rownames_to_column(var = "variable")
  return(.summaries)
}

categorical_to_df <- function(
  x
) {
  .cat_df <- data.frame()
  for (c_var in names(x)) {
    .tmp_df <- as.data.frame(x[[c_var]]) %>%
      tibble::rownames_to_column(var = "category")
    names(.tmp_df) <- c("category", "n")
    .tmp_df$variable <- c_var
    .cat_df <- rbind(.cat_df, .tmp_df)
  }
  return(.cat_df)
}

generate_file_path <- function(
  file_name,
  folder_path,
  variable_type
) {
  .file_path <- file.path(normalizePath(folder_path), file_name)
  cat(
    "Exporting",
    variable_type,
    "to:",
    .file_path,
    "\n"
  )
  return(.file_path)
}

get_full_file_path <- function(
  folder_path,
  file_path
) {
  return(
    file.path(
      normalizePath(folder_path),
      file_path
    )
  )
}

get_variables_path <- function(
  folder_path,
  file_path,
  variable_type
) {
  .file_path <- dplyr::case_when(
    variable_type == "binary" && file_path == "" ~
      "binary_variables.csv",
    variable_type == "categorical" && file_path == "" ~
      "categorical_variables.csv",
    variable_type == "continuous" && file_path == "" ~
      "continuous_variables.csv",
    variable_type == "quantiles" && file_path == "" ~
      "continuous_quantiles.csv",
    file_path != "" ~ file_path,
    TRUE ~ ""
  )
  .full_file_path <- get_full_file_path(
    folder_path,
    .file_path
  )
  if (file_path == "" && !file.exists(.full_file_path)) {
    message(
      paste0(
        "No file for ",
        variable_type,
        " found"
      )
    )
    return("")
  } else if (!file.exists(.full_file_path)) {
    stop(
      paste0(
        variable_type,
        "Must Exist"
      )
    )
  } else {
    return(.full_file_path)
  }
}

load_variables_file <- function(
  file_path,
  variable_type
) {
  if (file_path == "") {
    return()
  }
  tryCatch({
    return(
      utils::read.csv(
        file_path
      )
    )
  }, error = function(e) {
    stop(paste(
      "Error reading",
      variable_type,
      "variables from",
      file_path,
      "does the file exist?"
    ))
  })
}