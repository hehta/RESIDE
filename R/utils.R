get_missing_variables <- function(
  df,
  variables
) {
  # Set up a vector for missing variables
  .missing_variables <- c()
  # Loop throught the variables
  for (variable in variables) {
    # If the variable is not in the columns of the data frame
    if (!variable %in% names(df)) {
      # Add the variable to the missing variables vector
      .missing_variables <- c(.missing_variables, variable)
    }
  }
  # Return the missing variables vector
  return(.missing_variables)
}

binary_to_df <- function(
  x
) {
  # Scope the magrittr pipe
  `%>%` <- magrittr::`%>%`
  # Convert the binary variables list to a data frame
  binary_df <- as.data.frame(x)
  # Transform the data frame from wide to long
  # The data frame should contain variables and means
  binary_df <- binary_df %>%
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "variable",
      values_to = "mean"
    )
  # Return the data frame
  return(binary_df)
}

quantiles_to_df <- function(
  x
) {
  # Using lapply select the quantiles list from each
  # variables list
  .quantiles <- lapply(x, \(.x) .x[["quantiles"]])
  # Using do.call and rbind add all the quantiles a single data frame
  .quantiles <- do.call(rbind, .quantiles)
  # Return the data frame
  return(.quantiles)
}

continuous_to_df <- function(
  x
) {
  # Scope the magrittr pip
  `%>%` <- magrittr::`%>%`
  # Use lapply to extract the summary (list) of each variable
  # from the variables list
  .summaries <- lapply(x, \(.x) .x[["summary"]])
  # Use do.call and rbind to add all the summaries to a single data frame
  # And convert the row name (variable name) to a column named variable
  .summaries <- do.call(rbind, .summaries) %>%
    tibble::rownames_to_column(var = "variable")
  # Return the summaries data frame
  return(.summaries)
}

categorical_to_df <- function(
  x
) {
  # Forward declare and empty data frame
  .cat_df <- data.frame()
  # Loop through the variable names in the categorical variables list
  for (c_var in names(x)) {
    # Using the name select the variable and convert it to a data frame
    # Then convert the row names (categories) to a column
    .tmp_df <- as.data.frame(x[[c_var]]) %>%
      tibble::rownames_to_column(var = "category")
    # Rename the columns
    names(.tmp_df) <- c("category", "n")
    # add the variable name as a column
    .tmp_df$variable <- c_var
    # combine with the earlier declared data frame
    .cat_df <- rbind(.cat_df, .tmp_df)
  }
  # Return the data frame
  return(.cat_df)
}

generate_file_path <- function(
  file_name,
  folder_path,
  variable_type
) {
  # Create a file path, normalizing the folder path first
  # to get an absolute path
  .file_path <- file.path(normalizePath(folder_path), file_name)
  # Cat a message to state what is being exported and where
  cat(
    "Exporting",
    variable_type,
    "to:",
    .file_path,
    "\n"
  )
  # Return the file path
  return(.file_path)
}

get_full_file_path <- function(
  folder_path,
  file_path
) {
  # Return a file path joining the folder path with the file path
  # Using normalize path to get an absolute path
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
  # If no file path is given set a default file path
  # See export_marginal_distributions()
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
  # Combine the folder and file paths
  .full_file_path <- get_full_file_path(
    folder_path,
    .file_path
  )
  # If assumed a default path and that path doesn't exist
  # Assume there where no variables of that type exported
  # But print a message
  if (file_path == "" && !file.exists(.full_file_path)) {
    message(
      paste0(
        "No file for ",
        variable_type,
        " found"
      )
    )
    # And return and empty string
    return("")
    # Other wise if the file does not exist
  } else if (!file.exists(.full_file_path)) {
    # Throw an error
    stop(
      paste0(
        variable_type,
        "Must Exist"
      )
    )
  }
  # If the file exists return the path
  return(.full_file_path)
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