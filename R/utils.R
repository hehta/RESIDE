##################################################################
##                       Helper Functions                       ##
##################################################################
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
        "Info: No file for ",
        variable_type,
        " variables found"
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
        " variables file ",
        .full_file_path,
        " must Exist"
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
  # If the file path is empty ignore
  # and return and empty data frame
  if (file_path == "") {
    return(data.frame())
  }
  # Try to read the file
  tryCatch({
    return(
      utils::read.csv(
        file_path
      )
    )
  }, error = function(e) {
    # Else produce a custom error message
    stop(paste(
      "Error reading",
      variable_type,
      "variables from",
      file_path,
      "does the file exist?"
    ))
  })
}

max_decimal_places <- function(x) {
  # Filter NAS
  .x <- x[!is.na(x)]
  # Get a vector of decimal places using regex
  # and ignoring scientific notation e.g., 3.5e-13
  dps <- sapply(
    .x, function(y) {
      nchar(sub("^-?\\d*\\.?", "", format(y, scientific = FALSE)))
    }
  )
  # Return the maximum number of decimal places
  return(max(dps))
}

get_n_missing <- function(
  df,
  column
) {
  # Return the number of rows with NAs
  return(
    nrow( # Number of Rows with just NA
      as.data.frame( # Ensure it's a df as subsetting a single column
        df[is.na(df[column]), ]
      )
    )
  )
}

.write_csv <- function(
  df,
  file_path,
  variable_type
) {
  # Cat a message to state what is being exported and where
  cat(
    "Exporting",
    variable_type,
    "to:",
    file_path,
    "\n"
  )
  utils::write.csv(df, file_path)
}