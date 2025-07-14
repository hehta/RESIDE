##################################################################
##                       Helper Functions                       ##
##################################################################
# Returns a list of missing variables from a data frame.
get_missing_variables <- function(
  df,
  variables
) {
  # Set up a vector for missing variables
  .missing_variables <- c()
  # Loop through the variables
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

# Joins a folder and file path normalising the folder path
# The folder path must exist.
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

# Returns the file paths to each of the variables
# dependant on the file and folder path specified.
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
    variable_type == "summary" && file_path == "" ~
      "summary.csv",
    variable_type == "variable_map" && file_path == "" ~
      "variable_map.csv",
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
    # Check if file exist to prevent warning
    if (!file.exists(file_path)) {
      stop("File does not exist")
    }
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

# Function to return the maximum decimal places
# From on object that can be coerced into a numeric vector
# e.g. a column
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

# Returns the number of missing values from a given column
# of a given data frame.
get_n_missing <- function(
  df,
  column
) {
  # Return the number of rows with NAs
  return(
    nrow( # Number of Rows with just NA
      as.data.frame( # Ensure it's a df as sub-setting a single column
        df[is.na(df[column]), ]
      )
    )
  )
}

# Writes a data frame to a CSV file with a given file name
# whilst outputting information on the export
.write_csv <- function(
  df,
  file_path,
  variable_type,
  row_names = TRUE
) {
  # Produce a message to state what is being exported and where
  message(
    paste(
      "Exporting ",
      variable_type,
      "to: ",
      file_path
    )
  )
  utils::write.csv(df, file_path, row.names = row_names)
}

.variable_map_to_df <- function(variable_map) {
  # Convert the variable map to a data frame
  df <- dplyr::bind_cols(
    .fill_na(variable_map,
             .get_max_length(variable_map)),
  )
  return(df)
}

# Function to check if marginal files exist
# in a given folder, returns a character vector
# of the marginal files that exist.
marginal_files_exist <- function(folder_path) {
  # Forward declare vector for files that exist
  .files_exist <- c()
  # loop through the default file names (see zzz.R)
  for (.file in .marginal_file_names) {
    # If the file exists in the folder
    if (
      file.exists(
        file.path(
          normalizePath(folder_path),
          .file
        )
      )
    ) {
      # Add the file to the vector
      .files_exist <- c(.files_exist, .file)
    }
  }
  # Return the vector
  return(.files_exist)
}

# Function to attempt to remove marginal files
remove_marginal_files <- function(folder_path) {
  # Loop through expected files (see zzz.R)
  for (.file in .marginal_file_names){
    # Remove the file using unlink with force = TRUE
    unlink(
      file.path(
        normalizePath(folder_path),
        .file
      ),
      force = TRUE
    )
  }
  # Check any marginal files still exist
  .marginal_files <- marginal_files_exist(folder_path)
  # If any marginal files still exist error.
  if (length(.marginal_files) > 0) {
    stop(paste(
      "Could not remove existing files the
      following files will need to be deleted manually:",
      .marginal_files,
      sep = " ",
      collapse = ", "
    ))
  }
}

# Function to determine if a data frame is in long format
is_long_format <- function(df, subject_identifier) {
  if (! subject_identifier %in% names(df)) {
    stop("Subject Identifier must be in data")
  }
  unique_ids <- unique(df[[subject_identifier]])
  if (length(unique_ids) < nrow(dplyr::distinct(df))) {
    return(TRUE)
  }
  return(FALSE)
}

# Function to get the long (format) columns from a data frame
get_long_columns <- function(df, subject_identifier) {
  # Forward declare long columns
  long_columns <- c()

  # Get unique subjects
  unique_subjects <- unique(df[[subject_identifier]])

  # Loop through subjects
  for (subject in unique_subjects) {
    # Loop through columns
    for (col in names(df)){
      # Ignore any existing long columns
      if (!col %in% long_columns) {
        # Get the rows for the subject
        subject_rows <- df[df[[subject_identifier]] == subject, ]
        # Check if the column has different values for any row
        if (length(unique(subject_rows[[col]])) > 1) {
          # Add the column to the long columns list
          long_columns <- c(col, long_columns)
        }
      }
    }
  }
  return(long_columns)
}

# Function to convert a data frame from long to wide format
long_to_wide <- function(df, subject_identifier, max_obs = 10) {

  long_columns <- get_long_columns(df, subject_identifier)

  wide_df <-
    df %>%
    dplyr::group_by(dplyr::pick({{subject_identifier}})) %>%
    mutate(row = dplyr::row_number())

  if (max(wide_df$row) > max_obs) {
    warning(
      paste0(
        "The number of observations per subject exceeds ",
        max_obs,
        ". Only the first ",
        max_obs,
        " will be retained."
      )
    )
    wide_df <- wide_df %>%
      dplyr::filter(row <= max_obs)
  }

  wide_df <- wide_df %>%
    tidyr::pivot_wider(
      names_from = row,
      names_sep = ".obs.",
      values_from = tidyr::all_of(long_columns)
    )
  return(wide_df)
}

is_wide_format <- function(df) {
  return(any(grepl(
    "\\.obs\\.",
    names(df)
  )))
}

wide_to_long <- function(df) {

  # Pivot the data frame to long format
  long_df <- df %>%
    tidyr::pivot_longer(
      cols = tidyr::contains(".obs."),
      names_to = c(".value", "obs"),
      names_sep = ".obs."
    )

  return(long_df)
}

.get_max_length <- function(l) {
  max_len <- 0
  for (v in l) {
    if(length(v) > max_len) {
      max_len = length(v)
    }
  }
  return(max_len)
}

.fill_na <- function(l, len) {
  for (i in seq_len(length(l))) {
    if (length(l[[i]]) < len) {
      m <- len - length(l[[i]])
      l[[i]] <- c(l[[i]], rep(NA, m))
    }
  }
  return(l)
}

.remove_attributes <- function(x) {
  attributes(x) <- list(); return(x)
}

is_multi_table <- function(marginals) {
  if("variable_map" %in% names(marginals)) {
    if (length(marginals$variable_map) > 1) {
      # If the variable map is greater than 1
      # then it is a multi table
      return(TRUE)
    }
  } 
  return(FALSE)
}

is_multi_table_long <- function(marginals) {
  if("summary" %in% names(marginals)) {
    if (any(grepl("n_row.df.*", names(marginals[["summary"]])))) {
      nrows <- marginals$summary[,grepl("n_row.df.*", names(marginals[["summary"]]))]
      return(!apply(nrows, 1, function(row) all(row == row[1])))
    }
  } 
  return(FALSE)
}