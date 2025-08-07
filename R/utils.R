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
  return(.missing_variables) #nolint: return
}

# Joins a folder and file path normalising the folder path
# The folder path must exist.
get_full_file_path <- function(
  folder_path,
  file_path
) {
  # Return a file path joining the folder path with the file path
  # Using normalize path to get an absolute path
  return( #nolint: return
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
  return(.full_file_path) #nolint: return
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
      "does the file exist?",
      e$message
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
  return(max(dps)) #nolint: return
}

# Returns the number of missing values from a given column
# of a given data frame.
get_n_missing <- function(
  df,
  column
) {
  # Return the number of rows with NAs
  return( #nolint: return
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
  return(.files_exist) #nolint: return
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
  return(FALSE) #nolint: return
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
  return(long_columns) #nolint: return
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

is_multi_table <- function(marginals) {
  # if the summary contains a data frame with variables
  if (any(grepl("variables.df.", names(marginals$summary)))) {
    return(TRUE)
  }
  return(FALSE) #nolint: return
}

is_multi_table_long <- function(marginals) {
  if (is_multi_table(marginals)) {
    nrows <- marginals$summary[grepl("n_row.df.", names(marginals$summary))]
    u_nrows <- unique(as.numeric(nrows))
    if (length(u_nrows) > 1) {
      return(TRUE)
    }
  }
  return(FALSE) #nolint: return
}


.filter_marginals <- function(
  marginals,
  variables
) {
  new_marginals <- list()
  for (variable in variables) {
    if ("categorical_variables" %in% names(marginals)) {
      if (variable %in% names(marginals$categorical_variables)) {
        new_marginals$categorical_variables[[variable]] <-
          marginals$categorical_variables[[variable]]
      }
    }
    if ("binary_variables" %in% names(marginals)) {
      if (variable %in% names(marginals$binary_variables)) {
        new_marginals$binary_variables[[variable]] <-
          marginals$binary_variables[[variable]]
      }
    }
    if ("continuous_variables" %in% names(marginals)) {
      if (variable %in% names(marginals$continuous_variables)) {
        new_marginals$continuous_variables[[variable]] <-
          marginals$continuous_variables[[variable]]
      }
    }
  }
  if ("summary" %in% names(marginals)) {
    new_marginals$summary <- data.frame(
      n_row = marginals$summary$n_row,
      n_col = length(variables),
      variables = paste(variables, collapse = ", ")
    )
    if ("subject_identifier" %in% names(marginals$summary)) {
      new_marginals$subject_identifier <- marginals$summary$subject_identifier
    }
  }
  class(new_marginals) <- "RESIDE"
  return(new_marginals) #nolint: return
}

get_n_col <- function(marginals) {
  n_col <- 0
  variable_types <-
    c("categorical_variables", "binary_variables", "continuous_variables")
  for (variable_type in variable_types) {
    if (variable_type %in% names(marginals)) {
      n_col <- n_col + length(marginals[[variable_type]])
    }
  }
  return(n_col) #nolint: return
}

get_variables <- function(marginals) {
  variables <- c()
  variable_types <-
    c("categorical_variables", "binary_variables", "continuous_variables")
  for (variable_type in variable_types) {
    if (variable_type %in% names(marginals)) {
      variables <- c(variables, names(marginals[[variable_type]]))
    }
  }
  return(variables) #nolint: return
}

get_summary_variables <- function(marginals) {
  if (!"summary" %in% names(marginals)) {
    stop("Marginals do not contain a summary.")
  }
  if (!"variables" %in% names(marginals$summary)) {
    stop("Marginals summary does not contain variables.")
  }

  variables <- marginals$summary$variables

  variables <- .split_variables(variables)

}

.get_keys <- function(marginals) {
  .keys <-
    names(marginals$summary)[grepl("n_row.df.", names(marginals$summary))]
  .keys <- gsub("n_row.df.", "", .keys)
  return(.keys) #nolint: return
}

.split_variables <- function(variables) {
  # Split the variables by comma and trim whitespace
  variables <- strsplit(variables, ",")[[1]]
  variables <- trimws(variables)
  return(variables) # nolint: return
}

.filter_baseline_by_key <- function(
  marginals,
  baseline_df,
  key
) {
  baseline_variables <- names(baseline_df)
  baseline_key <-
    baseline_variables[grepl(paste0(".df.", key), baseline_variables)]
  vm_variables <-
    .split_variables(marginals[["summary"]][[paste0("variables.df.", key)]])
  baseline_variables <- intersect(baseline_variables, vm_variables)
  baseline_variables <- c("id", baseline_key, baseline_variables)
  bl_df <- baseline_df %>%
    dplyr::select(dplyr::any_of(baseline_variables))
  return(bl_df) #nolint: return
}

.replace_nas <- function(df) {
  df <- df %>% dplyr::mutate_if(
    is.character,
    function(x) ifelse(x == "NA's", "", x)
  )
}

.get_largest_n_row <- function(marginals) {
  # Get the largest n_row from the summary
  if ("summary" %in% names(marginals)) {
    n_rows <-
      marginals$summary[grepl("n_row", names(marginals$summary))]
    return(max(n_rows, na.rm = TRUE)) #nolint: return
  }
  return(0) #nolint: return
}

.is_date <- function(col, threshold = 0.2) {
  dates <- as.Date(col, optional = TRUE)
  if (
    !all(is.na(dates)) && length(na.omit(dates)) > length(dates) * threshold
  ) {
    return(TRUE)
  }
  return(FALSE) #nolint: return
}

.as_numeric_date <- Vectorize(function(x) {
  epoch <- as.Date("1970-01-01")
  difftime(as.Date(x, optional = TRUE), epoch, units = "days")
}, USE.NAMES = FALSE
)


.convert_date_columns <- function(df, threshold = 0.2) {
  # Convert columns to date if they are in date format
  for (col in names(df)) {
    if (.is_date(df[[col]])) {
      df[[col]] <- .as_numeric_date(df[[col]])
      names(df)[names(df) == col] <- paste0(col, ".nm.date")
    }
  }
  return(df) #nolint: return
}