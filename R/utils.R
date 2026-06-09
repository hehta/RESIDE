##################################################################
##                       Helper Functions                       ##
##################################################################
# Returns a list of missing variables from a data frame.
get_missing_variables <- function(
  dfs,
  variables
) {
  # Set up a vector for missing variables
  .missing_variables <- c()
  # Get all the columns from the data frame(s)
  all_variables <- get_all_columns(df)
  # Loop through the variables
  for (variable in variables) {
    # If the variable is not in the columns of the data frame
    if (!variable %in% all_variables) {
      # Add the variable to the missing variables vector
      .missing_variables <- c(.missing_variables, variable)
    }
  }
  # Return the missing variables vector
  return(.missing_variables) #nolint: return
}

filter_variables <- function(
  dfs,
  variables
) {
  .dfs <- lapply(dfs, function(df) {df[names(df) %in% variables]})
  .dfs
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

.filter_sub_marginals <- function(
  marginals,
  variables,
  keep_subject_identifier = TRUE
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
      new_marginals$summary$subject_identifier <- ifelse(
        keep_subject_identifier,
        marginals$summary$subject_identifier,
        ""
      )
    }
    if ("data_frame" %in% names(marginals$summary)) {
      new_marginals$summary$data_frame <- marginals$summary$data_frame
    }
  }
  return(new_marginals) #nolint: return
}

.filter_marginals <- function(
  marginals,
  variables,
  keep_subject_identifier = TRUE
) {
  marginals_copy <- marginals
  if ("overall_summary" %in% names(marginals)) {
    marginals_copy[["overall_summary"]] <- NULL
  }
  # Filter the marginals to only include the specified variables
  marginals_copy <- lapply(marginals_copy,
    .filter_sub_marginals,
    variables,
    keep_subject_identifier
  )
  data_frame_names <- get_df_names_or_key(marginals_copy)
  marginals_copy$overall_summary <- data.frame(
    n_data_frames = length(data_frame_names),
    data_frame_names = paste(data_frame_names, collapse = ", "),
    subject_identifier = ifelse(
      "subject_identifier" %in% names(marginals$overall_summary)
      && keep_subject_identifier,
      marginals$overall_summary$subject_identifier,
      ""
    ),
    n_subjects = ifelse(
      "n_subjects" %in% names(marginals$overall_summary),
      marginals$overall_summary$n_subjects,
      0
    ),
    common_columns = ifelse(
      "common_columns" %in% names(marginals$overall_summary),
      marginals$overall_summary$common_columns,
      ""
    )
  )
  # Return the filtered marginals
  return(marginals_copy) #nolint: return
}

.filter_common_marginals <- function(
  marginals,
  keep_subject_identifier = TRUE
) {
  # Get the common columns from the marginals
  common_columns <- get_common_columns(marginals, "subject_identifier")
  # Filter the marginals to only include the common columns
  common_marginals <- .filter_marginals(
    marginals,
    common_columns,
    keep_subject_identifier
  )
  # Return the filtered marginals
  return(common_marginals) #nolint: return
}

get_n_col <- function(marginals) {
  # forward declare n_col
  n_col <- 0
  # variable types to check
  variable_types <-
    get_default_variable_types()
  # loop through variable types
  for (variable_type in variable_types) {
    # only count if the variable type is present
    if (variable_type %in% names(marginals)) {
      # add the number of columns for this variable type
      n_col <- n_col + length(marginals[[variable_type]])
    }
  }
  # Explicitly return the number of columns
  return(n_col) #nolint: return
}

get_default_variable_types <- function() {
  return(c( #nolint: return
    "categorical_variables",
    "binary_variables",
    "continuous_variables"
  ))
}

get_submarginal_variables <- function(sub_marginals) {
  # forward declare variables
  variables <- c()
  variable_types <- get_default_variable_types()
  for (variable_type in variable_types) {
    if (variable_type %in% names(sub_marginals)) {
      variables <- c(variables, names(sub_marginals[[variable_type]]))
    }
  }
  return(variables) #nolint: return
}

get_variables <- function(marginals) {
  # forward declare variables
  variables <- c()
  df_names <- get_df_names_or_key(marginals)
  # Get variables from each sub-marginal
  for (df_name in df_names) {
    variables <-
      c(variables, get_submarginal_variables(marginals[[df_name]]))
  }
  return(unique(variables)) #nolint: return
}

.split_variables <- function(variables) {
  # Split the variables by comma and trim whitespace
  variables <- strsplit(variables, ",")[[1]]
  variables <- trimws(variables)
  return(variables) # nolint: return
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
  if (!is.character(col)) {
    return(FALSE) #nolint: return
  }
  dates <- as.Date(col, optional = TRUE)
  if (
    !all(is.na(dates)) &&
      length(na.omit(dates)) > length(na.omit(dates)) * threshold
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

get_dates_from_sub_marginals <- function(sub_marginals) {
  dates <- c()
  if ("continuous_variables" %in% names(sub_marginals)) {
    for (variable_name in names(sub_marginals$continuous_variables)) {
      variable <- sub_marginals$continuous_variables[[variable_name]]
      if ("is_date" %in% names(variable[["summary"]])) {
        if (variable$summary$is_date) {
          dates <- c(dates, variable_name)
        }
      }
    }
  }
  return(dates) #nolint: return
}

.back_transform_dates <- function(sub_marginals, sim_df) {
  # Convert numeric dates back to Date format
  date_variables <- get_dates_from_sub_marginals(sub_marginals)
  for (col in names(sim_df)) {
    if (col  %in% date_variables) {
      sim_df[[col]] <- as.Date(sim_df[[col]], origin = "1970-01-01")
    }
  }
  return(sim_df) #nolint: return
}


.convert_date_columns <- function(df, threshold = 0.2) {
  is_date <- c()
  # Convert columns to date if they are in date format
  for (col in names(df)) {
    if (.is_date(df[[col]])) {
      df[[col]] <- .as_numeric_date(df[[col]])
      is_date <- c(is_date, TRUE)
    } else {
      is_date <- c(is_date, FALSE)
    }
  }
  df <- set_col_attr(df, is_date)
  return(df) #nolint: return
}

get_df_names_or_key <- function(marginals) {
  df_names <- names(marginals[names(marginals) %in% "overall_summary" == FALSE])
  if (any(df_names == "") || is.null(df_names)) {
    df_names <- seq_along(marginals)
    if ("overall_summary" %in% names(marginals)) {
      df_names <- df_names[
        df_names != which(names(marginals) == "overall_summary")
      ]
    }
  }
  return(df_names) #nolint: return
}

get_common_subjects <- function(dfs, subject_identifier) {
  subjects <- lapply(dfs, function (x){
    unique(x[[subject_identifier]])
  })
  common_subjects <- Reduce(intersect, subjects)
  return(common_subjects) #nolint: return
}

get_n_unique_subjects <- function(dfs, subject_identifier) {
  subjects <- lapply(dfs, function(x) {
    unique(x[[subject_identifier]])
  })
  n_subjects <- length(unique(unlist(subjects)))
  return(n_subjects) #nolint: return
}

get_all_columns <- function(dfs) {
  all_columns <- c()
  for (df in dfs)  {
    all_columns <- c(
      all_columns,
      names(df)
    )
  }
  return(all_columns) #nolint: return
}

get_common_columns <- function(dfs, subject_identifier) {
  # Get all the column names
  all_columns <- get_all_columns(dfs)
  # Get all the columns with duplicated names
  dup_col_names <- all_columns[duplicated(all_columns)]

  # Forward declare vector for matched columns
  matched_cols <- c()

  # Iterate through the column matches
  for (col in dup_col_names) {
    # Ignore the subject identifier
    if (col == subject_identifier) {
      # Do nothing
    } else {
      # Add the dataframes to a list if it contains a duplicate column name
      col_matches <- lapply(dfs, function(x) {
        if (col %in% names(x)) {
          return (x) #nolint: return
        } else {
          return (NULL) #nolint: return
        }
      })
      # Remove any Null values from previous steps
      col_matches[sapply(col_matches, is.null)] <- NULL
      # Get the common subjects
      common_subjects <- get_common_subjects(col_matches, subject_identifier)
      # Use an lapply to get
      col_matches <- lapply(col_matches, function(x) {
        rtn <- x[x[[subject_identifier]] %in% common_subjects,]
        rtn <- rtn[!duplicated(rtn[[subject_identifier]]),]
        rtn <- rtn[order(rtn[[subject_identifier]]),]
        rtn <- rtn[[col]]
      })
      eq_cols <- lapply(col_matches, function (x) x == col_matches[[1]])
      eq_cols <- lapply(eq_cols, all)
      if (all(unlist(eq_cols))) {
        matched_cols <- c(matched_cols, col)
      }
    }
  }
  matched_cols <- unique(matched_cols)
  return(matched_cols) #nolint: return
}

# Subsetter
get_dates <- function(
  df,
  is_date
) {
  df[sapply(df, function(x) attr(x, "is_date") == is_date)]
}

# Gets attributes as vector
get_col_attr <- function(df) sapply(df, attr, "is_date")

# Sets attributes to columns from a single vector
set_col_attr <- function(
  df,
  attrs
) {
  as.data.frame(mapply(function(col, is_date) {
    attr(col, "is_date") <- is_date
    col
  }, df, attrs, SIMPLIFY = FALSE))
}

get_all_types <- function(marginals) {
  # Forward declare vector for marginal types
  marginal_types <- c()
  # Store only the marginal names, not the overall summary, for cleaner code
  df_names <- get_df_names_or_key(marginals)
  # Loop through the marginals and get the types of marginals
  for (i in seq_along(df_names)) {
    marginal_types <- c(marginal_types, names(marginals[[i]]))
  }
  marginal_types <- unique(marginal_types)
  marginal_types <-
    marginal_types[marginal_types %in% get_default_variable_types()]
  return(marginal_types) #nolint: return
}

is_single_table <- function(marginals) {
  # Extract the marginal names, excluding the overall summary if it exists,
  # to check if there is only one table done on two lines for readbility.
  df_names <- get_df_names_or_key(marginals)
  return(length(df_names) == 1)
}