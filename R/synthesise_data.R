#' @title Synthesise data from marginal distributions
#' @description Allows the synthesis of data from marginal
#' distributions obtained from a Trusted Research Environment (TRE)
#' @param marginals an object of class RESIDE
#' @param correlation_matrix No longer supported, use \code{correlations}.
#' Default: NULL
#' @param correlations A list of correlations created with the
#' \code{\link{correlation}} function, Default: NULL
#' @param ... Additional parameters currently none are used.
#' @return a data frame of simulated data, or a named list of data frames
#' for marginals from multiple data frames.
#' @details This function will synthesise a dataset from marginals imported
#' using \code{\link{import_marginal_distributions}}.
#' By default the dataset will not contain correlations,
#' however user specified correlations can be added using
#' the \code{correlations} parameter, see \code{\link{correlation}}.
#' Categorical variables are correlated using a single category,
#' specified with \code{factor_name.x} or \code{factor_name.y}.
#' Correlated variables are synthesised together, one row per subject,
#' and joined to each data frame by subject. Correlated variables therefore
#' take a single value per subject within each data frame.
#' It is not possible to entirely maintain the marginal distributions
#' when specifying correlations.
#' @examples
#' \dontrun{
#'    marginals <- import_marginal_distributions()
#'    df <- synthesise_data(marginals)
#'    df_cor <- synthesise_data(
#'      marginals,
#'      correlations = list(
#'        correlation("AGE", "RSBP", 0.3),
#'        correlation("SEX", "AGE", -0.2, factor_name.x = "M")
#'      )
#'    )
#' }
#' @seealso
#'  \code{\link{correlation}}
#' @rdname synthesise_data
#' @export
#' @importFrom methods is
#' @importFrom simstudy defData
#' @importFrom simstudy genData
synthesise_data <- function(
  marginals,
  correlation_matrix = NULL,
  correlations = NULL,
  ...
) {
  # Check class
  if (!methods::is(marginals, "RESIDE")) {
    stop("object must be of class RESIDE")
  }
  if (!is.null(correlation_matrix)) {
    stop(
      paste(
        "correlation_matrix is no longer supported,",
        "use correlations with the correlation() function instead."
      )
    )
  }
  # If there are no correlations
  # then synthesise the data without correlations
  if (is.null(correlations)) {
    if (is_single_table(marginals)) {
      sim_df <- synthesise_data_no_cor(
        marginals[[get_df_names_or_key(marginals)]]
      )
    } else {
      sim_df <- synthesise_data_multi_no_cor(marginals)
      sim_df <- replace_common_fields(
        marginals,
        sim_df
      )
    }
    return(sim_df)
  }
  # Allow a single correlation to be supplied
  if (methods::is(correlations, "correlation")) {
    correlations <- list(correlations)
  }
  if (
    !is.list(correlations) ||
      !all(vapply(correlations, methods::is, logical(1), "correlation"))
  ) {
    stop(
      paste(
        "correlations must be a list of correlations",
        "created with the correlation() function."
      )
    )
  }
  sim_df <- synthesise_data_multi_cor(marginals, correlations)
  if (is_single_table(marginals)) {
    sim_df <- sim_df[[1]]
  }
  return(sim_df)
}

#' @rdname synthesise_data
#' @export
synthesize_data <- synthesise_data

# Internal function to synthesise data without correlations
synthesise_data_no_cor <- function(
  sub_marginals,
  date_transform = TRUE
) {
  # Predefine dataDefinition
  data_def <- get_data_def(sub_marginals, FALSE)

  # Synthesise the data
  sim_df <- simstudy::genData(
    sub_marginals$summary$n_row,
    data_def
  )

  # Convert the internal category names back to the original categories
  sim_df <- decode_categories(
    sim_df,
    sub_marginals$categorical_variables
  )

  # Back transform continuous variables
  sim_df <- back_transform_continuous(
    sim_df,
    sub_marginals
  )

  # Add missing values (MAR)
  sim_df <- add_missingness(
    sim_df,
    sub_marginals
  )

  if (date_transform) {
    sim_df <- .back_transform_dates(sub_marginals, sim_df)
  }

  sim_df <- reindex_df(
    sub_marginals,
    sim_df
  )

  # Return the data frame
  return(sim_df)
}

# Internal function to synthesise correlated variables together,
# returns a data frame with an id column and a column per variable.
# Missing values and dates are added by the caller, once the variables
# are joined to their data frames.
synthesise_data_cor <- function(
  sub_marginals,
  correlations
) {
  # Categorical variables are defined as dummy (binary) variables
  data_def <- get_data_def(sub_marginals, TRUE)
  correlation_matrix <- generate_correlation_matrix(
    sub_marginals,
    correlations
  )
  # Check the correlations are consistent with each other
  eigen_values <- eigen(
    correlation_matrix,
    symmetric = TRUE,
    only.values = TRUE
  )$values
  if (min(eigen_values) < -1e-8) {
    stop(
      paste(
        "The correlations are not consistent with each other,",
        "the correlation matrix is not positive semi definite."
      )
    )
  }

  # Synthesise the data
  sim_df <- as.data.frame(
    simstudy::genCorFlex(
      sub_marginals$summary$n_row,
      data_def,
      corMatrix = correlation_matrix
    )
  )

  # Replace dummy categories where rows add up to 0 or more than 1
  sim_df <- fix_factors(
    sim_df,
    sub_marginals,
    get_correlated_categories(correlations)
  )

  sim_df <- restore_factors(
    sim_df,
    sub_marginals$categorical_variables
  )

  # Back transform continuous variables
  sim_df <- back_transform_continuous(
    sim_df,
    sub_marginals
  )

  # Reorder dataframe
  column_names <- c("id", get_submarginal_variables(sub_marginals))
  sim_df <- sim_df[, column_names, drop = FALSE]

  return(sim_df)
}

synthesise_data_multi_no_cor <- function(marginals) {
  # Forward declare list of data frames to be returned
  sim_dfs <- list()
  # The number of subjects of each data frame is needed before
  # it is overwritten with the overall number of subjects
  original_marginals <- marginals
  marginals <- add_n_subjects(marginals)
  n_subjects <- get_overall_n_subjects(marginals)
  df_names <- get_df_names_or_key(marginals)
  for (df in df_names) {
    sub_marginals <- marginals[[df]]
    n_df_subjects <- get_df_n_subjects(original_marginals[[df]], n_subjects)
    sub_marginals$summary$n_subjects <- n_df_subjects
    .sim_df <- synthesise_data_no_cor(sub_marginals)
    sim_dfs[[df]] <- sample_subjects(
      .sim_df,
      get_id_name(sub_marginals),
      n_subjects,
      n_df_subjects
    )
  }

  return(sim_dfs)
}

# Synthesise data with correlations in three parts:
# correlated variables are synthesised together, one row per subject,
# non correlated variables are synthesised separately for each data frame,
# and common variables are synthesised separately (unless correlated).
# The correlated variables are sampled to the number of subjects
# of each data frame and joined to the non correlated variables.
synthesise_data_multi_cor <- function(marginals, correlations) {
  validate_correlations(marginals, correlations)
  check_categorical_factor_names(marginals, correlations)
  # Forward declare list of data frames to be returned
  sim_dfs <- list()
  # The number of subjects of each data frame is needed before
  # it is overwritten with the overall number of subjects
  original_marginals <- marginals
  marginals <- add_n_subjects(marginals)
  n_subjects <- get_overall_n_subjects(marginals)
  common_fields <- .get_common_fields(marginals)
  common_fields <- common_fields[common_fields != ""]

  # Synthesise the correlated variables together, one row per subject
  correlated <- get_correlated_variables(
    marginals,
    correlations,
    common_fields
  )
  correlated_df <- synthesise_data_cor(
    get_correlated_marginals(marginals, correlated, n_subjects),
    encode_correlations(marginals, correlations, correlated, common_fields)
  )
  correlated_df <- decode_correlated_df(marginals, correlated_df, correlated)

  for (df in get_df_names_or_key(marginals)) {
    sub_marginals <- marginals[[df]]
    variables <- get_submarginal_variables(sub_marginals)
    # Get the correlated variables belonging to this data frame
    df_correlated <- Filter(
      function(item) {
        item$variable %in% variables && (item$common || identical(item$df, df))
      },
      correlated
    )
    correlated_variables <- unname(
      vapply(df_correlated, `[[`, character(1), "variable")
    )
    # Synthesise the non correlated variables
    non_correlated_marginals <- .filter_sub_marginals(
      sub_marginals,
      setdiff(variables, correlated_variables)
    )
    n_df_subjects <- get_df_n_subjects(original_marginals[[df]], n_subjects)
    non_correlated_marginals$summary$n_subjects <- n_df_subjects
    .sim_df <- synthesise_data_no_cor(non_correlated_marginals)
    id_name <- get_id_name(sub_marginals)
    .sim_df <- sample_subjects(.sim_df, id_name, n_subjects, n_df_subjects)
    # Join the correlated variables by subject
    if (length(df_correlated) > 0) {
      .correlated_df <- correlated_df[
        ,
        c("id", names(df_correlated)),
        drop = FALSE
      ]
      names(.correlated_df) <- c(id_name, correlated_variables)
      .sim_df <- dplyr::left_join(.sim_df, .correlated_df, by = id_name)
      correlated_marginals <- .filter_sub_marginals(
        sub_marginals,
        correlated_variables
      )
      # Add missing values (MAR) and dates for the correlated variables
      .sim_df <- add_missingness(.sim_df, correlated_marginals)
      .sim_df <- .back_transform_dates(correlated_marginals, .sim_df)
    }
    # Order the rows by subject and the columns as the marginals
    .sim_df <- .sim_df[
      order(.sim_df[[id_name]]),
      c(id_name, variables),
      drop = FALSE
    ]
    rownames(.sim_df) <- NULL
    sim_dfs[[df]] <- .sim_df
  }

  # Synthesise the common variables that are not correlated
  correlated_common <- unname(vapply(
    Filter(function(item) item$common, correlated),
    `[[`,
    character(1),
    "variable"
  ))
  common_fields <- setdiff(common_fields, correlated_common)
  if (length(common_fields) > 0) {
    sim_dfs <- replace_fields(
      marginals,
      sim_dfs,
      synthesise_filtered_fields(marginals, common_fields)
    )
  }

  return(sim_dfs)
}

# Get the correlated variables, identified by the data frame they belong to,
# common variables are identified by the variable alone and take their
# marginals from the first data frame they are present in.
# Each variable is given an internal name to use when synthesising.
get_correlated_variables <- function(
  marginals,
  correlations,
  common_fields
) {
  correlated <- list()
  for (correlation in correlations) {
    for (var in c("x", "y")) {
      key <- get_correlated_key(marginals, correlation, var, common_fields)
      if (key %in% names(correlated)) {
        next
      }
      variable <- correlation[[var]]
      df <- get_correlation_df(marginals, correlation, var, common_fields)
      common <- is.na(df)
      if (common) {
        df <- get_first_df(marginals, variable)
      }
      correlated[[key]] <- list(
        key = key,
        df = df,
        variable = variable,
        common = common,
        name = paste0("v", length(correlated) + 1)
      )
    }
  }
  # Name the list by the internal names
  names(correlated) <- vapply(correlated, `[[`, character(1), "name")
  return(correlated)
}

# A key to identify a correlated variable, the data frame and variable
# or the variable alone for common variables.
get_correlated_key <- function(
  marginals,
  correlation,
  var,
  common_fields
) {
  df <- get_correlation_df(marginals, correlation, var, common_fields)
  if (is.na(df)) {
    return(correlation[[var]])
  }
  return(paste(df, correlation[[var]], sep = "."))
}

# Build marginals for the correlated variables using the internal names,
# categories are given internal names to ensure the dummy variables
# are valid variable names.
get_correlated_marginals <- function(
  marginals,
  correlated,
  n_subjects
) {
  correlated_marginals <- list(
    categorical_variables = list(),
    binary_variables = list(),
    continuous_variables = list()
  )
  for (item in correlated) {
    sub_marginals <- marginals[[item$df]]
    if (item$variable %in% names(sub_marginals$categorical_variables)) {
      counts <- sub_marginals$categorical_variables[[item$variable]]
      # Rescale the counts from the number of rows to the number of subjects
      counts <- counts / sub_marginals$summary$n_row * n_subjects
      names(counts) <- paste0("c", seq_along(counts))
      correlated_marginals$categorical_variables[[item$name]] <- counts
    } else if (item$variable %in% names(sub_marginals$binary_variables)) {
      correlated_marginals$binary_variables[[item$name]] <-
        sub_marginals$binary_variables[[item$variable]]
    } else {
      correlated_marginals$continuous_variables[[item$name]] <-
        sub_marginals$continuous_variables[[item$variable]]
    }
  }
  correlated_marginals$summary <- data.frame(
    n_row = n_subjects,
    n_col = length(correlated),
    variables = paste(names(correlated), collapse = ", "),
    subject_identifier = "",
    n_subjects = n_subjects
  )
  return(correlated_marginals)
}

# Convert the correlations to use the internal variable and category names
encode_correlations <- function(
  marginals,
  correlations,
  correlated,
  common_fields
) {
  keys <- vapply(correlated, `[[`, character(1), "key")
  lapply(correlations, function(.correlation) {
    args <- list(rho = .correlation$rho)
    for (var in c("x", "y")) {
      key <- get_correlated_key(marginals, .correlation, var, common_fields)
      item <- correlated[[which(keys == key)]]
      args[[var]] <- item$name
      factor_var <- paste0("factor_name.", var)
      if (factor_var %in% names(.correlation)) {
        categories <- names(
          marginals[[item$df]]$categorical_variables[[item$variable]]
        )
        args[[factor_var]] <- paste0(
          "c",
          match(.correlation[[factor_var]], categories)
        )
      }
    }
    do.call(correlation, args)
  })
}

# Convert the internal category names back to the original categories
decode_correlated_df <- function(
  marginals,
  correlated_df,
  correlated
) {
  for (item in correlated) {
    categories <- names(
      marginals[[item$df]]$categorical_variables[[item$variable]]
    )
    if (length(categories) > 0) {
      correlated_df[[item$name]] <- categories[
        match(correlated_df[[item$name]], paste0("c", seq_along(categories)))
      ]
    }
  }
  return(correlated_df)
}

get_overall_n_subjects <- function(
  marginals
) {
  if ("n_subjects" %in% names(marginals$overall_summary)) {
    return(marginals$overall_summary$n_subjects)
  }
  return(marginals[[get_df_names_or_key(marginals)[1]]]$summary$n_row)
}

get_sub_n_subjects <- function(
  sub_marginals
) {
  if ("n_subjects" %in% names(sub_marginals$summary)) {
    return(sub_marginals$summary$n_subjects)
  }
  return(sub_marginals$summary$n_row)
}

# The number of subjects of a data frame, which can not be more than
# the overall number of subjects
get_df_n_subjects <- function(
  sub_marginals,
  n_subjects
) {
  n_df_subjects <- sub_marginals$summary$n_subjects
  if (is.null(n_df_subjects) || n_df_subjects > n_subjects) {
    return(n_subjects)
  }
  return(n_df_subjects)
}

# Sample the subjects of a data frame from all the subjects, so the
# subjects are shared between data frames, ordering the rows by subject
sample_subjects <- function(
  sim_df,
  id_name,
  n_subjects,
  n_df_subjects
) {
  subject_ids <- sample(n_subjects, n_df_subjects)
  sim_df[[id_name]] <- subject_ids[sim_df[[id_name]]]
  sim_df <- sim_df[order(sim_df[[id_name]]), ]
  rownames(sim_df) <- NULL
  return(sim_df)
}

# The name of the id column, the subject identifier if there is one
get_id_name <- function(
  sub_marginals
) {
  if (
    "subject_identifier" %in% names(sub_marginals$summary) &&
      sub_marginals$summary$subject_identifier != ""
  ) {
    return(sub_marginals$summary$subject_identifier)
  }
  return("id")
}

# Convert the internal category names (c1, c2, ...) back to the original
# categories, internal names are used as simstudy removes whitespace
decode_categories <- function(
  sim_df,
  categorical_summary
) {
  for (.column in names(categorical_summary)) {
    categories <- names(categorical_summary[[.column]])
    sim_df[[.column]] <- categories[
      match(sim_df[[.column]], paste0("c", seq_along(categories)))
    ]
  }
  return(sim_df)
}

# Get the correlated categories of each variable, from the factor names
get_correlated_categories <- function(
  correlations
) {
  correlated_categories <- list()
  for (.correlation in correlations) {
    for (var in c("x", "y")) {
      factor_var <- paste0("factor_name.", var)
      if (factor_var %in% names(.correlation)) {
        variable <- .correlation[[var]]
        correlated_categories[[variable]] <- unique(c(
          correlated_categories[[variable]],
          .correlation[[factor_var]]
        ))
      }
    }
  }
  return(correlated_categories)
}

# Back transform the normal continuous variables to their original
# distributions using the quantiles, rounded to the original decimal places
back_transform_continuous <- function(
  sim_df,
  sub_marginals
) {
  for (variable_name in names(sub_marginals$continuous_variables)) {
    variable <- sub_marginals$continuous_variables[[variable_name]]
    sim_df[[variable_name]] <-
      approx(
        variable$quantiles$tform_q,
        variable$quantiles$orig_q,
        xout = sim_df[[variable_name]], rule = 2, ties = "ordered"
      )$y
    # Round to original decimal places
    sim_df[[variable_name]] <- round(
      sim_df[[variable_name]],
      variable$summary$max_dp
    )
  }
  return(sim_df)
}

synthesise_filtered_fields <- function(
  marginals,
  fields
) {
  filtered_marginals <- .filter_marginals(
    marginals,
    fields,
    TRUE
  )
  common_dfs <- synthesise_data_multi_no_cor(filtered_marginals)

  n_subjects <- marginals$overall_summary$n_subjects

  cols <- list()
  for (col in fields) {
    for (df in common_dfs) {
      if (!col %in% names(df)) {
        next
      }
      if (col %in% names(cols)) {
        if (nrow(df) > length(cols[[col]])) {
          cols[[col]] <- df[[col]]
        }
      } else {
        cols[[col]] <- df[[col]]
      }
    }
  }

  for (col in names(cols)) {
    cols[[col]] <- sample(cols[[col]], n_subjects, replace = TRUE)
  }
  df <- as.data.frame(do.call(cbind, cols))
  df[[filtered_marginals$overall_summary$subject_identifier]] <-
    seq_len(nrow(df))
  df
}

synthesise_common_fields <- function(
  marginals
) {
  common_fields <- .get_common_fields(marginals)
  synthesise_filtered_fields(marginals, common_fields)
}

replace_fields <- function(
  marginals,
  sim_dfs,
  replacement_df
) {
  subject_identifier <- marginals$overall_summary$subject_identifier
  for (i in seq_along(sim_dfs)) {
    df <- sim_dfs[[i]]
    if (length(intersect(names(df), names(replacement_df))) > 1) {
      common_cols <- intersect(names(df), names(replacement_df))
      tmp_common_df <- replacement_df[, common_cols]
      tmp_df <- dplyr::select(
        df,
        -dplyr::all_of(common_cols[common_cols != subject_identifier])
      )
      tmp_df <- dplyr::left_join(
        tmp_df,
        tmp_common_df,
        by = subject_identifier
      )
      tmp_df <- tmp_df[, names(df)]
      sim_dfs[[i]] <- tmp_df
    }
  }
  sim_dfs
}

replace_common_fields <- function(
  marginals,
  sim_dfs
){
  replacement_df <- synthesise_common_fields(marginals)
  replace_fields(marginals, sim_dfs, replacement_df)
}

add_n_subjects <- function(
  marginals
) {
  if (!"n_subjects" %in% names(marginals$overall_summary)) {
    return(marginals)
  }
  df_names <- get_df_names_or_key(marginals)
  for (df in df_names) {
    if (df == "overall_summary") {
      next
    }
    marginals[[df]]$summary$n_subjects <- marginals$overall_summary$n_subjects
  }
  marginals
}

get_data_def <- function(
  sub_marginals,
  use_correlations = FALSE
) {
  # Predefine dataDefinition
  data_def <- NULL

  # If there are categorical variables
  if ("categorical_variables" %in% names(sub_marginals)) {
    # Define categorical variables dependant on correlations
    if (use_correlations) {
      data_def <- define_categorical_binary(
        sub_marginals$categorical_variables,
        sub_marginals$summary$n_row,
        data_def
      )
    } else {
      data_def <- define_categorical(
        sub_marginals$categorical_variables,
        sub_marginals$summary$n_row,
        data_def
      )
    }
  }

  # If there are binary variables
  if ("binary_variables" %in% names(sub_marginals)) {
    # Define binary variables
    data_def <- define_binary(
      sub_marginals$binary_variables,
      data_def
    )
  }

  # If there are continuous variables
  if ("continuous_variables" %in% names(sub_marginals)) {
    # Define continuous variables
    data_def <- define_continuous(
      sub_marginals$continuous_variables,
      data_def
    )
  }
  return(data_def)
}

define_categorical <- function(
  categorical_summary,
  n_row,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(categorical_summary)){
    # Forward declare the categories
    .labs <- c()
    # Forward declare the probabilities
    .probs <- c()
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Add the category to the categories, using internal names as
      # simstudy removes whitespace from categories (see decode_categories)
      .labs <- c(.labs, paste0("c", length(.labs) + 1))
      # Add the probability to the probabilities
      .probs <- c(.probs, (categorical_summary[[.column]][[.cat]] / n_row))
    }
    # Need at least two categories
    if (length(.labs) == 1) {
      .labs <- c(.labs, "NA")
      .probs <- c(.probs, 0)
    }
    # Add the definition to the definitions
    # Specifying a categorical distribution
    # Giving the categories and probabilities of each
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
      dist = "categorical",
      formula = paste0(.probs, collapse = ";"),
      variance = paste0(.labs, collapse = ";")
    )
  }
  # Return the definitions
  return(.data_def)
}

define_categorical_binary <- function(
  categorical_summary,
  n_row,
  data_def = NULL
) {
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(categorical_summary)){
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Add the category to the definitions as dummy (binary) variables
      .data_def <- simstudy::defData(
        .data_def,
        varname = paste(.column, .cat, sep = "_"),
        dist = "binary",
        formula = (categorical_summary[[.column]][[.cat]] / n_row)
      )
    }
  }
  # Return the definitions
  return(.data_def)
}

define_binary <- function(
  binary_summary,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(binary_summary)){
    # Add the variable to the definitions
    # Specifying a binary distribution
    # Using mean as the probability
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
      dist = "binary",
      formula = binary_summary[[.column]][["mean"]]
    )
  }
  return(.data_def)
}

define_continuous <- function(
  continuous_summary,
  data_def = NULL
) {
  # Explicitly copy the definitions
  .data_def <- data_def
  # Loop through the variables
  for (.column in names(continuous_summary)) {
    # Add the variable to the definition
    # Specifying a normal distribution
    # Using mean and standard deviation
    .data_def <- simstudy::defData(
      .data_def,
      varname = .column,
      dist = "normal",
      formula = 0,
      variance = 1
    )
  }
  return(.data_def)
}

add_missingness <- function(
  simulated_data,
  sub_marginals
) {
  # Remove purposely added 'missing' factors
  .df <- simulated_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character), ~gsub("missing", "", .x)
      )
    )
  # Loop through the binary variables
  for (binary_variable in names(sub_marginals$binary_variables)) {
    # Get the number of NAs
    .variable_missingness <-
      sub_marginals$binary_variables[[binary_variable]][["missing"]]
    # Only add NAs if there are any to add
    if (.variable_missingness > 0) {
      # Select a random set of rows given the number of NAs and replace
      # the variable of those rows with NAs
      .df[sample(nrow(.df), .variable_missingness), ][[binary_variable]] <- NA
    }
  }
  # Loop through the continuous variables
  for (continuous_variable in names(sub_marginals$continuous_variables)) {
    # Only add NAs if there are any to add
    .variable_missingness <-
      sub_marginals$continuous_variables[[continuous_variable]][["summary"]][["missing"]] # nolint line_length
    if (.variable_missingness > 0) {
      # Select a random set of rows given the number of NAs and replace
      # the variable of those rows with NAs
      .df[sample(nrow(.df), .variable_missingness), ][[continuous_variable]] <- NA # nolint line_length
    }
  }
  .df <- .replace_nas(
    .df
  )
  # Return the new data frame
  return(.df)
}

#' @title Export an empty correlation matrix (removed)
#' @description This function has been removed. Correlations should now be
#' specified using the \code{\link{correlation}} function.
#' @param ... Ignored, retained for backwards compatibility.
#' @return No return value, always throws an error.
#' @details Previously this function exported an empty correlation matrix
#' as a csv file. Correlations are now supplied directly to
#' \code{\link{synthesise_data}} as a list of objects created with the
#' \code{\link{correlation}} function.
#' @examples
#'  try(export_empty_cor_matrix())
#' @seealso
#'  \code{\link{correlation}}
#' @rdname export_empty_cor_matrix
#' @export
#' @importFrom simstudy genCorMat
#' @importFrom utils write.csv
export_empty_cor_matrix <- function(...) {
  .Defunct(
    new = "correlation",
    package = "RESIDE",
    msg = paste0(
      "export_empty_cor_matrix() has been removed. ",
      "Use the correlation() function to specify correlations instead, ",
      "e.g. correlation(\"age\", \"bmi\", 0.5)."
    )
  )
}

#' @title Import a correlation matrix (removed)
#' @description This function has been removed. Correlations should now be
#' specified using the \code{\link{correlation}} function.
#' @param ... Ignored, retained for backwards compatibility.
#' @return No return value, always throws an error.
#' @details Previously this function imported a correlation matrix from a
#' csv file. Correlations are now supplied directly to
#' \code{\link{synthesise_data}} as a list of objects created with the
#' \code{\link{correlation}} function.
#' @examples
#'  try(import_cor_matrix())
#' @seealso
#'  \code{\link{correlation}}
#' @rdname import_cor_matrix
#' @export
#' @importFrom utils read.csv
#' @importFrom tibble column_to_rownames
import_cor_matrix <- function(...) {
  .Defunct(
    new = "correlation",
    package = "RESIDE",
    msg = paste0(
      "import_cor_matrix() has been removed. ",
      "Use the correlation() function to specify correlations instead, ",
      "e.g. correlation(\"age\", \"bmi\", 0.5)."
    )
  )
}

generate_correlation_matrix <- function(
  marginals,
  correlations
) {
  # Get the data definition, to get all the variable names,
  # including the dummy variables
  data_def <- get_data_def(marginals, TRUE)
  # Generate an empty correlation matrix with a default correlation of 0
  cor_matrix <- simstudy::genCorMat(nrow(data_def), rho = 0)
  # Set the column names
  colnames(cor_matrix) <- data_def[["varname"]]
  # Set the row names
  rownames(cor_matrix) <- data_def[["varname"]]
  # Loop through the correlations
  for (cor in correlations) {
    names <- c()
    for (var in c("x", "y")) {
      name <- cor[[var]]
      factor_var <- paste0("factor_name.", var)
      # Categorical variables are correlated using the dummy variable
      # of the specified category
      if (factor_var %in% names(cor)) {
        name <- paste(name, cor[[factor_var]], sep = "_")
      } else if (name %in% names(marginals$categorical_variables)) {
        stop(
          paste(
            "Correlation variable",
            name,
            "is categorical and must have a factor name specified using",
            factor_var
          )
        )
      }
      # Check that the variables are in the correlation matrix
      if (!(name %in% colnames(cor_matrix))) {
        stop(paste0("Variable ", name, " not found in correlation matrix."))
      }
      names <- c(names, name)
    }
    # Add the correlation to the matrix
    cor_matrix[names[1], names[2]] <- cor$rho
    cor_matrix[names[2], names[1]] <- cor$rho
  }
  return(cor_matrix)
}

# With correlations it is possible that the dummy variables for a single
# category do not add up to one, we will fix that here
# Only the dummy variables of the correlated categories are used,
# the remaining categories are sampled using their probabilities,
# which maintains the marginal distribution when a single category
# of a variable is correlated.
fix_factors <- function(
  simulated_data,
  sub_marginals,
  correlated_categories = NULL
) {
  # Get the categorical summary from the marginals
  categorical_summary <- sub_marginals$categorical_variables
  # Extract the number for rows from the marginals
  n_row <- sub_marginals$summary$n_row
  # Loop through the columns
  for (.column in names(categorical_summary)){
    # Forward declare category names
    cat_names <- c()
    # Forward declare probabilities
    probs <- c()
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Get the dummy variable name for the category
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the dummy variable name to the category names
      cat_names <- c(cat_names, cat_name)
      # Calculate the probability for the category
      # and add it to the probabilities
      probs <- c(probs, (categorical_summary[[.column]][[.cat]] / n_row))
    }
    # Subset only the dummy variables for the current category
    variable_df <- simulated_data[, cat_names, drop = FALSE]

    # Ignore the dummy variables of the categories that are not correlated
    is_correlated <- rep(TRUE, length(cat_names))
    if (!is.null(correlated_categories)) {
      is_correlated <- names(categorical_summary[[.column]]) %in%
        correlated_categories[[.column]]
    }
    zero_probs <- probs
    if (!all(is_correlated)) {
      variable_df[, !is_correlated] <- 0
      zero_probs[is_correlated] <- 0
    }

    # Replace any row where the dummy variables total 0
    # (indicating no category was selected)
    # sampling from the categories that are not correlated
    zero_rows <- rowSums(variable_df) == 0
    if (any(zero_rows)) {
      variable_df[zero_rows, ] <-
        replace_zero_rows(variable_df[zero_rows, , drop = FALSE], zero_probs)
    }

    # Replace any row where the dummy variables total more than 1
    # (indicating more than one category was selected)
    multiple_rows <- rowSums(variable_df) > 1
    if (any(multiple_rows)) {
      variable_df[multiple_rows, ] <-
        replace_one_rows(variable_df[multiple_rows, , drop = FALSE], probs)
    }
    # Replace the columns with the corrected columns
    simulated_data[, cat_names] <- variable_df
  }
  # Return the corrected data frame
  return(simulated_data)
}

# Function to replace rows for dummy categorical
# variables where the dummy columns add up to zero
# indicating that a category has not been selected
# in which case a category is sampled using the probabilities
replace_zero_rows <- function(rows, probs) {
  selected <- sample(
    seq_along(probs),
    nrow(rows),
    replace = TRUE,
    prob = probs
  )
  return(one_hot_rows(rows, selected))
}

# Function to replace rows for dummy categorical
# variables where the dummy columns add up more than one
# indicating that more than one category has been selected
# in which case one of the categories selected is sampled
# using the probabilities.
replace_one_rows <- function(rows, probs) {
  selected <- apply(as.matrix(rows), 1, function(row) {
    options <- which(row == 1)
    if (length(options) == 1) {
      return(options)
    }
    sample(options, 1, prob = probs[options])
  })
  return(one_hot_rows(rows, selected))
}

# Set the selected column of each row to 1 and the other columns to 0
one_hot_rows <- function(rows, selected) {
  rtn_rows <- matrix(0, nrow = nrow(rows), ncol = ncol(rows))
  rtn_rows[cbind(seq_len(nrow(rows)), selected)] <- 1
  rtn_rows <- as.data.frame(rtn_rows)
  names(rtn_rows) <- names(rows)
  return(rtn_rows)
}

# Convert factors back from dummy categories
restore_factors <- function(
  simulated_data,
  categorical_summary
) {
  # Forward declare the category names
  category_names <- c()
  # loop through the categorical variables
  for (.column in names(categorical_summary)){
    # Add the variable as a column to the data frame
    simulated_data[[.column]] <- ""
    # Loop through the categories
    for (.cat in names(categorical_summary[[.column]])) {
      # Define the dummy category name
      cat_name <- paste(.column, .cat, sep = "_")
      # Add the category to the list of category names
      category_names <- c(category_names, cat_name)
      # If the indicator (1) for the dummy variable set the category
      # to the current category
      simulated_data[[.column]][simulated_data[[cat_name]] == 1] <- .cat
    }
  }
  # Remove dummy variables
  simulated_data[, category_names] <- list(NULL)
  # Return the modified data
  return(simulated_data)
}

reindex_df <- function(
  sub_marginals,
  sim_df
) {
  n_row <- n_ids <- sub_marginals$summary$n_row
  if ("n_subjects" %in% names(sub_marginals$summary)) {
    n_ids <- sub_marginals$summary$n_subjects
  }
  if (n_ids != nrow(sim_df)) {
    id_len <- ceiling(nrow(sim_df) / n_ids)
    # Repeat the ids to the length needed
    ids <- rep(seq_len(n_ids), id_len)
    # The length may be more than the number of rows in df
    # so we sample the ids to match the number of rows in df
    ids <- sample(ids, n_row)
    ids <- ids[order(ids)]
    sim_df$id <- ids
  } else {
    sim_df$id <- seq_len(n_row)
  }
  if (sub_marginals$summary$subject_identifier != "") {
    names(sim_df)[names(sim_df) == "id"] <-
      sub_marginals$summary$subject_identifier
  }
  sim_df
}
