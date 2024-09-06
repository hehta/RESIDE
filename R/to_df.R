##################################################################
##  Functions to convert marginal distributions to data frames  ##
##################################################################
binary_to_df <- function(
  x
) {
  # Convert the binary variables list to a data frame
  # and then convert the row names to a column
  binary_df <- as.data.frame(do.call(rbind, x)) %>%
    tibble::rownames_to_column(var = "variable")
  # Unlist the variables
  binary_df$mean <- unlist(unname(binary_df$mean))
  binary_df$missing <- unlist(unname(binary_df$missing))
  # Return the data frame
  return(binary_df)
}

quantiles_to_df <- function(
  x
) {
  # Return an empty dataframe if there are no variables
  if (length(x) < 1) {
    return(data.frame())
  }
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
  # Return an empty dataframe if there are no variables
  if (length(x) < 1) {
    return(data.frame())
  }
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