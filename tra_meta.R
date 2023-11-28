tra_meta <- function(input, cols_group = NULL, index_output = "metadata/group_indices_df.csv", vars_output = "metadata/group_vars_df", firstrows_output = "metadata/first_rows_df.csv", to_csv = FALSE ) {

  data_tibble <- tibble::as_tibble(input)

  if(is.null(cols_group)){stop("Error :// Set cols_group to determine the columns that shall be grouped.")}
  else {

  grouped_meta <- data_tibble %>%
   dplyr::group_by(across(all_of(cols_group)))

  # Create a data frame of group indices
  group_indices_df <- grouped_meta %>%
   dplyr::mutate(group_index = cur_group_id())

  # Create a data frame of group variables
  group_vars_df <- dplyr::tibble(group_vars = grouped_meta %>% group_vars())

  # Get the first rows of each group
  first_rows_df <- grouped_meta %>%
    dplyr::slice_head(n = 1)
  }

  if(to_csv == TRUE) {
  # Write the data frames to CSV files
  readr::write_csv(group_indices_df, index_output)
  readr::write_csv(group_vars_df, vars_output)
  readr::write_csv(first_rows_df, firstrows_output)
  }

  return(list(group_indices_df = group_indices_df, group_vars_df = group_vars_df, first_rows_df = first_rows_df))
}
