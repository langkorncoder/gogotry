tra_process <- function(input_data, output_file = "meandata/means.csv",  cols_drop = NULL, cols_rename = NULL, cols_group = NULL, to_csv = FALSE) {

   # Convert the data frame to a tibble
  data <- tibble::as_tibble(input_data)

  # Check the structure
  dplyr::glimpse(data)


  # If needed, delete a specific column
  if (!is.null(cols_drop)) {
    data <- data %>%
      dplyr::select(-all_of(cols_drop))
  }

    # If needed, rename specific columns
  if (!is.null(cols_rename)) {
    data <- dplyr::rename(data, !!cols_rename)
    }


   # Group the data frame by specified columns and calculate the mean of numeric columns
  if (!is.null(cols_group)) {
    means_data <- data %>%
      dplyr::group_by(across(all_of(cols_group))) %>%
      dplyr::add_tally() %>%
      dplyr::summarize(across(where(is.numeric), mean, na.rm = TRUE),
                       across(where(is.character), ~first(.)),.groups="drop")
    dplyr::glimpse(means_data)

  }

      else if (is.null(cols_group)) {print("Missing group parameter 'cols_group'. Set grouping variables from which the rows shall get meaned, otherwise it is not possible to create any summarized rows.")}


    if(to_csv == TRUE) {
    # Write the processed data frame to a CSV file
    readr::write_csv(means_data, file = output_file)
  }

  dplyr::glimpse(means_data)

  df <- return(means_data)
}
