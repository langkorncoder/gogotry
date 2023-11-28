
read_and_clean_data <- function(file_path, seperate = ";", decim = ",", tibble = TRUE) {
  # Read the CSV file with specified separator and decimal point
  data <- read.csv(file_path, sep = seperate, dec = decim)
                          #Clean the column names using the clean_names() function from the janitor package
  cleaned_data <- data %>%
    janitor::clean_names() %>%
    janitor::remove_empty() %>%
    janitor::remove_constant()

  if (tibble) {
    cleaned_data <- tibble::as_tibble(cleaned_data)
  }
  # Return the cleaned data
  cleaned_data
}


