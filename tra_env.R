# tra_env function
# @description This function transforms environmental data from a state in Germany into a standardized format and optionally saves it as a csv file.
# @param input A data frame containing the environmental data from a state.
# @param output_file A string specifying the path and name of the output csv file.
# @param state_name A string specifying the name of the state.
# @param to_csv A logical value indicating whether to save the output as a csv file or not.
# @return A list containing two elements: env_import, which is the transformed data frame, and str_env_import, which is the summary of the data frame.
# @examples
# tra_env(input = env_data, output_file = "import/env_import_Sachsen.csv", state_name = "Sachsen", to_csv = TRUE)
# @export
tra_env <- function(input, output_file = glue::glue("import/env_import_{state_name}.csv"), state_name = "Bundesland", to_csv = FALSE) {

  env = tibble::as_tibble(input)

  # Create new columns containing the name of the state and the env_id which is a paste object of the state, the bdf, the year and the month
  env_import = env %>%
    dplyr::mutate(state = state_name,
                  env_id = sprintf("%s-%s_%04d-%02d_env", state, bdf, year, month)) %>%
    dplyr::select(bdf, year, month, everything())

  # check if the new columns occured by performing skim()
  str_env_import = structured(env_import)
  # if to_csv = TRUE R directly creates a csv file in the specified output_path
  if(to_csv){
    readr::write_csv(env_import, file = output_file)
  }

  return(list(env_import = env_import, str_env_import = str_env_import))
}
