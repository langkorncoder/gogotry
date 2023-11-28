tra_select_means <- function(input, output_file = "means_bundesland/means_bundesland.csv", selects = NULL, date_format = c("dmy", "mdy", "ymd", "dBy", "mdBy", "ymdHMS"), date_col = "datum", to_csv = FALSE) {

  means_select <- tibble::as_tibble(input)

  if (!is.null(selects)) {
    means_select <- means_select %>%
      dplyr::select(all_of(selects)) %>%
      dplyr::mutate(year = lubridate::year(parse_date_time(!!sym(date_col), orders = date_format)),
                    month = lubridate::month(parse_date_time(!!sym(date_col), orders = date_format)))

    means_structure <- structured(means_select)

    if (to_csv == TRUE) {
      readr::write_csv(means_select, file = output_file)
    }

    df <- return(list(means_select = means_select, means_structure = means_structure))
  }
}

