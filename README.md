library(devtools)

install_github("davidmartini90/pam")

library(pam)

example_data


example_data %>%
  add_acquisition(
    start_of_night = "20:40",
    end_of_night = "05:00",
    date_format = "mdy"
  ) %>%
  add_reference() %>%
  plot_reference_parameters()


example_data %>%
  add_acquisition(
    start_of_night = "20:40",
    end_of_night = "05:00",
    date_format = "mdy"
  ) %>%
  add_reference() %>%
  add_pam_parameters() %>% glimpse()
