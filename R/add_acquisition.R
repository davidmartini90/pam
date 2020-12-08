#' Add acquisition number
#'
#' add_acquisition() adds the acquisition number. An acquisition last 24 hours, but it can start at the start of the night or at the start of the morning. The argument start_of_night and start_of_day define the start and end of the night period.
#'
#' @param input_data Data containing the output of the MONITORING-PAM insrument. A dataframe or a tibble
#' @param start_of_night A character string that specifies the start of the night period. The time has to be in HH:MM format, for example "20:40"
#' @param end_of_night A character string that specifies the end of the night period. The time has to be in HH:MM format, for example "05:00"
#' @param date_format A character string that specifies the format of the Date column: either "dmy", for day/month/year, or "mdy", for month/day/year
#'
#' @return
#' A tibble with the following additional columns:
#'
#' * date_time: column containing the date and time in format year-month-day hour:minute:second. The column has dttm format
#'
#' * DOY: column containing the day of the year (DOY). Numeric
#'
#' * DAYTIME: column that specifies if its daytime ("day"), or nighttime ("night"). Character
#'
#' * acquisition_night_start: column that containes the acquisition number. Acquisition starts at the start of the night. Numeric
#'
#' * acquisition_morning_start: column that containes the acquisition number. Acquisition starts at the start of the end of the night. Numeric
#'
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @import utils
#' @importFrom rlang .data
#' @export
add_acquisition<-function(input_data = NULL, start_of_night = "20:40", end_of_night = "05:00", date_format = "dmy"){

if(!(requireNamespace("dplyr"))){install.packages("dplyr")}
if(!(requireNamespace("lubridate"))){install.packages("lubridate")}
if(!(requireNamespace("tibble"))){install.packages("tibble")}

  loadNamespace("dplyr")
  loadNamespace("lubridate")
  loadNamespace("tibble")

if(is.null(input_data)){stop("no data provided in input_data argument")}
if(!("Date" %in% names(input_data))){stop("Column 'Date' is not present")}
if(!("Time" %in% names(input_data))){stop("Column 'Time' is not present")}

input_data %>% tibble::as_tibble() %>% dplyr::ungroup()->input_data

if(date_format == "mdy"){


    dplyr::mutate(input_data,
    date_time = paste(lubridate::mdy(.data$Date), .data$Time, sep=" ") %>% lubridate::ymd_hms()
      ) ->input_data

}

if(date_format == "dmy"){

  dplyr::mutate(input_data,
    date_time = paste(lubridate::dmy(.data$Date), .data$Time, sep=" ") %>% lubridate::ymd_hms()
      ) ->input_data

}


dplyr::mutate(input_data,
    DOY = lubridate::yday(.data$date_time)
  )->input_data

input_data$DOY %>% dplyr::first()->first_doy

dplyr::mutate(input_data,
    DAYTIME = ifelse(
      lubridate::hms(.data$Time) >= lubridate::hm(start_of_night) | lubridate::hms(.data$Time) <= lubridate::hm(end_of_night),
      "night", "day")
  ) %>%
  dplyr::group_by(.data$DOY) %>%
  dplyr::mutate(
    acquisition_night_start = ifelse(
       lubridate::hms(.data$Time) >= lubridate::hm(start_of_night),
      .data$DOY - first_doy + 2,
      .data$DOY - first_doy + 1
      ),
    acquisition_morning_start = ifelse(
       lubridate::hms(.data$Time) >= lubridate::hm(end_of_night),
      .data$DOY - first_doy + 2,
      .data$DOY - first_doy + 1
      )
  ) %>% dplyr::ungroup() ->output_data


return(output_data)

}
#' @examples
#' example_data %>%
#' add_acquisition(
#'  start_of_night = "20:40",
#'  end_of_night = "05:00",
#'  date_format = "mdy"
#'  )
#'
