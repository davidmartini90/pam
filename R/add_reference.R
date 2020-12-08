#' Add reference parameters (FmR, FoR and maximum night Fm)
#'
#' add_reference() Adds reference parameters (FmR, FoR and maximum night Fm)
#'
#' @param input_data Data containing the output of the MONITORING-PAM insrument. A dataframe or a tibble. The data needs to contain the column added by the funtion add_acquisition()
#' @param acquisition_starts_at character string specifying when the acquisistion starts. Can be either "night" or "morning"
#'
#' @return
#' A tibble with the following additional columns:
#'
#' * PSIImax: maximum PSII value (for each head)
#'
#' * FmR: Reference maximal fluorescence; Fm value when PSII is maximum (for each head). Eq. 6 of Porcar-Castell, A. (2011). Numeric
#'
#' * FoR: Reference minimal fluorescence;  F value when PSII is maximum (for each head). Eq. 5 of Porcar-Castell, A. (2011). Numeric
#'
#' * TimePSIImax: Time and date when PSII is maximum (for each head). Time
#'
#' * PSIImax_eachnight: Maximum PSII value at night (for each head and each acquisition). Numeric
#'
#' * Fo_nightmin: Night minimal fluorescence; F at night when PSII is maximum (for each head and each acquisition). Eq. 7 of Porcar-Castell, A. (2011). Numeric
#'
#' * Fm_nightmax: Night maximal fluorescence = F at night when PSII is maximum (for each head and each acquisition). Eq. 9 of Porcar-Castell, A. (2011). Numeric
#'
#' * TimePSIImax_eachnight: Time when the PSII is maximum (for each head and each acquisition). Time
#'
#' @references
#' Porcar-Castell, A. (2011). A high-resolution portrait of the annual dynamics of photochemical and non-photochemical quenching in needles of Pinus sylvestris. Physiologia Plantarum, 143(2), 139-153.
#' @import dplyr
#' @importFrom rlang .data
#' @export
add_reference<-function(input_data = NULL, acquisition_starts_at = "night"){

  if(!(requireNamespace("dplyr"))){install.packages("dplyr")}

  loadNamespace("dplyr")

if(is.null(input_data)){stop("no data provided in input_data argument")}
if(!("acquisition_night_start" %in% names(input_data))){stop("column 'acquisition_night_start' not found. Use the function 'add_acquisition' before 'add_reference")}
if(!("acquisition_morning_start" %in% names(input_data))){stop("column 'acquisition_morning_start' not found. Use the function 'add_acquisition' before 'add_reference")}



  #fmr and for

  input_data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Head) %>%
    dplyr::mutate(
      PSIImax = max(.data$PSII),
      FmR = .data$Fm[which.max(.data$PSII)], # eq 6
      FoR = .data$F[which.max(.data$PSII)],  # eq 5
      TimePSIImax  = .data$Time[which.max(.data$PSII)]
  ) %>%
  dplyr::ungroup()->input_data


#Fo_min and Fm_max- Night minimal and maximal fluorescence (Table 1, Eqn no.7 and 9); estimated every night

  if(acquisition_starts_at == "night"){

  input_data %>%
    dplyr::group_by(.data$Head, .data$acquisition_night_start) %>%
    dplyr::mutate(
      PSIImax_eachnight = max(.data$PSII),
      Fo_nightmin = .data$F[which.max(.data$PSII)], #eq 7
      Fm_nightmax = .data$Fm[which.max(.data$PSII)], #eq 9
      TimePSIImax_eachnight  = .data$Time[which.max(.data$PSII)]
  ) %>% dplyr::ungroup()->output_data

  } else if(acquisition_starts_at == "morning"){

    input_data %>%
      dplyr::group_by(.data$Head, .data$acquisition_morning_start) %>%
      dplyr::mutate(
        PSIImax_eachnight = max(.data$PSII),
        Fo_nightmin = .data$F[which.max(.data$PSII)], #eq 7
        Fm_nightmax = .data$Fm[which.max(.data$PSII)], #eq 9
        TimePSIImax_eachnight  = .data$Time[which.max(.data$PSII)]
      ) %>% dplyr::ungroup()->output_data

  } else( stop("the argument acquisition_starts_at can be either 'night' or 'morning'"))


  return(output_data)

}
#' @examples
#' library(dplyr)
#'
#'example_data %>%
#'  add_acquisition(
#'    start_of_night = "20:40",
#'    end_of_night = "05:00",
#'    date_format = "mdy"
#'  ) %>%
#'  add_reference()
#'
