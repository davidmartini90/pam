#' Add various PAM parameters
#'
#'add_pam_parameters adds the following parameters (see details):
#'
#'  Photochemical quenching (PQ)
#'
#'  Maximum PQ (PQmax)
#'
#'  Non-photochemical quenching (NPQ)
#'
#'  Sustained component of NPQ (NPQs)
#'
#'  Reversible component of NPQ (NPQr)
#'
#'  Yield of NPQ (NPQ_yield)
#'
#'  Yield of NPQs (NPQs_yield)
#'
#'  Yield of NPQr (NPQr_yield)
#'
#'  Yield of photochemistry (P_yield)
#'
#'  Yield of fluorescence and basal thermal energy dissipation (FD_yield)
#'
#'  Yield of fluorescence (F_yield)
#'
#'  Relative light saturation of photosynthesis (relative_light_saturation)
#'
#' @param input_data Data containing the output of the MONITORING-PAM instrument. A dataframe or a tibble. The data needs to contain the column added by the funtion add_acquisition() and add_reference()
#'
#' @return
#' A tibble with the following additional columns:
#'
#' * PQ: Photochemical quenching. Eq. 13 of Porcar-Castell, A. (2011). Numeric
#'
#' * PQmax: Maximum PQ. Eq. 12 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQ: Non-photochemical quenching. Eq. 14 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQs: Sustained component of NPQ. Eq. 15 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQr: Reversible component of NPQ. Eq. 16 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQ_yield: Yield of NPQ. Eq. 20 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQs_yield: Yield of NPQs. Eq. 21 of Porcar-Castell, A. (2011). Numeric
#'
#' * NPQr_yield: Yield of NPQr. Eq. 22 of Porcar-Castell, A. (2011). Numeric
#'
#' * P_yield: Yield of photochemistry. Eq. 23 of Porcar-Castell, A. (2011). Numeric
#'
#' * FD_yield: Yield of fluorescence and basal thermal energy dissipation. Atherton, J. et al. (2019). NUmeric
#'
#' * F_yield: Yield of fluorescence. Porcar-Castell, A. et al. (2014). Numeric
#'
#' * relative_light_saturation: Relative light saturation of photosynthesis. Eq. 18 of Van der Tol, C., et al. (2014). NUmeric

#'
#' @references
#'Porcar‐Castell, A. (2011). A high‐resolution portrait of the annual dynamics of photochemical and non‐photochemical quenching in needles of Pinus sylvestris. Physiologia Plantarum, 143(2), 139-153.
#'
#'Atherton, J., Liu, W., & Porcar-Castell, A. (2019). Nocturnal Light Emitting Diode Induced Fluorescence (LEDIF): A new technique to measure the chlorophyll a fluorescence emission spectral distribution of plant canopies in situ. Remote Sensing of Environment, 231, 111137.
#'
#'Porcar-Castell, A., Tyystjärvi, E., Atherton, J., Van der Tol, C., Flexas, J., Pfündel, E. E., ... & Berry, J. A. (2014). Linking chlorophyll a fluorescence to photosynthesis for remote sensing applications: mechanisms and challenges. Journal of experimental botany, 65(15), 4065-4095.
#'
#'Van der Tol, C., Berry, J. A., Campbell, P. K. E., & Rascher, U. (2014). Models of fluorescence and photosynthesis for interpreting measurements of solar‐induced chlorophyll fluorescence. Journal of Geophysical Research: Biogeosciences, 119(12), 2312-2327.
#' @import dplyr
#' @importFrom rlang .data
#' @export
add_pam_parameters<-function(input_data= NULL){

if(!(requireNamespace("dplyr"))){install.packages("dplyr")}

  loadNamespace("dplyr")

if(is.null(input_data)){stop("no data provided in input_data argument")}
if(!("FmR" %in% names(input_data) )){stop("column 'FmR' not found. Use the function 'add_reference' before 'add_pam_parameters")}
if(!("Fm_nightmax" %in% names(input_data))){stop("column 'Fm_nightmax' not found. Use the function 'add_reference' before 'add_pam_parameters")}

input_data %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    PQmax = .data$FmR/.data$FoR -1,  # the maximum PQ capacity, Eqn no.12 (this is KPSII)
    PQ = .data$FmR/.data$F-.data$FmR/.data$Fm,  # the actual PQ capacity, Eqn no.13

    NPQ = .data$FmR/.data$Fm-1,  # the total regulated thermal energy dissipation capacity. Where NPQ is the widely used NPQ parameter, Eqn no.14
    NPQs = .data$FmR/.data$Fm_nightmax-1,  # the sustained component of kNPQ, Eqn no.15
    NPQr = (.data$FmR/.data$Fm)-(.data$FmR/.data$Fm_nightmax),  # the the reversible component of kNPQ, Eqn no.16

    NPQ_yield= (.data$F/.data$Fm)-(.data$F/.data$FmR),  # Yield of total NPQ: eq 20
    NPQs_yield= (.data$F/.data$Fm_nightmax)-(.data$F/.data$FmR),  # Yield of sustained NPQ: eq 21
    NPQr_yield= (.data$F/.data$Fm)-(.data$F/.data$Fm_nightmax),  # Yield of reversible NPQ: eq 22

    P_yield= (1-(.data$F/.data$Fm)),  # Yield of photochemistry eq 23
    FD_yield= .data$F/.data$FmR,  # Yield of fluorescence and basal thermal energy dissipation (typo in the paper from 2011, Fmprime should be FmR according to: (see below)
    F_yield= 0.1 / (1 + .data$PQ + .data$NPQ), #fig 9 porcar castell et al 2014 review
    relative_light_saturation =  1- (.data$PSII / .data$PSIImax_eachnight )

  )->output_data

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
#'  add_reference() %>%
#'  add_pam_parameters()
#'
