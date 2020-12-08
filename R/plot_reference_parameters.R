#' Plot FmR and FoR
#'
#' plot_reference_parameters() plots FmR and FoR and highlights time when PSII was maximum
#'
#' @param input_data Data containing the output of the MONITORING-PAM instrument. A dataframe or a tibble. The data needs to contain the column added by the funtion add_acquisition() and add_reference()
#'
#' @return
#' a ggplot object
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plot_reference_parameters<-function(input_data= NULL){

  if(!(requireNamespace("dplyr"))){install.packages("dplyr")}
  if(!(requireNamespace("tidyr"))){install.packages("tidyr")}
  if(!(requireNamespace("ggplot2"))){install.packages("ggplot2")}

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("ggplot2")

if(is.null(input_data)){stop("no data provided in input_data argument")}
if(!("date_time" %in% names(input_data))){stop("column 'date_time' not found. Use the function 'add_acquisition' and 'add_reference' before 'plot_reference_parameters")}
if(!("FmR" %in% names(input_data))){stop("column 'FmR' not found. Use the function 'add_acquisition' and 'add_reference' before 'plot_reference_parameters")}
if(!("FoR" %in% names(input_data))){stop("column 'FoR' not found. Use the function 'add_acquisition' and 'add_reference' before 'plot_reference_parameters")}


input_data %>%
  dplyr::group_by(.data$Head) %>%
  dplyr::mutate(
    date_time= .data$date_time[which.max(.data$PSII)]
  ) %>%
  dplyr::summarise(
    date_time= unique(.data$date_time),
    FmR= unique(.data$FmR),
    FoR= unique(.data$FoR)
  )->table_reference

input_data$date_time %>% dplyr::first()->first_date

table_reference %>% tidyr::pivot_longer(c(.data$FmR, .data$FoR))->table_reference

input_data %>%
  ggplot2::ggplot(aes(x=.data$date_time, y=.data$Fm, col=.data$Head %>% as.factor)) +
  scale_color_discrete(name= "Head") +
  geom_point(data= table_reference, size = 3, aes(x=.data$date_time, y=.data$value)) +
  geom_segment(
    data= table_reference,
    aes(x=first_date, xend=.data$date_time, y=.data$value, yend= .data$value, col=.data$Head %>% as.factor)
  ) +
  geom_segment(
    data= table_reference,
    aes(x=.data$date_time, xend=.data$date_time, y=0, yend= .data$value, col=.data$Head %>% as.factor)
  ) + xlab("Date Time") +
  facet_wrap(vars(.data$name)) +
  theme_bw() +
  theme(legend.position = "top")-> output_plot

return(output_plot)

}
#' @examples
#'example_data %>%
#'  add_acquisition(
#'    start_of_night = "20:40",
#'    end_of_night = "05:00",
#'    date_format = "mdy"
#'  ) %>%
#'  add_reference() %>%
#'  plot_reference_parameters()
#'
