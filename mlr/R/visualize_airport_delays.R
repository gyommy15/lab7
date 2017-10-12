#' visualize_airport_delays
#'
#' @description A ggplot visualisation of flight delays in New York city. Uses information from 336,776 flights from three airports(JFK, LGA, EWR).
#' 
#' @examples
#' visualize_airport_delays()
#' @return A plot displaying the average delay for flights in 105 airports.
#' @export visualize_airport_delays
#' @import nycflights13

visualize_airport_delays <- function() {
  #Assumptions
  # Delay is the arr_delay
  # Airport for the mean delay calculation is the destination airport
  
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  #selecting required columns
  flights_rc <- dplyr::select(flights, arr_delay, dest)
  airports_rc <- dplyr::select(airports, faa, lat, lon)
  
  #Combining data sets
  airports_rc<- dplyr::rename(airports_rc, dest=faa)
  data_set <- dplyr::left_join(flights_rc, airports_rc, "dest")
  
  #set_0 function for early arrivals
  set_0 <- function(x) {
    x[x<0] <- 0
    return(x)
  }
  
  #new data_set with only plus delay
  data_set<-dplyr::mutate(data_set, plus_delay=set_0(arr_delay))
  
  avg_delay <- dplyr::summarise(dplyr::group_by(data_set, dest), "delay_avg"=mean(plus_delay, na.rm = TRUE))
  
  data_set <- dplyr::left_join(avg_delay, data_set, "dest")
  data_set <- dplyr::distinct(data_set)
  data_set <- as.data.frame(data_set)
  
  plot1 <- ggplot2::ggplot(data=data_set) + ggplot2::aes(x=lon,y=lat,size=delay_avg) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")+ ggplot2::ggtitle("Mean delay visualization") + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::labs(caption="2017 LiU") +
    ggplot2::scale_size_area(max_size = 10) + ggplot2::geom_point(shape=16 , col="red", na.rm = TRUE)
  
  plot(plot1)
}