# Load flight gps points
library(dplyr)


# Load flight data ------
flight.points <- read.csv(
  "D:/Dropbox/LBBG_flight_height/R_files/flight_subset_altitude_points.csv", header = TRUE)
flight.points <- as_tibble(flight.points)
flight.points$date_time <- as.POSIXct(flight.points$date_time,
                                      tz = "UTC")

# Get flight IDs plus points to keep -----
flight_points_numbered <- read.csv("D:/Dropbox/LBBG_flight_height/R_files/flights_points.csv", header = TRUE)


# Select only those flights in the flight list ----
flights_selected <- filter(flight.points, flight_id %in% flight_points_numbered$ID_FLIGHT)


gps_points_selected <- list()

flights <- unique(flights_selected$flight_id)

for(i in 1:length(flights)){
  x <- filter(flight.points, flight_id %in% flights[i])
  ix <- which(flight_points_numbered$ID_FLIGHT == flights[i])
  x <- x[flight_points_numbered$POINT_MIN[ix]:flight_points_numbered$POINT_MAX[ix],]
  gps_points_selected[[i]] <- x
    x <- NULL
  
}

gps_points_selected_df <- do.call(rbind.data.frame, gps_points_selected)

source("deg.dist.R")
library("geosphere")

#  Make summary table ------
flight_summary <- gps_points_selected_df %>% group_by(flight_id) %>% summarise(
  date_time_start = first(date_time),
  device_info_serial = first(device_info_serial),
  start_lat = first(latitude),
  start_long = first(longitude),
  end_lat = last(latitude),
  end_long = last(longitude),
  u_wind_mean = mean(wind_u_10m),
  v_wind_mean = mean(wind_v_10m),
  distance_km = deg.dist(start_long, start_lat,
                 end_long, end_lat),
  direction_bearing_deg = bearingRhumb(c(start_long, start_lat),
                                       c(end_long, end_lat)),

  wind_speed = sqrt(v_wind_mean*v_wind_mean +
                      u_wind_mean*u_wind_mean),
  
  v_wind_proj = (v_wind_mean)*cos(2*pi*(direction_bearing_deg/360)),
  u_wind_proj = (u_wind_mean)*cos(2*pi*((-90 + direction_bearing_deg)/360)),
  
  wind_proj = u_wind_proj + v_wind_proj
  
  
)

# cos(45/360*2*pi)

# Output this to file:
write.csv(flight_summary, file = "flights_winds.csv")
