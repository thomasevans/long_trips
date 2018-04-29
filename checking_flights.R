library(dplyr)


# Load flight data ------
flight.points <- read.csv(
  "D:/Dropbox/LBBG_flight_height/R_files/flight_subset_altitude_points.csv", header = TRUE)
flight.points <- as_tibble(flight.points)
flight.points$date_time <- as.POSIXct(flight.points$date_time,
                                      tz = "UTC")


# Inspect flight 14477 -----
flight_id14477 <- filter(flight.points, flight_id == 14477)

par(mfrow=c(2,1))
plot(flight_id14477$speed_3d~flight_id14477$date_time)
plot(flight_id14477$altitude~flight_id14477$date_time)




# V accuracy ----
flight_ids <- c(
  24234, 436, 564,571,
  5225, 5917, 32591,
  14477, 16542, 16606,
  20104, 24611, 30960,
  38745, 31210, 31321, 31599,
  31861)
flight_sub <- subset(flight.points, flight_id %in% flight_ids)


hist(flight_sub$v_accuracy, breaks = 50,
     xlim = c(0,30),
     main = "",
     xlab = "Vertical accuracy (m?)")
summary(flight_sub$v_accuracy)

plot(flight_sub$v_accuracy)

hist(as.numeric(flight_sub$satellites_used),
     main = "",
     xlab = "# sats")
# Most don't have satellite data



# Sat count etc ------------------
library(dplyr)
str(flight_sub$satellites_used)

# Replace the missing value with an R readable missing value
levels(flight_sub$satellites_used)[levels(flight_sub$satellites_used)=="\\N"] <- "NA"

# Stored as factor - convert to numeric
flight_sub$satellites_used <- as.numeric(as.character(flight_sub$satellites_used))

# sum(is.na(flight_sub$satellites_used))

# Make summaries for each flight of GPS quality criteria
flight_summary <- flight_sub %>% group_by(flight_id) %>% summarise(
  date_time_start = first(date_time),
  device_info_serial = first(device_info_serial),
  sat_min = min(satellites_used),
  sat_max = max(satellites_used),
  sat_mean = mean(satellites_used),
  sat_median = median(satellites_used),
  v_accuracy_mean = mean(v_accuracy, na.rm = TRUE),
  v_accuracy_median = median(v_accuracy, na.rm = TRUE),
  pos_dop_mean = mean(positiondop),
  pos_dop_median = median(positiondop)
  
)

# Output above table to file
write.csv(flight_summary, file = "flight_gps_quality_table.csv")
