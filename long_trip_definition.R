# Defining long trips 

# What are long trips?
# Define by duration or distance, or a combination of the two?



# Required R package -----
library(RODBC)
# library(plyr)
library(dplyr)
# library(ggplot2)

# Load in trip summary data -----
# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/lbbg_all_db/lbbg_all.accdb')

# See what tables are available
# sqlTables(gps.db)


# Load in GPS point data
trips <- sqlQuery(gps.db,
                       query =
                         "SELECT DISTINCT g.*
                       FROM lund_lbbg_gps_trips2 AS g
                       ORDER BY g.device_info_serial, g.trip_id;
                       ",
                       as.is = TRUE)


# Fix data structure
str(trips)

trips$start_time <-  as.POSIXct(strptime(trips$start_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
trips$end_time <-  as.POSIXct(strptime(trips$end_time,
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "UTC"))
str(trips)


# Filter out migratory trips
trips.baltic <- filter(trips, trip_type == "baltic")

# Some basic quality criteria (quite crude)
# at least 10 points, gaps no more than 12 hours
trips.baltic.hq <- filter(trips.baltic,
                          (n_points >9) &
                            (gps_time_interval_max < 12*60*60))






# Make some summary plots of data distribution (distance and duration) -----
hist(trips.baltic.hq$duration_s/60/60/24)
trips.baltic.hq$duration_days <- trips.baltic.hq$duration_s/60/60/24

x <- (rev(sort(trips.baltic.hq$duration_days)))
x[1:100]

# Histrogram of duration (days)
hist(trips.baltic.hq$duration_days, ylim = c(0,200),
     breaks = c(0:ceiling(max(trips.baltic.hq$duration_days))))

# round(7, -1)
# Histrogram of max distance (kms)
png("trip_distances.png", res = 300,
    width = 8, height = 4, units = "in")
hist(trips.baltic.hq$coldist_max/1000, ylim = c(0,200),
     breaks = seq(0,round(ceiling(max(trips.baltic.hq$coldist_max/1000)),-1), 5),
     main = "Distance from colony (maximum)",
     xlab = "Distance (km) - 5 km bins",
     ylab = "Trips (n)")
abline(v=150, lty = 2)
text("150 km", x = 200, y = 100)
dev.off()

# >150 km
long_trips <- filter(trips.baltic.hq, coldist_max > 150000)


hist(long_trips$gps_time_interval_max/(60*60),
     ylim = c(0,50))
abline(v=12*60*60)
median(long_trips$gps_time_interval_max/(60))

range(long_trips$n_points)
median(long_trips$n_points)
hist(long_trips$n_points)

# How many birds do at least one long trip?
unique(long_trips$ring_number)
unique(trips$ring_number)


save(long_trips, file = "long_trips.RData")
