# Trip info summary


# Make a summary table of 'trips'
# Include, e.g. max dist, duration, max time gap...


# Required R package -----
library(RODBC)
# library(plyr)
library(dplyr)


# Load in data ------
# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/lbbg_all_db/lbbg_all.accdb')

# See what tables are available
sqlTables(gps.db)



# Load in GPS point data
gps.points <- sqlQuery(gps.db,
                       query =
                         "SELECT lund_lbbg_gps_points.device_info_serial, lund_lbbg_gps_points.date_time, lund_lbbg_gps_points.latitude, lund_lbbg_gps_points.longitude, lund_lbbg_gps_points.altitude, lund_lbbg_gps_points.speed_2d, lund_lbbg_gps_points.ring_number, lund_lbbg_gps_points.key_name, lund_lbbg_gps_points_info.nest_dist, lund_lbbg_gps_points_info.p2p_dist, lund_lbbg_gps_points_info.time_interval, lund_lbbg_gps_points_trip_id.trip_id
FROM (lund_lbbg_gps_points_info INNER JOIN lund_lbbg_gps_points ON (lund_lbbg_gps_points_info.date_time = lund_lbbg_gps_points.date_time) AND (lund_lbbg_gps_points_info.device_info_serial = lund_lbbg_gps_points.device_info_serial)) INNER JOIN lund_lbbg_gps_points_trip_id ON (lund_lbbg_gps_points.date_time = lund_lbbg_gps_points_trip_id.date_time) AND (lund_lbbg_gps_points.device_info_serial = lund_lbbg_gps_points_trip_id.device_info_serial)
WHERE (((lund_lbbg_gps_points_trip_id.on_trip)='TRUE'));
                       ",
                       as.is = TRUE)


# Fix data structure
str(gps.points)

gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
str(gps.points)


# Make unique table to trips -----
# E.g. 'unique ...'
# To get device_info_serial, trip_id, ring_number...
# Using tip from SO: http://stackoverflow.com/a/22960230/1172358 and comment under that
trips.df <- gps.points[,c(1,7,8,12)] %>% group_by(trip_id) %>% do(head(.,1))




# GPS points to filter out (bad locations) ----
gps.points <- dplyr::filter(gps.points, !(device_info_serial %in% c(645,522,522) &
                                            date_time %in% as.POSIXct(c("2012-06-14 04:32:41 UTC",
                                                                        "2012-07-20 13:00:32 UTC",
                                                                        "2012-07-20 13:02:26 UTC"),
                                                                      tz = "UTC")
))


# Use dplyr + plyr?? to summarise foraging trips -----

# Summary information for foraging trips
trips.df.info <- summarise(group_by(gps.points,
                               trip_id),
                      
                      # #details
                      # ring_number = first(ring_number),
                      # device_info_serial = first(device_info_serial),
                      # species = first(species_latin_name),
                      
                      
                      # Variables to extract
                      #Coldist
                      coldist_max = max(nest_dist, na.rm = TRUE),
                      col_dist_mean = mean(nest_dist, na.rm = TRUE),
                      col_dist_median = median(nest_dist, na.rm = TRUE),
                      col_dist_start = nest_dist[1],
                      col_dist_end = nest_dist[n()],
                      
                      #duration + time
                      start_time = first(date_time),
                      end_time = last(date_time),
                      duration_s = end_time - start_time,
                      
                      # Quality criteria
                      gps_time_interval_min = min(time_interval, na.rm = TRUE),
                      gps_time_interval_max = max(time_interval, na.rm = TRUE),
                      gps_time_interval_median = median(time_interval, na.rm = TRUE),
                      n_points = n(),
                      
                      # Distance moved
                      distance_total = sum(p2p_dist) - first(p2p_dist),
                      long_min = min(longitude),
                      long_max = max(longitude),
                      lat_min = min(latitude),
                      lat_max = max(latitude)
                      
                      
)




# Combine trip paramater summary table with trip ID summary stuff -----
trips.export <- left_join(trips.df, trips.df.info)

# Label Migratory trips (recognise by minimum latitude):
head(sort(trips.export$lat_min))
(sort(trips.export$lat_min))[1:50]

trips.export$trip_type <- "baltic"
trips.export$trip_type[trips.export$lat_min <50] <- "migration"
summary(as.factor(trips.export$trip_type))

# Check the format
str(trips.export)

# Export to DB ------
#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, trips.export, tablename = "lund_lbbg_gps_trips2",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(start_time = "Date",
                     end_time = "Date")
)
# ?sqlSave
close(gps.db)



