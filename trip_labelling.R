# Trip definition and numbering


# Require packages ------
library(RODBC)
library(dplyr)



# Load in data ------
# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/lbbg_all_db/lbbg_all.accdb')

# See what tables are available
sqlTables(gps.db)


# Load in GPS point data
gps.points <- sqlQuery(gps.db,
                       query =
                         "SELECT DISTINCT g.*
                       FROM lund_lbbg_gps_points_info AS g
                       ORDER BY g.device_info_serial, g.date_time;
                       ",
                       as.is = TRUE)





# GPS data with calculated variables
str(gps.points)


# fix data structure (if needed)
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
str(gps.points)


# Point 'on trip', if over distance threshold -------
hist(gps.points$nest_dist[gps.points$nest_dist < 5000],
     breaks = 50)

gps.points$on_trip <- gps.points$nest_dist > 1000


# Go through points and number trips ------
trip_id <- NULL

trip_num <- 0

trip_id[1] <- NA
for(i in 2:(nrow(gps.points))){
  
  # same bird
  new.bird <- gps.points$device_info_serial[i] != gps.points$device_info_serial[i-1]
  
  if(gps.points$on_trip[i]){
    # If it's either a new bird, or the previous point wasn't on a trip, increment trip number by 1
    if(new.bird| is.na(trip_id[i-1])){trip_num <- trip_num + 1}
    trip_id[i] <- trip_num
  }
  
}

summary(trip_id)

# trip_id[nrow(gps.points)] <- NA
gps.points$trip_id <- trip_id

# Put info together in table for export ----
# Include, on_trip, trip_id, device_info_serial, and date_time
gps.info <- select(gps.points, one_of(c("device_info_serial",
                                         "date_time",
                                         "on_trip",
                                         "trip_id")))
str(gps.info)

# Export to DB ------
#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps.info, tablename = "lund_lbbg_gps_points_trip_id",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)
# ?sqlSave
close(gps.db)

