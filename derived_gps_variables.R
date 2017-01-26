# Derived variables from GPS


# Required packages -------
library(RODBC)
library(dplyr)

source("deg.dist.R")

# Load in data (connect to DB) --------

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/lbbg_all_db/lbbg_all.accdb')

# See what tables are available
sqlTables(gps.db)


# Load in GPS point data
gps.points <- sqlQuery(gps.db,
                       query =
                         "SELECT DISTINCT g.*
                       FROM lund_lbbg_gps_points AS g
                       ORDER BY g.device_info_serial, g.date_time;
                       ",
                       as.is = TRUE)

str(gps.points)

gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
str(gps.points)

# Load in bird/ GPS deployment data
# (includes nest location used for distance from colony)
birds <- sqlQuery(gps.db,
                       query =
                         "SELECT DISTINCT b.*
                       FROM lund_LBBG_all_deployment_info AS b;
                       ",
                       as.is = TRUE)
str(birds)

# fix data structure
birds$start_date <-  as.POSIXct(strptime(birds$start_date,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
birds$end_date <-  as.POSIXct(strptime(birds$end_date,
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "UTC"))
birds$device_info_serial <- as.integer(birds$device_info_serial)
birds$latitude <- as.double(birds$latitude)
birds$longitude <- as.double(birds$longitude)

# Check this looks ok
str(birds)



# Filter GPS data by some quality criteria ------
# Remove rows with NAs for long, lat, or speed
gps.points2 <- filter(gps.points, (!is.na(longitude) &
                                     !is.na(latitude) &
                                     !is.na(speed_2d)))

# Calculate distance from colony -------
# Get required columns, and drop two duplicated rows
birds.nest <- birds[-c(2:3),c(3:5)]
str(birds.nest)

# Re-name latitude and longitude to have unique column names
names(birds.nest)[c(2:3)] <- c("nest_lat", "nest_long")

# For some reason some birds are missing a specified nest location - add this
unique(gps.points2$device_info_serial)[!(unique(gps.points2$device_info_serial) %in% birds.nest$device_info_serial)]

# "8120502" "8120504" "8120503" "8120612"

nests.missing <- read.csv("missing_nests.csv", header = TRUE)
str(nests.missing)
nests.missing <- nests.missing[,-2]
names(nests.missing)[c(2:3)] <-  c("nest_lat", "nest_long")
# nests.missing$ring_number <- as.character(nests.missing$ring_number)

birds.nest <- rbind.data.frame(birds.nest, nests.missing)

gps.points3 <- left_join(gps.points2, birds.nest)

# Any missing nest location?
any(is.na(gps.points3$nest_lat))

gps.points3$nest_dist <- mapply(deg.dist,
                                long1 = gps.points3$longitude,
                                lat1 = gps.points3$latitude,
                                long2 = gps.points3$nest_long,
                                lat2 = gps.points3$nest_lat,
                                km = FALSE)


# Calculate inter-point distance ------
# (Distance moved between fixes)

p2p_dist <- mapply(deg.dist,
                                long1 = gps.points3$longitude[-nrow(gps.points3)],
                                lat1 = gps.points3$latitude[-nrow(gps.points3)],
                               long2 = gps.points3$longitude[-1],
                               lat2 = gps.points3$latitude[-1],
                                km = FALSE)

gps.points3$p2p_dist <- c(0, p2p_dist)


# Calculate time between fixes (time interval) -------
time_interval <- difftime(gps.points3$date_time[-1],
                          gps.points3$date_time[-nrow(gps.points3)],
                          units = "secs")
head(time_interval)

# Add value for first point
gps.points3$time_interval <- c(0,time_interval)


# Prepare dataframe for export ------
str(gps.points3)

gps.info <- select(gps.points3, one_of(c("device_info_serial",
                                  "date_time",
                                  "nest_dist",
                                  "p2p_dist",
                                  "time_interval")))


# Export dataframe to DB -------

#export trip information to the database
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps.info, tablename = "lund_lbbg_gps_points_info",
        append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,
        fast = TRUE, test = FALSE, nastring = NULL,
        varTypes = c(date_time = "Date")
)

close(gps.db)



