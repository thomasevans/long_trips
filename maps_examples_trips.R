# Trip illustration figures
# Some examples of long trips, one each for the two colonies


# Required packages ------
library(maps)
library(maptools)
library(mapdata)
library(dplyr)
library(RODBC)
library(sp)

# Helper functions -----

# Map scale
source("map.scale2.R")


# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# Load data ------

# Load trip data
load("long_trips.Rdata")

str(long_trips$trip_id)
long_trips <- filter(long_trips, !(trip_id %in% c(1076,1623,1711)))
# 2070,2072)))


# GPS point data
# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/lbbg_all_db/lbbg_all.accdb')

# See what tables are available
# sqlTables(gps.db)

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


# GPS points to filter out (bad locations)
gps.points <- dplyr::filter(gps.points, !(device_info_serial %in% c(645,522,522) &
                                            date_time %in% as.POSIXct(c("2012-06-14 04:32:41 UTC",
                                                                        "2012-07-20 13:00:32 UTC",
                                                                        "2012-07-20 13:02:26 UTC"),
                                                                      tz = "UTC")))
                                          

# Map data
map.data <- list()

# list of map files
files <- list.files(pattern = "\\.rds$",
                    all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE)

for(i in 1:length(files)){
  map.data[[i]] <- readRDS(paste(files[i]))
  # map.data[[i]]$data$place <- "baltic_country"
}
# str(map.data[[i]])


# Combine to single spatialpolygondataframe
all_coast <- do.call(raster::bind, map.data) 
# plot(all_coast)

# Trim to study area (whole Baltic sea + a bit more)
all_coast_baltic <- raster::crop(all_coast, raster::extent(c(10, 32, 52, 69)))

# Check how this looks
# map(all_coast_baltic)

                         
# Colour -----
# http://colorbrewer2.org/?type=qualitative&scheme=Dark2&n=7
# 7-class Dark2
cols <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d')
col.alpha <- addalpha(cols, 0.6)


# Plot example map 1 (Fågelsundet) -----



pdf("map_combined.pdf", width = 8, height = 5)
png("map_combined.png", width = 8, height = 5,
    res = 600, units = "in")
# Ring_number
ring_number <- 8120614
trips_ids <- long_trips$trip_id[long_trips$ring_number == ring_number]

gps.points.f <- filter(gps.points, trip_id %in% trips_ids)


# Side by side
par(mfrow=c(1,2))
# plot(1:10)

# Set plot margins
par( mar = c(1.5, 2, .5, .5))

xlim <- range(gps.points.f$longitude)
ylim <- range(gps.points.f$latitude)

# Trim to study area (whole Baltic sea + a bit more)
all_coast_baltic2 <- raster::crop(all_coast_baltic, raster::extent(c(11, 28, 54, 65)))


plot(all_coast_baltic2, xlim = xlim,
     ylim = ylim, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.2,
     main = "",
     lty = 1)

# Add axis
axis(side=(1),las=1, cex.lab = 0.6, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.6, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Outline box
box(lwd=2)


# Plot GPS data

for(i in 1:length(trips_ids)){
  
  gps.sub <- filter(gps.points.f, trip_id == trips_ids[i])
  
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           col = col.alpha[i], lty = 1, lwd = 1)

}

# Add colony locations:
points(c(17.93, 17.972088),
       c(60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

# ?legend


# legend("topleft", "A)", bty="n", cex = 1.4) 
# legend("topright", "8120614", bty="n", cex = 1.4) 
legend("topleft", "A", bty="n", cex = 1.8, adj = c(2,-0.4))
legend("topright", "8120614", bty="n", cex = 1.4, adj = c(0,-0.4))
# 
# legend(x = "topright",
#        legend = c("8120614"),
#        bty = "n",
#        cex = 1.4)

# Add map scale bar
map.scale2(x = 20, y = 55.2, ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.6)
# dev.off()


# Stora Karlsö ----
# Ring_number
ring_number <- 8114316
trips_ids <- long_trips$trip_id[long_trips$ring_number == ring_number]

gps.points.f <- filter(gps.points, trip_id %in% trips_ids)
all_coast_baltic2 <- raster::crop(all_coast_baltic, raster::extent(c(14, 22, 53.5, 59)))

# pdf("map_stora_karlso_8114316.pdf", width = 4, height = 5)

# Set plot margins
par( mar = c(1.5, 2, .5, .5))

xlim <- range(gps.points.f$longitude)
ylim <- range(gps.points.f$latitude)

plot(all_coast_baltic2, xlim = xlim,
     ylim = ylim, col= "dark grey", bg = NA,
     # main = title.text,
     border = "black",
     lwd = 0.2,
     main = "",
     lty = 1)

# Add axis
axis(side=(1),las=1, cex.lab = 0.6, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.6, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Outline box
box(lwd=2)


# Plot GPS data

for(i in 1:length(trips_ids)){
  
  gps.sub <- filter(gps.points.f, trip_id == trips_ids[i])
  
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           col = col.alpha[i], lty = 1, lwd = 1)
  
}

# Add colony locations:
points(c(17.93, 17.972088),
       c(60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

# legend(x = "topleft",
#        legend = c("B"),
#        bty = "n",
#        cex = 1.4)
# 
# legend(x = "topright",
#        legend = c("8114316"),
#        bty = "n",
#        cex = 1.4)

legend("topleft", "B", bty="n", cex = 1.8, adj = c(2,-0.4))
legend("topright", "8114316", bty="n", cex = 1.4, adj = c(0,-0.4))
# ?legend
# usr <- par( "usr" )
# text( usr[ 1 ], usr[ 4 ], "B",    pos = c(1,4), cex = 1.6)
# text( usr[ 2 ], usr[ 4 ], "8114316",     pos = c(1,2), cex = 1.4)
# # ?text
# Add map scale bar
map.scale2(x = 17.6, y = 54.2, ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.6)
dev.off()