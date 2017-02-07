# Assemble data for long trips, and make some figures


# Required R package -----
library(RODBC)
# library(plyr)
library(dplyr)



# Load in data -----
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


# Load trip data
load("long_trips.Rdata")

str(long_trips$trip_id)
long_trips <- filter(long_trips, !(trip_id %in% c(1076,1623,1711)))


# Load sex data
sex.tab <- read.csv("simplified_sex_ring_number_only.csv",
                    header = TRUE)
# sex.tab[duplicated(sex.tab$ring_number),]

# Combine with long_trips dataframe
long_trips.new <- inner_join(long_trips, sex.tab)
nrow(long_trips.new)
length(unique(long_trips.new$trip_id))

table(long_trips.new$Location, long_trips.new$key_name)

long_trips <- long_trips.new

# Filter GPS data ------
gps.points <- filter(gps.points, trip_id %in% long_trips$trip_id)


# Put GPS point data to file ------
write.csv(gps.points, file = "gps_points_long_trips.csv",
          row.names = FALSE)

# Put summary data out to file -----
write.csv(long_trips, file = "long_trips_summary.csv",
          row.names = FALSE)



# Split GPS data by colony and individual ------
# Split by colony first
gps.points.fag <- filter(gps.points, key_name == "V_FAGELSUNDET")
# unique(gps.points$key_name)

write.csv(gps.points.fag, file = "gps_points_long_trips_fagelsundet.csv",
          row.names = FALSE)


gps.points.sk <- filter(gps.points, key_name == "LBBG_STKARLSO")
# unique(gps.points$key_name)

write.csv(gps.points.sk, file = "gps_points_long_trips_stora_karlso.csv",
          row.names = FALSE)


# Individuals:
individuals <- unique(gps.points$ring_number)
# i <- 1
for(i in 1:length(individuals)){
  gps.points.f <- filter(gps.points, ring_number == individuals[i])
  # unique(gps.points$key_name)
  
  sex <- long_trips$Sex[long_trips$ring_number == individuals[i]][1]
  
  if(gps.points.f$key_name[1] == "LBBG_STKARLSO") {col <- "stora_karlso"}else if(
    gps.points.f$key_name[1] == "V_FAGELSUNDET"
  ){col <- "fagelsundet"} else col <- "NA"
  
  write.csv(gps.points.f, file = 
              paste("gps_points_long_trips_",
                    col, "_",
                    sex,
                    "_",
                    individuals[i],
                    ".csv"),
            row.names = FALSE
            )
  
}




# Map the trips and output to figures ------
library(maps)
library(mapdata)
source("map.scale2.R")

i <- 1



# ?pdf
pdf("long_trip_maps.pdf")
    # , paper = "a4")
# xlim = range(points.f$longitude),
# ylim = range(points.f$latitude)


for(i in 1:nrow(long_trips)){
  trip_idx <- long_trips$trip_id[i]
  
  points.f <- filter(gps.points, trip_id == trip_idx)
      
      
    
    par( mar = c(1.5, 2, .5, .5))
    
    # Basic map
    xlims <- range(points.f$longitude)
    ylims <- range(points.f$latitude)
    xlims[1] <- xlims[1] - (0.15*(xlims[2]-xlims[1]))
    xlims[2] <- xlims[2] + (0.15*(xlims[2]-xlims[1]))
    ylims[1] <- ylims[1] - (0.15*(ylims[2]-ylims[1]))
    ylims[2] <- ylims[2] + (0.15*(ylims[2]-ylims[1]))
    
    ydif <- ylims[2]-ylims[1]
    xdif <- xlims[2]-xlims[1]
    
    if(ydif < .5*xdif){
      ylims[1] <- ylims[1] - 0.1*xdif
      ylims[2] <- ylims[2] + 0.1*xdif
    } else if(xdif < 2 * ydif){
      xlims[1] <- xlims[1] - 0.4*ydif
      xlims[2] <- xlims[2] + 0.4*ydif
    }
      
    
    
    map(database = "world",
        xlim = xlims,
        ylim = ylims,
        # col= "dark grey", bg = NA,
         # main = title.text,
         main = "",
        fill = TRUE,
        col = "grey")
    # ?map
    axis(side=(1),las=1, cex.lab = 0.5, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
    axis(side=(2),las=1, cex.lab = 0.5, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)
    
    ## Scale bar and axis
    box(lwd=1.5)
    
    # Add map scale bar
    map.scale2(ratio = FALSE, lwd.line = 1.5,
               relwidth = 0.25, cex = 0.8)
    
    
    
    
    # Plot lines
    n <- nrow(points.f)
    segments(points.f$longitude[-1], points.f$latitude[-1],
             points.f$longitude[1:n-1], points.f$latitude[1:n-1],
             lty = 1, lwd = 0.8)
    
    # Plot points
    points(x = points.f$longitude, y = points.f$latitude)
    
    
    # Add colony locations:
    points(c(17.93, 17.972088), c(60.63, 57.284804), pch = 23,
           col = "red", bg = NA,
           cex = 1.5)
    
    
    legend("topleft",
           paste("Trip: ", long_trips$ring_number[i], "_", trip_idx, sep = ""),
           bty="n",
           text.col  = "red") 
    
    legend("bottomright",
           legend =
             c(paste("Depart: ", long_trips$start_time[i], sep = ""),
               paste("Duration: ", round(long_trips$duration_days[i],2), " days", sep = ""),
               paste("Distance (max): ", round(long_trips$coldist_max[i]/1000,1), " km", sep = "")),
           bty="n",
           text.col  = "red") 
}
dev.off()