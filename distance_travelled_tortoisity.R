# Analysing total distance travelled during long trips


# R packages ------
library(dplyr)

# For interpolation/re-scaling
library("adehabitatLT")



# Load data ------

# Load trip info
trips <- read.csv("long_trips_summary_trip_class_2017_02_26_for_R.csv",
                  header = TRUE)

# Load GPS points
gps.points <- read.csv(file = "gps_points_long_trips.csv",
                        header = TRUE)


str(trips)
str(gps.points)

# Fix date_time structure
gps.points$date_time <-  as.POSIXct(strptime(gps.points$date_time,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))



# Re-scale/sample GPS to common time interval (1h?) -----

# Treat each trip as a 'burst'
points_all.ltraj <- as.ltraj(gps.points[,4:3], gps.points$date_time,
                             gps.points$trip_id,
                                  burst = gps.points$trip_id, typeII = TRUE)
# ?as.ltraj

# 1h
points_all.ltraj.3600 <- redisltraj(points_all.ltraj, 3600, type = "time")

# See how a few of these look
plot(points_all.ltraj[10])
plot(points_all.ltraj.3600[10])


par(mfrow=c(2,2))
plot(points_all.ltraj[5])
plot(points_all.ltraj.3600[5])
plot(points_all.ltraj[60])
plot(points_all.ltraj.3600[60])


# Make into data frame
points.3600.df <- ld(points_all.ltraj.3600)



# Distance calculations
# use deg.dist function
source("deg.dist.R")
n <- nrow(points.3600.df)
p2p_dist <- deg.dist(points.3600.df$x[-n],
                     points.3600.df$y[-n],
                     points.3600.df$x[-1],
                     points.3600.df$y[-1],
                     km = FALSE)

p2p_dist <- c(NA,p2p_dist)
hist(p2p_dist)
# sum(p2p_dist == 0, na.rm = TRUE)
# hist(points.3600.df$y)

# Set NA values for first location on each trip
trip_ids <- unique(points.3600.df$burst)

for(i in 1:length(trip_ids)){
  p2p_dist[points.3600.df$burst == trip_ids[i]][1] <- NA
  
}

points.3600.df$p2p_dist_m <- p2p_dist


# Summarise each trip ----



trips.summary <- points.3600.df %>%
  group_by(burst) %>%
  summarise(
    # Total distance
    tot_dist = sum(p2p_dist_m, na.rm = TRUE),
    
    # Maximum distance from first location
    max_start = max(deg.dist(first(x),
                             first(y),
                             x,
                             y,
                             km = FALSE), na.rm = TRUE),
    # Tortoisity
    tortoisity = tot_dist/(2*max_start)
    
    
    
    )
    

# merge with original trips table ----
names(trips.summary)[1] <- "trip_id"
trips.info <- merge(trips, trips.summary)

boxplot(trips.info$tortoisity~trips.info$Sex+trips.info$Location)

boxplot(trips.info$tortoisity~trips.info$TRIP_CLASS+trips.info$Location,
        ylab = "Tortuosity")


boxplot(trips.info$tot_dist/1000~trips.info$TRIP_CLASS+trips.info$Location,
        ylab = "Distance (km)", ylim = c(0,max(trips.info$tot_dist/1000 + 50)))


write.csv(trips.info, file ="trip_info.csv")
