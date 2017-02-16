# Plotting foraging trip frequencies and distances


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

# Exclude a few odd trips (erroneous GPS fixes etc) ----
trips_to_keep <- filter(trips.baltic.hq, !(trip_id %in% c(1076,1623,1711)))
# 2070,2072)))
# trips_to_keep$coldist_max
# Trips >3 km
trips_to_keep <- filter(trips_to_keep, coldist_max > 3000)


# "2012-07-20 13:00:32 UTC" "2012-07-20 13:02:26 UTC" both device_info_serial == 522



# Load more data ------

# Load trip data
load("long_trips.Rdata")

# str(long_trips$trip_id)
long_trips <- filter(long_trips, !(trip_id %in% c(1076,1623,1711)))
# 2070,2072)))

trips_to_keep$long <- trips_to_keep$trip_id %in% long_trips$trip_id



# add some more data columns ------

# Julian date+time of trip departure (tip from http://stackoverflow.com/a/27594294/1172358):
trips_to_keep$j_date <- sapply(trips_to_keep$start_time,
                               function(x) julian(
                                 x, origin = as.POSIXct(
                                   paste0(format(x, "%Y"),'-01-01'), tz = 'GMT')))

# get trip year
trips_to_keep$year <- format(trips_to_keep$start_time,'%Y') 


# Load sex data
sex.tab <- read.csv("simplified_sex_ring_number_only.csv",
                    header = TRUE)
# sex.tab[duplicated(sex.tab$ring_number),]

# Combine with long_trips dataframe
trips_to_keep <- inner_join(trips_to_keep, sex.tab)


trips_to_keep$Site <- trips_to_keep$key_name
trips_to_keep$Site[trips_to_keep$Site == "LBBG_STKARLSO"] <- "SKA"
trips_to_keep$Site[trips_to_keep$Site == "V_FAGELSUNDET"] <- "FÅG"

# ID (ring_number + year)
trips_to_keep$ring_year <- paste(trips_to_keep$Site,"_",
                                 trips_to_keep$Sex,"_",
                                 trips_to_keep$ring_number,"_",
                                 trips_to_keep$year, sep = "")

# Duration days
trips_to_keep$duration_day <- trips_to_keep$duration_s/(60*60*24)

# Make a ggplot thing of this -------
library(ggplot2)
library(cowplot)

# Download ggforce allows pagination of 
# devtools::install_github('thomasp85/ggforce')

library(ggforce)


# Themes etc for ggplot figures
theme_new <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "none",
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12, face = "italic"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        # legend.text = element_text(colour="blue", size = 16, face = "bold")
        legend.key.width = unit(1, "lines"),
        legend.title = element_blank(),
        strip.text.y = element_text(size = 8, colour = "black", angle = 0)
  )
# 
# # Fågelsundet only
# trips.fag <- filter(trips_to_keep, key_name == "V_FAGELSUNDET")
# str(trips.fag)
# 
# fag_plot <- ggplot(trips.fag, aes(x = j_date, y = coldist_max/1000)) +
#   geom_bar(stat="identity", width = 0.4) + facet_grid(ring_year~.)
# 
# ggsave("test.svg", width = 10, height = 12)

# unique(trips_to_keep$key_name)
# hist(trips.sk$duration_day)


# Label months
dates <- c("2015-04-01", "2015-05-01", "2015-06-01", "2015-07-01", "2015-08-01", "2015-09-01")

dates_j <- sapply(as.POSIXct(dates),
                  function(x) julian(
                    x, origin = as.POSIXct(
                      paste0(format(x, "%Y"),'-01-01'), tz = 'GMT')))



# SK only
trips.f <- filter(trips_to_keep, key_name == "V_FAGELSUNDET")
str(trips.f)

fag_plot <- ggplot(trips.f, aes(x = (j_date-(duration_day/2)),
                                 y = coldist_max/1000,
                                 colour = long,
                                 fill = long))+
                                 # width = duration_day))+
                                 # ,
                                 # position = -(duration_day/2))) +
  geom_bar(stat="identity", width = 0.5,
           alpha = 0.6) + facet_grid(ring_year~.) +
  labs(x = "Date", y = "Maximum distance (km)")+
  scale_x_continuous(breaks=dates_j,
                     labels=c("April","May", "June", "July", "August",
                              "September")) +
  theme_new
# ?geom_bar
ggsave("fagelsundet_birds_trip_duration_date.svg", width = 10, height = 8)
ggsave("fagelsundet_birds_trip_duration_date.pdf", width = 10, height = 8)





# Stora Karlsö only
trips.sk <- filter(trips_to_keep, key_name == "LBBG_STKARLSO")
str(trips.sk)

for(i in 1:ceiling(length(unique(trips.sk$ring_year))/10)){
  fag_plot <- ggplot(trips.sk, aes(x = (j_date),
                                   y = coldist_max/1000,
                                   colour = long,
                                   fill = long))+
    # width = duration_day))+
    # ,
    # position = -(duration_day/2))) +
    geom_bar(stat="identity", width = 0.5,
             alpha = 0.6) + 
    # facet_grid(ring_year~.) +
    facet_grid_paginate(ring_year~., ncol = 1, nrow = 10, page = i)+
    labs(x = "Date", y = "Maximum distance (km)")+
    scale_x_continuous(breaks=dates_j,
                       labels=c("April","May", "June", "July", "August",
                                "September")) +
    theme_new
  ggsave(paste("Karlso_birds_trip_duration_date_", i, ".pdf", sep = ""),
         width = 8, height = 8)
  # ggsave("Karlso_birds_trip_duration_date.pdf", width = 10, height = 12)
}

# ?geom_bar





# Fågelsundet only
trips.sk <- filter(trips_to_keep, key_name == "V_FAGELSUNDET")
str(trips.sk)

for(i in 1:ceiling(length(unique(trips.sk$ring_year))/10)){
  fag_plot <- ggplot(trips.sk, aes(x = (j_date),
                                   y = coldist_max/1000,
                                   colour = long,
                                   fill = long))+
    # width = duration_day))+
    # ,
    # position = -(duration_day/2))) +
    geom_bar(stat="identity", width = 0.5,
             alpha = 0.6) + 
    # facet_grid(ring_year~.) +
    facet_grid_paginate(ring_year~., ncol = 1, nrow = 10, page = i)+
    labs(x = "Date", y = "Maximum distance (km)")+
    scale_x_continuous(breaks=dates_j,
                       labels=c("April","May", "June", "July", "August",
                                "September")) +
    theme_new
  ggsave(paste("Fagelsundet_birds_trip_duration_date_", i, ".pdf", sep = ""),
         width = 8, height = 8)
  # ggsave("Karlso_birds_trip_duration_date.pdf", width = 10, height = 12)
}




# Combined ----
# trips_to_keep$Site <- trips_to_keep$key_name
# trips_to_keep$Site[trips_to_keep$Site == "LBBG_STKARLSO"] <- "Stora Karlsö"
# trips_to_keep$Site[trips_to_keep$Site == "V_FAGELSUNDET"] <- "Fågelsundet"

for(i in 1:ceiling(length(unique(trips_to_keep$ring_year))/11)){
  fag_plot <- ggplot(trips_to_keep, aes(x = (j_date),
                                   y = coldist_max/1000,
                                   colour = long,
                                   fill = long))+
    # width = duration_day))+
    # ,
    # position = -(duration_day/2))) +
    geom_bar(stat="identity", width = 0.5,
             alpha = 0.6) + 
    # facet_grid(ring_year~.) +
    facet_grid_paginate(ring_year~., ncol = 1, nrow = 11, page = i)+
    labs(x = "Date", y = "Maximum distance (km)")+
    scale_x_continuous(breaks=dates_j,
                       labels=c("April","May", "June", "July", "August",
                                "September")) +
    theme_new
  ggsave(paste("Karlso_birds_trip_duration_date_", i, ".pdf", sep = ""),
         width = 9.41, height = 6.65)
  # ggsave("Karlso_birds_trip_duration_date.pdf", width = 10, height = 12)
}


# Make a unique table of deployments -----
str(trips_to_keep)
# trips.df <- trips_to_keep[,c(1:3,)] %>% group_by(trip_id) %>% do(head(.,1))




trips.df <-trips_to_keep %>%
  group_by(ring_year) %>%
  summarise(
    
    #details
    location_key = first(key_name),
    ring_number = first(ring_number),
    device_info_serial = first(device_info_serial),
    year = first(year),
    trip_n_all = n(),
    trip_n_long = sum(long),
    trips_long_prop = trip_n_long/trip_n_all,
    first_track_in_year = first(start_time),
    last_track_in_year = last(end_time),
    first_to_last_track_period_days = ceiling(as.numeric(last_track_in_year - first_track_in_year)/(60*24*60))
    
    
  )
    # unique(trips.df$location)
    # trips.df$location[trips.df$location == "LBBG_STKARLSO"] <- "Stora Karlsö"
    # trips.df$location[trips.df$location == "V_FAGELSUNDET"] <- "Fågelsundet"
    # 

    
    # Load sex data
    sex.tab <- read.csv("simplified_sex_ring_number_only.csv",
                        header = TRUE)
    # sex.tab[duplicated(sex.tab$ring_number),]
    
    # Combine with long_trips dataframe
    trips.df_sex <- inner_join(trips.df, sex.tab)
    
    trips.df$ring_number[!(trips.df$ring_number %in% trips.df_sex$ring_number)]
    
    
    write.csv(trips.df_sex, file = "trips_tracked_all.csv")
    