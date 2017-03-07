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


# write.csv(trips.info, file ="trip_info.csv")

# Reset graphics device
dev.off()
dev.off()

# Stats -----



library(lme4)
library(arm)
library(lattice)
library(MuMIn)

library(pbkrtest)

KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}


# total distal LMMs -------
trips.info$Sex_n <- trips.info$Sex
trips.info$Sex_n[(trips.info$Sex_n) == "U"] <- NA

hist(sqrt(trips.info$tot_dist))

trips.info$tot_dist_sqrt <- sqrt(trips.info$tot_dist/1000)

mods <- list()
mods[1] <- lmer(tot_dist_sqrt ~
                  TRIP_CLASS+Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[2] <- lmer(tot_dist_sqrt ~
                  TRIP_CLASS+Location+
                  (1|ring_number),
                data = trips.info)

# anova(mods[[1]], mods[[2]])

mods[3] <- lmer(tot_dist_sqrt ~
                  TRIP_CLASS+Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[4] <- lmer(tot_dist_sqrt ~
                  TRIP_CLASS+
                  (1|ring_number),
                data = trips.info)

mods[5] <- lmer(tot_dist_sqrt ~
                  Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[6] <- lmer(tot_dist_sqrt ~
                  Location+
                  (1|ring_number),
                data = trips.info)

mods[7] <- lmer(tot_dist_sqrt ~
                  Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[8] <- lmer(tot_dist_sqrt ~
                  1+
                  (1|ring_number),
                data = trips.info)


# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)

mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                mods.aicc.dif,
                                t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

mods.fit.df

# Save to csv file
write.csv(mods.fit.df, file = "tot_dist_models.csv")

# Check model behaviour
plot(mods[[1]])
qqmath(mods[[1]])

summary(mods[[1]])

# Refit with missing sex removed
mods[[1]] <- lmer(tot_dist_sqrt ~
                    TRIP_CLASS+Sex_n+Location+
                    (1|ring_number),
                  data = trips.info[!is.na(trips.info$Sex_n),])


# P values for gull model
drop1(mods[[1]], test="user", sumFun=KRSumFun)
# drop1(mods[[1]], test="user", sumFun=KRSumFun)

mod.coef <- summary(mods[[1]])$coef[, 1]
mod.ci <- confint(mods[[1]], method="Wald")
mod.par_df <- cbind.data.frame(mod.coef,mod.ci[-c(1:2),])
mod.par_df




# tortuosity LMMs -------
trips.info$Sex_n <- trips.info$Sex
trips.info$Sex_n[(trips.info$Sex_n) == "U"] <- NA

hist(log10(trips.info$tortoisity))

trips.info$tortuosity_log10 <- log10(trips.info$tortoisity)

mods <- list()
mods[1] <- lmer(tortuosity_log10 ~
                  TRIP_CLASS+Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[2] <- lmer(tortuosity_log10 ~
                  TRIP_CLASS+Location+
                  (1|ring_number),
                data = trips.info)

# anova(mods[[1]], mods[[2]])

mods[3] <- lmer(tortuosity_log10 ~
                  TRIP_CLASS+Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[4] <- lmer(tortuosity_log10 ~
                  TRIP_CLASS+
                  (1|ring_number),
                data = trips.info)

mods[5] <- lmer(tortuosity_log10 ~
                  Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[6] <- lmer(tortuosity_log10 ~
                  Location+
                  (1|ring_number),
                data = trips.info)

mods[7] <- lmer(tortuosity_log10 ~
                  Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[8] <- lmer(tortuosity_log10 ~
                  1+
                  (1|ring_number),
                data = trips.info)


# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)

mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                mods.aicc.dif,
                                t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

mods.fit.df

# Save to csv file
write.csv(mods.fit.df, file = "tort_models.csv")

# Check model behaviour
plot(mods[[4]])
qqmath(mods[[4]])

summary(mods[[4]])


# P values for gull model
drop1(mods[[4]], test="user", sumFun=KRSumFun)

mod.coef <- summary(mods[[4]])$coef[, 1]
mod.ci <- confint(mods[[4]], method="Wald")
mod.par_df <- cbind.data.frame(mod.coef,mod.ci[-c(1:2),])
mod.par_df





# max distal LMMs -------
trips.info$Sex_n <- trips.info$Sex
trips.info$Sex_n[(trips.info$Sex_n) == "U"] <- NA

hist(log10(trips.info$coldist_max/1000))

trips.info$max_dist_km_log10 <- log10(trips.info$coldist_max/1000)

mods <- list()
mods[1] <- lmer(max_dist_km_log10 ~
                  TRIP_CLASS+Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[2] <- lmer(max_dist_km_log10 ~
                  TRIP_CLASS+Location+
                  (1|ring_number),
                data = trips.info)

# anova(mods[[1]], mods[[2]])

mods[3] <- lmer(max_dist_km_log10 ~
                  TRIP_CLASS+Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[4] <- lmer(max_dist_km_log10 ~
                  TRIP_CLASS+
                  (1|ring_number),
                data = trips.info)

mods[5] <- lmer(max_dist_km_log10 ~
                  Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[6] <- lmer(max_dist_km_log10 ~
                  Location+
                  (1|ring_number),
                data = trips.info)

mods[7] <- lmer(max_dist_km_log10 ~
                  Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[8] <- lmer(max_dist_km_log10 ~
                  1+
                  (1|ring_number),
                data = trips.info)


# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)

mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                mods.aicc.dif,
                                t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

mods.fit.df

# Save to csv file
write.csv(mods.fit.df, file = "max_dist_models.csv")

# Check model behaviour
plot(mods[[2]])
qqmath(mods[[2]])

summary(mods[[2]])

# Refit with missing sex removed
mods[[1]] <- lmer(tot_dist_sqrt ~
                    TRIP_CLASS+Sex_n+Location+
                    (1|ring_number),
                  data = trips.info[!is.na(trips.info$Sex_n),])


# P values for gull model
drop1(mods[[2]], test="user", sumFun=KRSumFun)
# drop1(mods[[1]], test="user", sumFun=KRSumFun)

mod.coef <- summary(mods[[2]])$coef[, 1]
mod.ci <- confint(mods[[2]], method="Wald")
mod.par_df <- cbind.data.frame(mod.coef,mod.ci[-c(1:2),])
mod.par_df






# Duration LMMs -------
trips.info$Sex_n <- trips.info$Sex
trips.info$Sex_n[(trips.info$Sex_n) == "U"] <- NA

hist(log10(trips.info$duration_days))

trips.info$duration_days_log10 <- log10(trips.info$duration_days)

mods <- list()
mods[1] <- lmer(duration_days_log10 ~
                  TRIP_CLASS+Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[2] <- lmer(duration_days_log10 ~
                  TRIP_CLASS+Location+
                  (1|ring_number),
                data = trips.info)

# anova(mods[[1]], mods[[2]])

mods[3] <- lmer(duration_days_log10 ~
                  TRIP_CLASS+Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[4] <- lmer(duration_days_log10 ~
                  TRIP_CLASS+
                  (1|ring_number),
                data = trips.info)

mods[5] <- lmer(duration_days_log10 ~
                  Sex_n+Location+
                  (1|ring_number),
                data = trips.info)

mods[6] <- lmer(duration_days_log10 ~
                  Location+
                  (1|ring_number),
                data = trips.info)

mods[7] <- lmer(duration_days_log10 ~
                  Sex_n+
                  (1|ring_number),
                data = trips.info)

mods[8] <- lmer(duration_days_log10 ~
                  1+
                  (1|ring_number),
                data = trips.info)


# Summarise information from the models
mods.aicc <- sapply(mods, AICc)
mods.aicc.dif <- mods.aicc-min(mods.aicc)
mods.r2m <- sapply(mods, r.squaredGLMM)
t(mods.r2m)

mods.fit.df <- cbind.data.frame(c(1:length(mods)),mods.aicc,
                                mods.aicc.dif,
                                t(mods.r2m))
names(mods.fit.df) <- c("mod", "AICc", "dAICc", "R2m", "R2c")

mods.fit.df

# Save to csv file
write.csv(mods.fit.df, file = "duration_models.csv")

# Check model behaviour
plot(mods[[4]])
qqmath(mods[[4]])

summary(mods[[4]])

# Refit with missing sex removed
mods[[9]] <- lmer(duration_days_log10 ~
                    TRIP_CLASS + Sex_n+
                    (1|ring_number),
                  data = trips.info[!is.na(trips.info$Sex_n),])


# P values for gull model
drop1(mods[[9]], test="user", sumFun=KRSumFun)
# drop1(mods[[1]], test="user", sumFun=KRSumFun)

mod.coef <- summary(mods[[1]])$coef[, 1]
mod.ci <- confint(mods[[1]], method="Wald")
mod.par_df <- cbind.data.frame(mod.coef,mod.ci[-c(1:2),])
mod.par_df





# plots ----
library(ggplot2)




# 
p <- ggplot(trips.info, aes(x = TRIP_CLASS, y= duration_days))
p <- p + geom_boxplot(show.legend = FALSE)
p <- p + theme_bw() #+ scale_fill_manual(values =c("white", "grey50"))
# p <- p + ylim(c(0,800))
p <- p + labs(list(x = "Trip class",
                   y = "Trip duration (days)",
                   fill = ""))
p
p_duration <- p


# 
p <- ggplot(trips.info, aes(x = TRIP_CLASS, fill = Location, y= coldist_max/1000))
p <- p + geom_boxplot(show.legend = FALSE)
p <- p + theme_bw() + scale_fill_manual(values =c("white", "grey50"))
p <- p + ylim(c(0,800))
p <- p + labs(list(x = "Trip class",
                   y = "Maximum distance (km)",
                   fill = ""))
p_max_dist <- p



# 
p <- ggplot(trips.info, aes(x = TRIP_CLASS, fill = Location, y= tot_dist/1000))
p <- p + geom_boxplot(show.legend = FALSE)
p <- p + theme_bw() + scale_fill_manual(values =c("white", "grey50"))
p <- p + ylim(c(0,4500))
p <- p + labs(list(x = "Trip class",
                   y = "Total distance (km)",
                   fill = ""))
p
p_tot_dist <- p



# 
p <- ggplot(trips.info, aes(x = TRIP_CLASS, y= tortoisity))
p <- p + geom_boxplot(show.legend = FALSE)
p <- p + theme_bw() + scale_fill_manual(values =c("white", "grey50"))
p <- p + ylim(c(0,6.5))
p <- p + labs(list(x = "Trip class",
                   y = "Tortuosity",
                   fill = ""))
p
p_tort <- p



# Combine above 
library(cowplot)
p_all <- plot_grid(p_duration, p_max_dist, p_tot_dist, p_tort, nrow = 2, labels="AUTO",
                   hjust = - 5, vjust = 2)
p_all
save_plot(p_all, file ="stats_plots.pdf", base_width = 5, base_height = 4)
save_plot(p_all, file ="stats_plots.png", base_width = 5, base_height = 4)
save_plot(p_all, file ="stats_plots.tiff", base_width = 5, base_height = 4)
# ?save_plot




# GPS time intervals -----


trips.summary <- gps.points %>%
  group_by(trip_id) %>%
  summarise(
    # Total distance
    # tot_dist = sum(p2p_dist_m, na.rm = TRUE),
    # 
    # # Maximum distance from first location
    # max_start = max(deg.dist(first(x),
    #                          first(y),
    #                          x,
    #                          y,
    #                          km = FALSE), na.rm = TRUE),
    # # Tortoisity
    # tortoisity = tot_dist/(2*max_start)
    time_interval_med = median(time_interval[2:(n()-1)]),
    time_interval_max = max(time_interval[2:(n()-1)])
    
    
  )
median(trips.summary$time_interval_med)
mean(trips.summary$time_interval_med)
max((trips.summary$time_interval_med))



median(trips.info$tot_dist/1000)
sd(trips.info$tot_dist/1000)/sqrt(nrow(trips.info))
range(trips.info$tot_dist/1000)


median(trips.info$coldist_max)








type.summary <- trips.info %>%
  group_by(TRIP_CLASS) %>%
  summarise(
   
    tortuosity_mean = mean(tortoisity),
    tortuosity_se = sd(tortoisity)/sqrt(n()),
    
    tot_dist_mean = mean(tot_dist/1000),
    tot_dist_se = sd(tot_dist/1000)/sqrt(n()),
    
    n = n()
  )
type.summary
