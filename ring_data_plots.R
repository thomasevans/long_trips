# Mapping ring recoveries for Baltic LBBG


# Packages ----
library(maps)
library(mapdata)
source("map.scale2.R")
# library(maptools)
library(raster)

# 1. Load in recovery point data ----
ring_data <- read.csv("Figure info ringing recoveries.csv",
                      header = TRUE)

# Check structure
str(ring_data)
ring_data$date_ringed <- as.POSIXct(ring_data$FDatum, "%Y-%m-%d", tz = "UTC")
ring_data$date_recovered <- as.POSIXct(ring_data$MDatum, "%Y-%m-%d", tz = "UTC")


# Spatial extent of data
min.lat <- min(c(min(ring_data$Recovery.Lat..degN., na.rm = TRUE), min(ring_data$Ringing.Lat..deg.N. , na.rm = TRUE)))
max.lat <- max(c(max(ring_data$Recovery.Lat..degN., na.rm = TRUE), max(ring_data$Ringing.Lat..deg.N. , na.rm = TRUE)))

min.long <- min(c(min(ring_data$Recovery.Long..deg.E., na.rm = TRUE), min(ring_data$Ringing.Long..deg.E. , na.rm = TRUE)))
max.long <- max(c(max(ring_data$Recovery.Long..deg.E., na.rm = TRUE), max(ring_data$Ringing.Long..deg.E. , na.rm = TRUE)))



# points(y = c(51.133, 51.133, 66.017, 66.017), x = c(8.583, 32.500,8.583, 32.500), col = "red")
# ?points


# 2. Set-up base-map plot -----

# Detailed coastline
# Load coast-line data
load("openstreetmap_coast_polygon.RData")



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
extent.map <- c(min.long-1, max.long+1,
                min.lat-0.5, max.lat+0.5)
all_coast_baltic <- raster::crop(all_coast, raster::extent(extent.map))
# 
# ?raster::crop
# ?raster::extent

# Check how this looks
map(all_coast_baltic)

# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}




# 3. Map the data ------

# Adults only:
pdf("rings_lines_col_days365.pdf", width = 12, height = 7,
    useDingbats = FALSE)
par(mfrow=c(1,2))
par( mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
    )
# ?map
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
n_adult <- sum(ring_data$Age == "Adult")

for(i in 1:n_adult){
  #same year?
  # y1 <- format(ring_data$date_ringed[ring_data$Age == "Adult"][i], "%Y")
  # y2 <- format(ring_data$date_recovered[ring_data$Age == "Adult"][i], "%Y")
  # if(y1 == y2){
  #   col.use <- "red"
  # }else col.use <- "black"
  
  
  # >1 year (days)
  # y1 <- format(ring_data$date_ringed[ring_data$Age == "Nestling"][i], "%Y")
  # y2 <- format(ring_data$date_recovered[ring_data$Age == "Nestling"][i], "%Y")
  if(ring_data$Days[ring_data$Age == "Adult"][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[ring_data$Age == "Adult"][i],
         ring_data$Ringing.Lat..deg.N.[ring_data$Age == "Adult"][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[ring_data$Age == "Adult"][i],
           ring_data$Ringing.Lat..deg.N.[ring_data$Age == "Adult"][i],
           ring_data$Recovery.Long..deg.E.[ring_data$Age == "Adult"][i],
           ring_data$Recovery.Lat..degN.[ring_data$Age == "Adult"][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[ring_data$Age == "Adult"][i],
         ring_data$Recovery.Lat..degN.[ring_data$Age == "Adult"][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)

legend("topleft", "A", bty="n", cex = 2.5) 

# dev.off()




# Nestlings only:
# pdf("nestling_test.pdf", width = 6, height = 8,
#     useDingbats = FALSE)
par( mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)
# ?map
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
n_adult <- sum(ring_data$Age == "Nestling")

for(i in 1:n_adult){
  
  # # Difference calendar year?
  # y1 <- format(ring_data$date_ringed[ring_data$Age == "Nestling"][i], "%Y")
  # y2 <- format(ring_data$date_recovered[ring_data$Age == "Nestling"][i], "%Y")
  # if(y1 == y2){
  #   col.use <- "red"
  # }else col.use <- "black"
  
  # >1 year (days)
  # y1 <- format(ring_data$date_ringed[ring_data$Age == "Nestling"][i], "%Y")
  # y2 <- format(ring_data$date_recovered[ring_data$Age == "Nestling"][i], "%Y")
  if(ring_data$Days[ring_data$Age == "Nestling"][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[ring_data$Age == "Nestling"][i],
         ring_data$Ringing.Lat..deg.N.[ring_data$Age == "Nestling"][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[ring_data$Age == "Nestling"][i],
           ring_data$Ringing.Lat..deg.N.[ring_data$Age == "Nestling"][i],
           ring_data$Recovery.Long..deg.E.[ring_data$Age == "Nestling"][i],
           ring_data$Recovery.Lat..degN.[ring_data$Age == "Nestling"][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[ring_data$Age == "Nestling"][i],
         ring_data$Recovery.Lat..degN.[ring_data$Age == "Nestling"][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)
legend("topleft", "B)", bty="n", cex = 2) 
dev.off()






# By year period ------

# Adults only:
pdf("rings_lines_col_days365_period2000_new.pdf", width = 6, height = 6,
    useDingbats = FALSE)
par(mfrow=c(2,2))
par(mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    main = "",
    lwd = 0.5,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)


axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
f <- ring_data$Age == "Adult" & (as.numeric(format(ring_data$date_ringed, "%Y"))<2000)
n_adult <- sum(f)

for(i in 1:n_adult){

  if(ring_data$Days[f][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[f][i],
         ring_data$Ringing.Lat..deg.N.[f][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[f][i],
           ring_data$Ringing.Lat..deg.N.[f][i],
           ring_data$Recovery.Long..deg.E.[f][i],
           ring_data$Recovery.Lat..degN.[f][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[f][i],
         ring_data$Recovery.Lat..degN.[f][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)

legend("topleft", "A", bty="n", cex = 1.8) 


# Nestling early
par( mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)

axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
f <- ring_data$Age == "Nestling" & (as.numeric(format(ring_data$date_ringed, "%Y"))<2000)
n_adult <- sum(f)

for(i in 1:n_adult){
  # >1 year (days)
  if(ring_data$Days[f][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[f][i],
         ring_data$Ringing.Lat..deg.N.[f][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[f][i],
           ring_data$Ringing.Lat..deg.N.[f][i],
           ring_data$Recovery.Long..deg.E.[f][i],
           ring_data$Recovery.Lat..degN.[f][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[f][i],
         ring_data$Recovery.Lat..degN.[f][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)

legend("topleft", "B", bty="n", cex = 1.8) 


# Adult late
par( mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    main = "",
    lwd = 0.5,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)


axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
f <- ring_data$Age == "Adult" & (as.numeric(format(ring_data$date_ringed, "%Y"))>1999)
n_adult <- sum(f)

for(i in 1:n_adult){

  if(ring_data$Days[f][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[f][i],
         ring_data$Ringing.Lat..deg.N.[f][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[f][i],
           ring_data$Ringing.Lat..deg.N.[f][i],
           ring_data$Recovery.Long..deg.E.[f][i],
           ring_data$Recovery.Lat..degN.[f][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[f][i],
         ring_data$Recovery.Lat..degN.[f][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)

legend("topleft", "C", bty="n", cex = 1.8) 


# Nestling late
par( mar = c(1.5, 2, .5, .5))
map(all_coast_baltic,
    xlim = extent.map[1:2] + c(0.2,-0.2),
    ylim = extent.map[3:4] + c(0.4,-0.1),
    main = "",
    lwd = 0.5,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)

axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

# Plot adult data only first
f <- ring_data$Age == "Nestling" & (as.numeric(format(ring_data$date_ringed, "%Y"))>1999)
n_adult <- sum(f)

for(i in 1:n_adult){
 
  if(ring_data$Days[f][i]<365){
    col.use <- "red"
  }else col.use <- "black"
  
  
  # plot start location
  points(ring_data$Ringing.Long..deg.E.[f][i],
         ring_data$Ringing.Lat..deg.N.[f][i],
         pch = 1,
         col = addalpha(col.use,0.5))
  segments(ring_data$Ringing.Long..deg.E.[f][i],
           ring_data$Ringing.Lat..deg.N.[f][i],
           ring_data$Recovery.Long..deg.E.[f][i],
           ring_data$Recovery.Lat..degN.[f][i],
           col = addalpha(col.use,0.5))
  points(ring_data$Recovery.Long..deg.E.[f][i],
         ring_data$Recovery.Lat..degN.[f][i],
         pch = 0,
         col = addalpha(col.use,0.5))
  
}


## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)

legend("topleft", "D", bty="n", cex = 1.8) 


dev.off()


