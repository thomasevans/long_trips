# Mapping ring recoveries for Baltic LBBG


# Packages ----
library(maps)
library(mapdata)
source("map.scale2.R")
# library(maptools)
library(raster)
library(rgeos)


# Coastline data --------

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


# Combine to single spatialpolygondataframe
all_coast <- do.call(raster::bind, map.data) 

# Trim to study area (whole Baltic sea + a bit more)
extent.map <- c(2,32,50,65)

all_coast_baltic <- raster::crop(all_coast, raster::extent(extent.map))

dev.off()
# Check how this looks
map(all_coast_baltic)

# Reduce complexity of coastline
# For less complex increase value of tol
all_coast_baltic_simple <- gSimplify(all_coast_baltic, tol=0.02)
all_coast_baltic_simple <- SpatialPolygonsDataFrame(all_coast_baltic_simple,
                                                    data=all_coast_baltic@data)
# Check how this looks
map(all_coast_baltic_simple)






# Map -----



# postscript("overview_map.ps",width = 4, height = 4,
           # paper = "special", colormodel = "gray")
# ?postscript
pdf(file ="overview_map4.pdf", width = 2.5, height = 4,
    paper = "special", colormodel = "gray")

pdf(file ="track_map_combined.pdf", width = 5, height = 4,
    paper = "special", colormodel = "srgb")
# ?pdf
# ?pdf
# ?layout
layout(matrix(c(1,2), 1, 2, byrow = TRUE),
       widths=c(2.3,3.6), heights=c(1))
# layout.show(n=2)
# par(fig=c(0,0.4,0,1), new=TRUE)

# ?pdf
# par(mfrow=c(1,1))
# par( mar = c(1.5, 2, 0, 0))
# par( mar = c(0.4, 0.7, 0, 0))
par( mar = c(0.1, 0.1, 0, 0))
# par(omd = c(.15, .25, 0, 0))

map(all_coast_baltic_simple,
    xlim = c(9, 23.5),
    ylim = c(51, 64.7),
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 1,
    # bg = NA,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)
# ?map


## Scale bar and axis
box(lwd=1)
axis(side=(1),las=1, tcl=0.5, padj = -2.5, cex = 0.5,
     cex.axis = 0.7)
axis(side=(2),las=1, tcl=0.5, hadj = 0, cex = 0.5,
     cex.axis = 0.7)
# axis(side=(3),las=1, tcl=0.5, padj = 1.2, cex = 0.7)
# axis(side=(4),las=1, tcl=0.5, hadj = 0.8, cex = 0.7)

# Add map scale bar
map.scale2(y = 53.5, ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.4, cex = 0.6)
# map.scale2
legend("topleft", "(a)", bty = "n")
# ?legend
# map.scale2

# Study island location
points(17.970, 57.289,
       pch = 8, cex = 1.5
)

# dev.off()

# GPS track data plotted -------
# pdf(file ="track_map.pdf",width = 3.5, height = 4,
# paper = "special", colormodel = "srgb")
# pdf(file ="track_map_combined.pdf", width = 6, height = 4,
#     paper = "special", colormodel = "srgb")
# # ?pdf
# # ?pdf
# # ?layout
# layout(matrix(c(1,2), 1, 2, byrow = TRUE), 
#        widths=c(2,3), heights=c(1))
# par(mfrow=c(1,1))
# par( mar = c(1.5, 2, .1, .1))
# par(fig=c(0.3,1,0,1), new=FALSE)
# par( mar = c(0.5, 1, 0, 0))
par( mar = c(0.1, 0.1, 0, 0))
# par(omd = c(.15, .25, 0, 0))

library(dplyr)
# Flight IDs
flight_ids <- c(436, 564, 571, 5225,
                5285, 5917, 14477,
                16542, 16606, 20104,
                24234, 30960, 31210, 
                31321, 31599, 31861,
                32591, 38745)
# If excluding 5285
flight_ids_original <- flight_ids
flight_ids <- flight_ids[!flight_ids == 5285]


# Load in GPS tracking data
load("flight_subset_altitude_points.RData")
points_all <- as_tibble(points_all)
# Just keep the variables we need
points_sub <- dplyr::select(points_all,
                     device_info_serial:altitude,
                     speed_3d,
                     flight_id)
# Keep only the flights to be plot
points_sub <- dplyr::filter(points_sub,
                     flight_id %in% flight_ids)
# Make sure it is ordered by flight_id then by time
points_sub <- dplyr::arrange(points_sub, flight_id,
                      date_time)

xrange <- range(points_sub$longitude)
yrange <- range(points_sub$latitude)
xrange[1] <- xrange[1] - 0.3*(xrange[2]-xrange[1])
xrange[2] <- xrange[2] + 0.5*(xrange[2]-xrange[1])
yrange[1] <- yrange[1] - 0.2*(yrange[2]-yrange[1])
yrange[2] <- yrange[2] + 0.2*(yrange[2]-yrange[1])



# Trim to study area (whole Baltic sea + a bit more)
extent.map2 <- c(xrange+c(-0.1,0.1),yrange+c(-0.1,0.1))

gps_area <- raster::crop(all_coast, raster::extent(extent.map2))




# Map
# par(mfrow=c(1,1))
# par( mar = c(1.5, 2, .5, .5))
map(gps_area,
    xlim = xrange,
    ylim = yrange,
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 1,
    # bg = NA,
    fill = TRUE,
    col = "grey"
    , border = "grey40"
)
# ?map


# Colours
# cols <- rainbow(length(flight_ids))
# generated at http://tools.medialab.sciences-po.fr/iwanthue/
cols <- c("#d88f8c",
          "#4fe077",
          "#92207a",
          "#287900",
          "#7397ff",
          "#f4731a",
          "#01a1b4",
          "#b10054",
          "#cbcb5d",
          "#ff6cab",
          "#90351a",
          "#975e81")

# Shuffle (repeatably)
set.seed(1)
cols <- sample(cols, 18, replace = TRUE)
# dev.off()
# plot(1:18,1:18, col = cols, pch = 8)

# Plot GPS tracks
for(i in 1:length(flight_ids)){
  gps.sub <- filter(points_sub, flight_id == flight_ids[i])
  
  n <- nrow(gps.sub)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = cols[i], lty = 1, lwd = 1)
  
  
}

# x <- filter(points_sub, latitude <57)



## Scale bar and axis
box(lwd=1)
# axis(side=(1),las=1, tcl=0.5, padj = -1, cex = 0.5,
#      cex.axis = 0.7)
# # ?axis
# axis(side=(2),las=1, tcl=0.5, hadj = 0.7, cex = 0.5,
#      cex.axis = 0.7)
axis(side=(1),las=1, tcl=0.5, padj = -2.5, cex = 0.5,
     cex.axis = 0.7)
axis(side=(2),las=1, tcl=0.5, hadj = 0.5, cex = 0.5,
     cex.axis = 0.7)
# axis(side=(3),las=1, tcl=0.5, padj = 1.5, cex = 0.7)
# axis(side=(4),las=1, tcl=0.5, hadj = 0.8, cex = 0.7)

# Add map scale bar
map.scale2(y = 57.03, ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.4, cex = 0.6)

legend("topleft", "(b)", bty = "n")


# # Study island location
# points(17.970, 57.289,
#        pch = 8, cex = 2
# )



dev.off()


# 
pdf("flights.pdf")


for(i in 1:length(flight_ids)){

  map(gps_area,
      xlim = xrange,
      ylim = yrange,
      # col= "dark grey", bg = NA,
      # main = title.text,
      # main = "",
      main = "",
      lwd = 1,
      # bg = NA,
      fill = TRUE,
      col = "grey"
      , border = "grey40"
  )

  # i <- 10
  gps.sub <- filter(points_sub, flight_id == flight_ids[i])

  n <- nrow(gps.sub)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = cols[i], lty = 1, lwd = 1)

  box(lwd=1)
  # axis(side=(1),las=1, tcl=0.5, padj = -1, cex = 0.5,
  #      cex.axis = 0.7)
  # # ?axis
  # axis(side=(2),las=1, tcl=0.5, hadj = 0.7, cex = 0.5,
  #      cex.axis = 0.7)
  axis(side=(1),las=1, tcl=0.5, padj = -2.5, cex = 0.5,
       cex.axis = 0.7)
  axis(side=(2),las=1, tcl=0.5, hadj = 0.5, cex = 0.5,
       cex.axis = 0.7)
  # axis(side=(3),las=1, tcl=0.5, padj = 1.5, cex = 0.7)
  # axis(side=(4),las=1, tcl=0.5, hadj = 0.8, cex = 0.7)
  
  # Add map scale bar
  map.scale2(y = 56.7, ratio = FALSE, lwd.line = 2,
             relwidth = 0.4, cex = 0.6)
  
  legend("topleft", paste("Flight", flight_ids[i]),
         bty = "n")
  
  
  
}

dev.off()
# 
# # flight_ids[10]

# names(points_all)
