# Maps to illustrate study area (key locations etc)

# Packages ----
library(maps)
library(mapdata)
source("map.scale2.R")
# library(maptools)
library(raster)

# Map data ----
# Coastline data
load("SWE_adm0.RData")

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
all_coast_baltic <- raster::crop(all_coast, raster::extent(c(10, 32, 52, 69)))

# Check how this looks
# map(all_coast_baltic)

# Trim to study area (whole Baltic sea + a bit more)
all_coast_baltic2 <- raster::crop(all_coast_baltic, raster::extent(c(11, 28, 53.5, 65)))





# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# Large overview map -------
pdf("map_overview_hi_wide.pdf", width = 5, height = 8,
    useDingbats = FALSE)

par( mar = c(1.5, 2, .5, .5))

# Basic map
xlims <- c(13, 24.5)
ylims <- c(53.8, 64.5)


# 
# plot(gadm_clip, xlim = xlim,
#      ylim = ylim, col= "dark grey", bg = NA,
#      # main = title.text,
#      main = "",
#      lty = 0)

map(all_coast_baltic2,
    xlim = xlims,
    ylim = ylims,
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey")
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)


# Add colony locations + Lund:
points(c(13.1279564, 17.93, 17.972088),
       c(55.7068782, 60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

dev.off()





# Fågelsundet map -------

# Merge multiple polygons into one - so that outline is not drawn round all the sub polygons
openstreetmap_coast_polygon@data$place <- "Sweden"
openstreetmap_coast_polygon_merged <- aggregate(openstreetmap_coast_polygon, by = "place")


pdf("map_fagelsundet_2.pdf", width = 4, height = 4,
    useDingbats = FALSE)

par( mar = c(1.5, 2, .5, .5))

# Basic map
xlims <- c(16.9, 18.7)
ylims <- c(60.2, 61.0)

# ?unionSpatialPolygons
# openstreetmap_coast_polygon$FID_new <- rep(1, length(openstreetmap_coast_polygon$FID))
# openstreetmap_merged_map <- unionSpatialPolygons(openstreetmap_coast_polygon, openstreetmap_coast_polygon$FID_new)



map(openstreetmap_coast_polygon_merged,
    xlim = xlims,
    ylim = ylims,
    # col= "dark grey", bg = NA,
    # main = title.text,
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey")
# ?map
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)


# Add colony locations:
points(c(17.93, 17.972088),
       c(60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

dev.off()





# Stora Karlsö map -------

pdf("map_stora_karlso2.pdf", width = 4, height = 4,
    useDingbats = FALSE)

par( mar = c(1.5, 2, .5, .5))

# Basic map
xlims <- c(16.2, 19)
ylims <- c(56.5, 58)

# ?unionSpatialPolygons
# openstreetmap_coast_polygon$FID_new <- rep(1, length(openstreetmap_coast_polygon$FID))
# openstreetmap_merged_map <- unionSpatialPolygons(openstreetmap_coast_polygon, openstreetmap_coast_polygon$FID_new)



map(gadm,
    xlim = xlims,
    ylim = ylims,
    # col= "dark grey", bg = NA,
    # main = title.text,
    main = "",
    lwd = 0.5,
    # bg = NA,
    fill = TRUE,
    col = "grey")
# ?map
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)


# Add colony locations:
points(c(17.93, 17.972088),
       c(60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

dev.off()





# Large overview map 2 -------
pdf("map_overview_niche_paper.pdf", width = 4, height = 8,
    useDingbats = FALSE)
png("map_overview_niche_paper.png", width = 3, height = 6,
    units= "in", res = 300)

par( mar = c(1.5, 2, .5, .5))

# Basic map
xlims <- c(-5, 28)
ylims <- c(30, 73)


# 
# plot(gadm_clip, xlim = xlim,
#      ylim = ylim, col= "dark grey", bg = NA,
#      # main = title.text,
#      main = "",
#      lty = 0)
# map('worldHires')


map('worldHires',
    xlim = xlims,
    ylim = ylims,
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 1.5,
    # bg = NA,
    fill = TRUE,
    col = "grey",
    border = "grey40")
# ?map
axis(side=(1),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = -2, hadj = NA)
axis(side=(2),las=1, cex.lab = 0.7, cex.axis =0.5, cex = 0.5, padj = 0, hadj = 0.6)

## Scale bar and axis
box(lwd=2.5)

# Add map scale bar
map.scale2(0, 72, ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 0.8)


# Add colony locations + Lund:
points(c(17.93),
       c(60.63), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 2.5, lwd = 2)

dev.off()