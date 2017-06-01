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
pdf("map_overview_razos.pdf", width = 5, height = 8,
    useDingbats = FALSE)
png("map_overview_razos.png", width = 5, height = 8,
    res = 300, units = "in")
par( mar = c(1.5, 2, .5, .5))

# Basic map
xlims <- c(10.5, 26)
ylims <- c(53.7, 67)


# 
# plot(gadm_clip, xlim = xlim,
#      ylim = ylim, col= "dark grey", bg = NA,
#      # main = title.text,
#      main = "",
#      lty = 0)

map(all_coast_baltic,
    xlim = xlims,
    ylim = ylims,
    # col= "dark grey", bg = NA,
    # main = title.text,
    # main = "",
    main = "",
    lwd = 1,
    # bg = NA,
    fill = TRUE,
    col = "grey")
# ?map
axis(side=(1),las=1, cex.lab = 1, cex.axis =1, cex = 1, padj = -1, hadj = NA)
axis(side=(2),las=1, cex.lab = 1, cex.axis =1, cex = 1, padj = 0, hadj = 0.6)

## Scale bar and axis
box(lwd=1.5)

# Add map scale bar
map.scale2(17.3, 54.2 ,ratio = FALSE, lwd.line = 1.5,
           relwidth = 0.35, cex = 1.1)


# Add colony location:
points(c(17.972088),
       c(57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

dev.off()
