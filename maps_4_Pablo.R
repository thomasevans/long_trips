# Maps for Pablo


# Set working directory ---
# You might want to change working directory - though if you use RStudio 'Projects' option the the directory will already be set
# setwd("")

# Packages ----
# If you don't have these packages already you'll need to install them:
install.packages(c("maps","mapdata"))
# maps is just for plotting maps with appropriate projections etc
# mapdata includes a few different global base maps - which may be sufficient for your purposes

# Load packages
library(maps)
library(mapdata)


# Helper function/s ----

# A little function to add an alpha channel to colours - transparency - useful if layering
# data to show density/overlap

# Alpha channel
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}






# Plot map -------

# Save plot out to file
pdf("example_map.pdf", width = 8, height = 8,
    useDingbats = FALSE)

# Other file types you might use include png or jpeg, see help for options
?Devices

# For maps I like to have quite small margins around the plot, this sets near zero margins on the top and right side, and smallish on the left and base (where the axis lat/long labels go)
par( mar = c(1.5, 2, .5, .5))

# Set plot limits - base on what range of longitudes (x) and latitudes (y) you should like to include
xlims <- c(-10, 25)
ylims <- c(35, 68)


# Plot the base-map (we'll add some more details afterwards)
map('worldHires',
    xlim = xlims,
    ylim = ylims,
    main = "",
    lwd = 1.5,
    fill = TRUE,
    col = "grey",
    border = "grey40")

# Label lat/long axis
# Logitude axis
axis(side=(1),las=1, cex.lab = 1, cex.axis =0.7, cex = 0.5, padj = -1.5, hadj = NA)
# Latitude axis
axis(side=(2),las=1, cex.lab = 1, cex.axis =0.7, cex = 0.5, padj = 0, hadj = 0.6)

## Put a box around the plot
box(lwd=1.5)

# Add map scale bar (comment out one of these - depending what you'd like to do)
# You can control the location of this by setting the x,y location (see second example)
map.scale(ratio = FALSE,
           relwidth = 0.35, cex = 0.8)

# Specify location for scale bar
map.scale(-7, 67, ratio = FALSE,
          relwidth = 0.35, cex = 0.8)


# Add study locations:
points(c(13.1279564, 17.93, 17.972088),
       c(55.7068782, 60.63, 57.284804), pch = 23,
       col = "black", bg = addalpha("white", alpha = 0.5),
       cex = 1.5, lwd = 2)

# Label these location if desired
text(c(13.1279564, 17.93, 17.972088),
    c(55.7068782, 60.63, 57.284804),
    c("A", "C", "B"),
    pos = 4)


# Close the graphics device (if outputting this to a file)
dev.off()