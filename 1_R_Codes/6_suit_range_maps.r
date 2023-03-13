
# ------------------------------------------------------------------------------------------------ #

### Title: Suitability and range maps for sea snakes ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(ggplot2)
library(rnaturalearth)
library(raster)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Functions for suitability and range maps #

# ------------------------------------------------------------------------------------------------ #

r_suit <- function(raster_data, colors = c("white", "red")) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	p <- ggplot() + 
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
			scale_fill_gradient(low = colors[1], high = colors[2], name = "Suitability", 
            guide = guide_colorbar(override.aes = list(alpha = 1), size = 2)) +
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", 
			color = "black", size = 0.2) +
			theme_bw() +
			theme(legend.position = c(0.1, 0.2), legend.justification = c(0, 0),
			axis.title.x = element_blank(), axis.title.y = element_blank())
	return(p)
}

r_range <- function(raster_data, colors = c("white", "#525252")) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	p <- ggplot() + 
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
			scale_fill_gradient(low = colors[1], high = colors[2], name = "Range", 
            guide = guide_colorbar(override.aes = list(alpha = 1), size = 2)) +
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", 
			color = "black", size = 0.2) +
			theme_bw() +
			theme(legend.position = c(0.1, 0.2), legend.justification = c(0, 0),
			axis.title.x = element_blank(), axis.title.y = element_blank())
	return(p)
}

# ------------------------------------------------------------------------------------------------ #

# Loop for saving the maps #

# ------------------------------------------------------------------------------------------------ #

for(i in 1:49){
	setwd("D:/9_Model_medians/5m/suit")
	s_5 <- raster(list.files(pattern = ".tif")[i])
	setwd("D:/9_Model_medians/5m/Range")
	r_5 <- raster(list.files(pattern = ".tif")[i])
	setwd("D:/9_Model_medians/10m/suit")
	s_10 <- raster(list.files(pattern = ".tif")[i])
	setwd("D:/9_Model_medians/10m/Range")
	r_10 <- raster(list.files(pattern = ".tif")[i])
	
	a <- r_suit(s_5)
	b <- r_suit(s_10)
	c <- r_range(r_5)
	d <- r_range(r_10)
	
	p <- (a + b) / (c + d)
	p1 <- p + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
	setwd("D:/4_SM_Fig")
	ggsave(file = paste0(names(s_5), "_450_dpi.jpeg"), plot = p1, width = 60, height = 40, dpi = 450, units = "cm", device = "jpeg")
}

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
