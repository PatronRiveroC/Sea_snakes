
# ------------------------------------------------------------------------------------------------ #

### Title: Density plot ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(dplyr)
library(ggplot2)
library(leaflet)

# ------------------------------------------------------------------------------------------------ #

# Calculate the density by degree of Long and Lat #

# ------------------------------------------------------------------------------------------------ #

df <- read.csv("D:/6_Inputs/5_spp.csv")

df$Long_round <- round(df$Long)
df$Lat_round <- round(df$Lat)

df_count <- df %>% 
  group_by(Genus, Long_round, Lat_round) %>% 
  summarise(count = n()) %>% 
  ungroup()

colnames(df_count)[2:3] <- c("Long", "Lat")

df_count <- df_count %>% 
  arrange(Genus, Long, Lat)
df_count <- as.data.frame(df_count)
df_count$density <- log10(df_count$count)

# ------------------------------------------------------------------------------------------------ #

# Density plot #

# ------------------------------------------------------------------------------------------------ #

world_map <- map_data("world")

p <- ggplot(df_count, aes(x = Long, y = Lat)) + 
		geom_point(aes(size = count, color = Genus, alpha = 0.1)) +	
		scale_color_manual(values = c("red", "green", "blue", "orange", "purple"), name = "Genus") +
		theme_void() +    
		geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", 
			color = "black", size = 0.05) +
			coord_fixed() +
			theme_bw() +
			theme(axis.title.x = element_blank(), axis.title.y = element_blank())

p <- p + theme(plot.margin = unit(c(0,0,0,0), "cm"))

setwd("D:/4_SM_Fig")
ggsave(file = "Fig_S1.jpeg", plot = p, width = 30, height = 25, dpi = 2000, units = "cm", device = "jpeg")

# ------------------------------------------------------------------------------------------------ #

# Interactive density plot #

# ------------------------------------------------------------------------------------------------ #

pal <- colorFactor(
  palette = c("red", "green", "blue", "orange", "purple"), domain = df_count$Genus)

leaflet(df_count) %>%
  addTiles() %>%
  addCircleMarkers(~Long, ~Lat, radius = ~density * 5, stroke = FALSE,
  fillOpacity = 0.5, color = ~pal(Genus)) %>%
  addLegend(pal = pal, values = ~Genus, title = "", opacity = 0.5)

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
