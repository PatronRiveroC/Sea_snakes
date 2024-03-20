
# ------------------------------------------------------------------------------------------------ #

### Title: Density plot ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain ecological ###
### 			niches and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(dplyr)
library(leaflet)
library(htmlwidgets)
library(pandoc)

# ------------------------------------------------------------------------------------------------ #

# Calculate the density by degree of Long and Lat #

# ------------------------------------------------------------------------------------------------ #

df <- read.csv("E:/1_Ssnks/2_Inputs/5_spp.csv")

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

pal <- colorFactor(
  palette = c("#4292c6", "#e31a1c", "#fed976", "#f768a1"), domain = df_count$Genus)

map <- leaflet(df_count) %>%
		addTiles() %>%
			addCircleMarkers(~Long, ~Lat, radius = ~density * 5, stroke = FALSE,
			fillOpacity = 0.5, color = ~pal(Genus)) %>%
				addLegend(pal = pal, values = ~Genus, title = "", opacity = 0.5)

map
saveWidget(map, file = "E:/1_Ssnks/2_Inputs/genus_map.html")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
