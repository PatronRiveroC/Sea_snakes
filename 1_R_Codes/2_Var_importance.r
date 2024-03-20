
# ------------------------------------------------------------------------------------------------ #

### Title: Jenk high relative important variables and Chord Diagram ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain ecological ###
### 			niches and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(dplyr)
library(BAMMtools)
library(circlize)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------------------------------------------ #

# Function for calculate Jenk's breaks #

# ------------------------------------------------------------------------------------------------ #

get_ss <- function(data, col, val) {
  breaks <- getJenksBreaks(data[, col], 3)
  subset(data[, c(col, 8)], data[, val] > breaks[2]) %>%
   dplyr::mutate(taxa = "Sea_Snakes") %>%
    dplyr::rename(value = 1, var = 2, taxa = 3)
}

# ------------------------------------------------------------------------------------------------ #

# Function for groupping data #

# ------------------------------------------------------------------------------------------------ #

get_group <- function(data, col, val, group) {
  breaks <- getJenksBreaks(data[, col], 3)
  subset(data[, c(col, 8)], data[, val] > breaks[2]) %>%
    dplyr::mutate(taxa = group) %>%
    dplyr::rename(value = 1, var = 2, taxa = 3)
}

# ------------------------------------------------------------------------------------------------ #

# Estimations #

# ------------------------------------------------------------------------------------------------ #

vars <- read.csv("E:/1_Ssnks/2_Inputs/3_PC_PI_5.csv")

PC <- vars %>% subset(calc == "PC")
PC_5 <- bind_rows(
	get_ss(PC, 1, "ss"),
	get_group(PC, 2, "h", "Hydrophiinae"),
	get_group(PC, 3, "l", "Laticaudinae"),
	get_group(PC, 4, "Aip", "Aipysurus"),
	get_group(PC, 5, "Emy", "Emydocephalus"),
	get_group(PC, 6, "Hyd", "Hydrophis"),
	get_group(PC, 7, "Lat", "Laticauda"))

PI <- vars %>% subset(calc == "PI")
PI_5 <- bind_rows(
	get_ss(PI, 1, "ss"),
	get_group(PI, 2, "h", "Hydrophiinae"),
	get_group(PI, 3, "l", "Laticaudinae"),
	get_group(PI, 4, "Aip", "Aipysurus"),
	get_group(PI, 5, "Emy", "Emydocephalus"),
	get_group(PI, 6, "Hyd", "Hydrophis"),
	get_group(PI, 7, "Lat", "Laticauda"))

vars <- read.csv("E:/1_Ssnks/2_Inputs/4_PC_PI_10.csv")

PC <- vars %>% subset(calc == "PC")
PC_10 <- bind_rows(
	get_ss(PC, 1, "ss"),
	get_group(PC, 2, "h", "Hydrophiinae"),
	get_group(PC, 3, "l", "Laticaudinae"),
	get_group(PC, 4, "Aip", "Aipysurus"),
	get_group(PC, 5, "Emy", "Emydocephalus"),
	get_group(PC, 6, "Hyd", "Hydrophis"),
	get_group(PC, 7, "Lat", "Laticauda"))

PI <- vars %>% subset(calc == "PI")
PI_10 <- bind_rows(
	get_ss(PI, 1, "ss"),
	get_group(PI, 2, "h", "Hydrophiinae"),
	get_group(PI, 3, "l", "Laticaudinae"),
	get_group(PI, 4, "Aip", "Aipysurus"),
	get_group(PI, 5, "Emy", "Emydocephalus"),
	get_group(PI, 6, "Hyd", "Hydrophis"),
	get_group(PI, 7, "Lat", "Laticauda"))

# ------------------------------------------------------------------------------------------------ #

# Load data and function #

# ------------------------------------------------------------------------------------------------ #

lin <- "Sea Snakes"
subf <- c("Hydrophiinae", "Laticaudinae")
genus <- c("Aipysurus", "Emydocephalus", "Hydrophis", "Laticauda")

transform_data <- function(data, taxa) {
  data <- spread(data, var, value)
  rownames(data) <- data[[taxa]]
  rownames(data)[7] <- lin
  data[[taxa]] <- NULL
  data <- as.matrix(data)
  data[is.na(data)] <- 0
  return(data)
}

# ------------------------------------------------------------------------------------------------ #

# Apply function to all data sets #

# ------------------------------------------------------------------------------------------------ #

C_PC5 <- transform_data(PC_5, "taxa")
nm <- unique(unlist(dimnames(C_PC5)))
g <- c("Genus", "Genus", "Subfamily", "Genus", "Genus", "Subfamily", 
		"Lineage", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable")
group_PC5 <- structure(g, names = nm)

C_PI5 <- transform_data(PI_5, "taxa")
nm <- unique(unlist(dimnames(C_PI5)))
g <- c("Genus", "Genus", "Subfamily", "Genus", "Genus", "Subfamily", 
		"Lineage", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable")
group_PI5 <- structure(g, names = nm)

C_PC10 <- transform_data(PC_10, "taxa")
nm <- unique(unlist(dimnames(C_PC10)))
g <- c("Genus", "Genus", "Subfamily", "Genus", "Genus", "Subfamily", 
		"Lineage", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable")
group_PC10 <- structure(g, names = nm)

C_PI10 <- transform_data(PI_10, "taxa")
nm <- unique(unlist(dimnames(C_PI10)))
g <- c("Genus", "Genus", "Subfamily", "Genus", "Genus", "Subfamily", 
		"Lineage", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable", "Variable")
group_PI10 <- structure(g, names = nm)

# ------------------------------------------------------------------------------------------------ #

# Chord diagram function #

# ------------------------------------------------------------------------------------------------ #

circos.plot <- function(data, groups, order, main) {
  circos.par(gap.after = 6)
  chordDiagram(data, group = groups, annotationTrack = c("name", "grid"), transparency = 0.7, order = order,
               grid.col = grid.col, big.gap = 10)
  title(main = main, adj = 0, line = -3, cex.main = 2, font.main = 1.2)
  highlight.sector(lin, track.index = 1, col = "#023858", text = "Lineage", cex = 0.8, text.col = "white",
                    niceFacing = TRUE, lwd = 0.5, padding = c(0, 0, -0.2, 0))
  highlight.sector(subf, track.index = 1, col = "#66c2a4", text = "Subfamily", cex = 0.8, text.col = "white",
                    niceFacing = TRUE, lwd = 0.5, padding = c(0, 0, -0.2, 0))
  highlight.sector(genus, track.index = 1, col = "#a6bddb", text = "Genus", cex = 0.8, text.col = "white",
                    niceFacing = TRUE, lwd = 0.5, padding = c(0, 0, -0.2, 0))
  circos.clear()		
}

# ------------------------------------------------------------------------------------------------ #

# Plotting #

# ------------------------------------------------------------------------------------------------ #

setwd("E:/1_Ssnks/2_Figures") 
tiff(file = "Fig2.tiff", width = 15, height = 15, units = "cm", res = 300)
par(family = "Arial", omi = c(0, 0, 0, 0), mgp = c(0, 0, 0), mar = c(0, 0, 0, 0), cex = 1.3, ps = 8.5)
par(mfrow = c(2, 2))

nr <- c("Aipysurus", "Emydocephalus", "Hydrophiinae", "Hydrophis", "Laticauda", "Laticaudinae", 
		"Sea Snakes", "Cvel", "Cal", "Doxy", "Iro", "Nit", "Pho", "Sal", "Sil", "Tem")
col <- c("#fc4e2a", "#74a9cf", "#662506", "#006d2c", "#74c476", "#993404", "#feb24c", "#9ebcda", "#8c6bb1", 
		"grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey")
grid.col = structure(col, names = nr)


circos.plot(C_PC5, rev(group_PC5), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                 "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cal", "Doxy", "Iro", "Nit",
                                 "Pho", "Sal", "Sil", "Tem"), "A)")
circos.plot(C_PI5, rev(group_PI5), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                 "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Doxy", "Iro", "Nit", "Pho",
                                 "Sil", "Tem"), "B)")
circos.plot(C_PC10, rev(group_PC10), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                   "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cvel", "Nit", "Pho",
                                   "Sal", "Sil", "Tem"), "C)")
circos.plot(C_PI10, rev(group_PI10), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                   "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cvel", "Doxy", "Nit", "Pho",
                                   "Sil", "Tem"), "D)")
								   
dev.off()

setwd("E:/1_Ssnks/2_Figures") 
pdf(file = "Fig2.pdf", width = 10, height = 10)
par(mfrow = c(2, 2))

nr <- c("Aipysurus", "Emydocephalus", "Hydrophiinae", "Hydrophis", "Laticauda", "Laticaudinae", 
		"Sea Snakes", "Cvel", "Cal", "Doxy", "Iro", "Nit", "Pho", "Sal", "Sil", "Tem")
col <- c("#fc4e2a", "#74a9cf", "#662506", "#006d2c", "#74c476", "#993404", "#feb24c", "#9ebcda", "#8c6bb1", 
		"grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey", "grey")
grid.col = structure(col, names = nr)


circos.plot(C_PC5, rev(group_PC5), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                 "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cal", "Doxy", "Iro", "Nit",
                                 "Pho", "Sal", "Sil", "Tem"), "A)")
circos.plot(C_PI5, rev(group_PI5), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                 "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Doxy", "Iro", "Nit", "Pho",
                                 "Sil", "Tem"), "B)")
circos.plot(C_PC10, rev(group_PC10), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                   "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cvel", "Nit", "Pho",
                                   "Sal", "Sil", "Tem"), "C)")
circos.plot(C_PI10, rev(group_PI10), c("Laticauda", "Hydrophis", "Emydocephalus", "Aipysurus",
                                   "Laticaudinae", "Hydrophiinae", "Sea Snakes", "Cvel", "Doxy", "Nit", "Pho",
                                   "Sil", "Tem"), "D)")
										   
dev.off()

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
