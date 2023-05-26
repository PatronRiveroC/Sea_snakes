
# ------------------------------------------------------------------------------------------------ #

### Title: Heatmap and summary of relative importance variables ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(ggplot2)
library(patchwork)
library(reshape)

# ------------------------------------------------------------------------------------------------ #

# Medians function #

# ------------------------------------------------------------------------------------------------ #

calc_median <- function(data) {
	subf <- split(data, data$Family)
	gen <- split(data, data$Genus)
	ss <- apply(data[, 4:23], 2, function(x) median(x[x > 0]))
	h <- apply(subf$Hydrophiinae[, 4:23], 2, function(x) median(x[x > 0]))
	l <- apply(subf$Laticaudinae[, 4:23], 2, function(x) median(x[x > 0]))
	Aip <- apply(gen$Aipysurus[, 4:23], 2, function(x) median(x[x > 0]))
	Emy <- apply(gen$Emydocephalus[, 4:23], 2, function(x) median(x[x > 0]))
	Eph <- apply(gen$Ephalophis[, 4:23], 2, function(x) median(x[x > 0]))
	Hyd <- apply(gen$Hydrophis[, 4:23], 2, function(x) median(x[x > 0]))
	Lat <- apply(gen$Laticauda[, 4:23], 2, function(x) median(x[x > 0]))
	Mic <- apply(gen$Microcephalophis[, 4:23], 2, function(x) median(x[x > 0]))
	med <- cbind(ss, h, l, Aip, Emy, Eph, Hyd, Lat, Mic)
	med[is.na(med)] <- 0
	med <- as.data.frame(med)
	med$var <- rownames(med)
	rownames(med) <- NULL
	med$calc <- "PC"
	med[11:20, "calc"] <- "PI"
	med[11:20, "var"] <- c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
	return(med)
}

# ------------------------------------------------------------------------------------------------ #

# Medians 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data_5 <- read.csv("D:/1_Ssnks/2_Inputs/1_raw_PC_PI_5.csv")
med_5 <- calc_median(data_5)
write.csv(med_5, "D:/2_Inputs/3_PC_PI_5.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Medians 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data_10 <- read.csv("D:/1_Ssnks/2_Inputs/2_raw_PC_PI_10.csv")
med_10 <- calc_median(data_10)
write.csv(med_10, "D:/2_Inputs/4_PC_PI_10.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plotting function #

# ------------------------------------------------------------------------------------------------ #

plot_data <- function(data) {
  ggplot(data = data, aes(x = var, y = factor(variable, levels = rev(levels(factor(variable)))), 
	fill = value)) + 
    geom_tile() + 
    geom_text(aes(label = round(value, digits = 2))) + 
    scale_fill_gradient(low = "white", high = "#e31a1c") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "none") +
    labs(fill = "Contribution (%)") +
    facet_grid(cols = vars(calc)) +
    scale_y_discrete(labels = c("Microcephalophis", "Laticauda", "Hydrophis", "Ephalophis", "Emydocephalus",
                                "Aipysurus", "Laticaudinae", "Hydrophiinae", "Sea snakes lineage")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(face = "italic"))
}

# ------------------------------------------------------------------------------------------------ #

# Plotting #

# ------------------------------------------------------------------------------------------------ #

data1 <- melt(med_5, measure.vars = c("ss", "h", "l", "Aip", "Emy", "Eph", "Hyd", "Lat", "Mic"))
data2 <- melt(med_10, measure.vars = c("ss", "h", "l", "Aip", "Emy", "Eph", "Hyd", "Lat", "Mic"))

a <- plot_data(data1)
b <- plot_data(data2)

plot2 <- a / b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

setwd("D:/1_Ssnks/2_Figures")
ggsave(file = "Fig1_var_import_600_dpi.tiff", plot = p4, width = 25, height = 25, dpi = 600, units = "cm", device = "tiff")
ggsave(file = "Fig_1.pdf", plot = p4, width = 25, height = 25, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

# Supplemental material #

# ------------------------------------------------------------------------------------------------ #

# 1. Load spp data function #

load_data <- function(filepath) {
  data <- read.csv(filepath)
  data <- data[, 3:23]
  PC <- data[, 1:11]
  PC$calc <- "PC"
  PI <- data[, c(1, 12:21)]
  colnames(PI) <- c("Species", "Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
  PI$calc <- "PI"
  med <- rbind(PC, PI)
  data_melted <- melt(med, measure.vars = c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem"))
  return(data_melted)
}

# 2. Plot function with data inside by species #

plot_spp <- function(data) {
  ggplot(data = data, aes(x = variable, y = factor(Species, levels = rev(levels(factor(Species)))), 
	fill = value)) + 
    geom_tile() + 
    geom_text(aes(label = round(value, digits = 2)), size = 1.8) + 
    scale_fill_gradient(low = "white", high = "#e31a1c") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "none") +
    labs(fill = "Contribution (%)") +
    facet_grid(cols = vars(calc)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(face = "italic"))
}

# 3. Load and plotting data by species #

data1 <- load_data("D:/1_Ssnks/2_Inputs/1_raw_PC_PI_5.csv")
data2 <- load_data("D:/1_Ssnks/2_Inputs/2_raw_PC_PI_10.csv")

a <- plot_spp(data1)
b <- plot_spp(data2)

plot2 <- a + b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

setwd("D:/1_Ssnks/4_SM_Fig")
ggsave(file = "Fig_S62.jpeg", plot = p4, width = 25, height = 18, dpi = 600, units = "cm", device = "jpeg")
ggsave(file = "Fig_S62.pdf", plot = p4, width = 25, height = 18, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
