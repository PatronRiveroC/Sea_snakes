
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
  # split data by Family and Genus
  subf <- split(data, data$Family)
  gen <- split(data, data$Genus)
  
  # calculate medians
  ss <- apply(data[, 4:23], 2, function(x) median(x[x > 0]))
  h <- apply(subf$Hydrophiinae[, 4:23], 2, function(x) median(x[x > 0]))
  l <- apply(subf$Laticaudinae[, 4:23], 2, function(x) median(x[x > 0]))
  Aip <- apply(gen$Aipysurus[, 4:23], 2, function(x) median(x[x > 0]))
  Emy <- apply(gen$Emydocephalus[, 4:23], 2, function(x) median(x[x > 0]))
  Hyd <- apply(gen$Hydrophis[, 4:23], 2, function(x) median(x[x > 0]))
  Lat <- apply(gen$Laticauda[, 4:23], 2, function(x) median(x[x > 0]))
  Mic <- apply(gen$Microcephalophis[, 4:23], 2, function(x) median(x[x > 0]))
  
  # create dataframe with medians
  med <- cbind(ss, h, l, Aip, Emy, Hyd, Lat, Mic)
  med[is.na(med)] <- 0
  med <- as.data.frame(med)
  med$var <- rownames(med)
  rownames(med) <- NULL
  
  # add PC or PI info
  med$calc <- "PC"
  med[11:20, "calc"] <- "PI"
  med[11:20, "var"] <- c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
  
  return(med)
}

# ------------------------------------------------------------------------------------------------ #

# Medians 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data_5 <- read.csv("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/1_raw_PC_PI_5.csv")
med_5 <- calc_median(data_5)
write.csv(med_5, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/3_PC_PI_5.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Medians 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data_10 <- read.csv("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/2_raw_PC_PI_10.csv")
med_10 <- calc_median(data_10)
write.csv(med_10, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/4_PC_PI_10.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plotting function #

# ------------------------------------------------------------------------------------------------ #

plot_df <- function(data) {
  ggplot(data = data, aes(x = var, y = factor(variable, levels = rev(levels(factor(variable)))), 
	fill = value)) + 
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "#e31a1c") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "none") +
    labs(fill = "Contribution (%)") +
    facet_grid(cols = vars(calc)) +
    scale_y_discrete(labels = c("Microcephalophis", "Laticauda", "Hydrophis", "Emydocephalus", 
                                "Aipysurus", "Laticaudinae", "Hydrophiinae", "Sea snakes lineage")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(face = "italic"))
}

# ------------------------------------------------------------------------------------------------ #

# Plotting #

# ------------------------------------------------------------------------------------------------ #

data1 <- melt(med_5, measure.vars = c("ss", "h", "l", "Aip", "Emy", "Hyd", "Lat", "Mic"))
data2 <- melt(med_10, measure.vars = c("ss", "h", "l", "Aip", "Emy", "Hyd", "Lat", "Mic"))

a <- plot_df(data1)
b <- plot_df(data2)

plot2 <- a / b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

setwd("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity")
ggsave(file = "Fig_Var_genus_600_dpi.pdf", plot = p4, width = 12, height = 12, dpi = 600, units = "cm", device = "pdf")
ggsave(file = "Fig_Var_genus_2000_dpi.jpeg", plot = p4, width = 12, height = 12, dpi = 2000, units = "cm", device = "jpeg")

# ------------------------------------------------------------------------------------------------ #

# Supplemental material #

# ------------------------------------------------------------------------------------------------ #

# 1. Plot function with data inside #

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
    scale_y_discrete(labels = c("Microcephalophis", "Laticauda", "Hydrophis", "Emydocephalus", 
                                "Aipysurus", "Laticaudinae", "Hydrophiinae", "Sea snakes lineage")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(face = "italic"))
}

# 2. Plot with data inside #

a <- plot_data(data1)
b <- plot_data(data2)

plot2 <- a / b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

setwd("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/2_Figures")
ggsave(file = "SM_X_genus_2000_dpi.jpeg", plot = p4, width = 28, height = 30, dpi = 2000, units = "cm", device = "jpeg")

# 3. Load spp data function #

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

# 4. Plot function with data inside by species #

plot_spp <- function(data) {
  ggplot(data = data, aes(x = variable, y = factor(Species, levels = rev(levels(factor(Species)))), 
	fill = value)) + 
    geom_tile() + 
    geom_text(aes(label = round(value, digits = 2))) + 
    scale_fill_gradient(low = "white", high = "#e31a1c") +
    theme_minimal() +
    theme(axis.title = element_blank(), legend.position = "none") +
    labs(fill = "Contribution (%)") +
    facet_grid(cols = vars(calc)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(face = "italic"))
}

# 5. Load and plotting data by species #

data1 <- load_data("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/1_raw_PC_PI_5.csv")
data2 <- load_data("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/2_raw_PC_PI_10.csv")

a <- plot_spp(data1)
b <- plot_spp(data2)

plot2 <- a + b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

setwd("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/4_SM_Fig")
ggsave(file = "Figure_S61.jpeg", plot = p4, width = 50, height = 30, dpi = 600, units = "cm", device = "jpeg")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
