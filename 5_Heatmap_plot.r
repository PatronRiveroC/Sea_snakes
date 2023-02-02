
# ------------------------------------------------------------------------------------------------ #

### Title: Heatmap high relative importance variables plot ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(reshape)

# ------------------------------------------------------------------------------------------------ #

data_5 <- read.csv("D:/SSnks_Var_Therm/Submission/Mar_env_research/Median_genus_5m.csv")

data1 <- melt(data_5, measure.vars = c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem"))

data1$variable <- as.factor(data1$variable)
data1$Genus <- as.factor(data1$Genus)

a <- ggplot(data1, aes(x = variable, y = factor(Genus, levels = rev(levels(factor(Genus)))), fill = value)) + 
	 geom_tile() + scale_fill_gradient(low = "white", high = "#e31a1c")
a <- a + theme_minimal()
a <- a + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
a <- a + theme(legend.position = "none")
a <- a + labs(fill = "Contribution (%)")

## 10 min

data_10 <- read.csv("D:/SSnks_Var_Therm/Submission/Mar_env_research/Median_genus_10m.csv")

data2 <- melt(data_10, measure.vars = c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem"))

data2$variable <- as.factor(data2$variable)
data2$Genus <- as.factor(data2$Genus)

b <- ggplot(data2, aes(x = variable, y = factor(Genus, levels = rev(levels(factor(Genus)))), fill = value)) + 
	 geom_tile() + scale_fill_gradient(low = "white", high = "#e31a1c")
b <- b + theme_minimal()
b <- b + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
b <- b + theme(legend.position = "none")
b <- b + theme(axis.title.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank())

plot2 <- a + b

p4 <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')

p4

setwd("D:/SSnks_Var_Therm/Submission/Mar_env_research")
ggsave(file = "Fig_Var_genus_600_dpi.pdf", plot = p4, width = 20, height = 12, dpi = 600, units = "cm", device = "pdf")
ggsave(file = "Fig_Var_genus_2000_dpi_dpi.jpeg", plot = p4, width = 20, height = 12, dpi = 2000, units = "cm", device = "jpeg")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
