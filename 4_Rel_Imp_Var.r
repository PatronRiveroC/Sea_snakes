
# ------------------------------------------------------------------------------------------------ #

### Title: High relative importance variables by taxonomic level ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(circlize)
library(ggplot2)

# ------------------------------------------------------------------------------------------------ #

varCD <- read.csv("D:/SSnks_Var_Therm/Submission/Mar_env_research/copia.csv")

rownames(varCD) <- varCD$Group
varCD$Group <- NULL
varCD1 <- as.matrix(varCD)

grid.col = c(L_10 = "#045a8d", L_5 = "#74a9cf", H_10 = "#006d2c", H_5 = "#74c476", S_10 = "#993404", S_5 = "#feb24c", 
			 Nit= "grey", Pho= "grey", Sal= "grey", Sil= "grey", Tem= "grey")

setwd("D:/SSnks_Var_Therm/Submission/Mar_env_research") 
jpeg(file = "Fig_1_Var_Imp_Tax_2500_dpi.jpeg", width = 9, height = 8, units = "in", res = 2500)
par(omi = c(0, 0, 0, 0), mgp = c(0, 0, 0), mar = c(0, 0, 0, 0), cex = 1.3)
			 
circos.par(gap.after = 6)
chordDiagram(varCD1, annotationTrack = c("name", "grid"), transparency = 0.7, order = c("L_10", "L_5", "H_10", "H_5", 
			 "S_10", "S_5", "Nit", "Pho", "Sal", "Sil", "Tem"), grid.col = grid.col)
			 
dev.off()

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
