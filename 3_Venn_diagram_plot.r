
# ------------------------------------------------------------------------------------------------ #

### Title: Venn diagram plot  ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(VennDiagram)

# ------------------------------------------------------------------------------------------------ #

Tem <- c("S_5", "S_10", "H_5", "H_10", "L_5", "L_10")
Nit <- c("S_5", "S_10", "H_5", "H_10", "L_5")
Sil <- c("S_5", "S_10", 	   "H_10", "L_5", "L_10")
Pho <- c("S_5", 		"H_5", 		   "L_5")
Sal <- c(									  "L_10")

v <- venn.diagram(list(Tem = Tem, Nit = Nit, Sil = Sil, Pho = Pho, Sal = Sal),
                  fill = c("orange", "blue", "Red", "Green", "yellow"),
                  alpha = c(0.5, 0.5, 0.5, 0.5, 0.5), cat.cex = 1.5, cex = 1,
                  filename = NULL)

lapply(v,  names)
lapply(v, function(i) i$label)

a <- c("S_10", "H_10")
b <- c("S_10", "H_10")
c <- c("S_5", "L_5")
d <- c("S_5", "L_5")

v[[11]]$label  <- "Temperature"
v[[12]]$label  <- "Nitrates"
v[[13]]$label  <- "Silicates"
v[[14]]$label  <- "Phosphates"
v[[15]]$label  <- "Salinity"
v[[16]]$label  <- NULL
v[[17]]$label  <- NULL
v[[18]]$label  <- NULL
v[[19]]$label  <- NULL
v[[20]]$label  <- NULL
v[[21]]$label  <- NULL
v[[22]]$label  <- NULL
v[[23]]$label  <- NULL
v[[24]]$label  <- NULL
v[[25]]$label  <- NULL
v[[26]]$label  <- NULL
v[[27]]$label  <- "L_10"
v[[28]]$label  <- NULL
v[[29]]$label  <- "H_5"
v[[30]]$label  <- NULL
v[[31]]$label  <- NULL
v[[32]]$label  <- paste(intersect(a, b), collapse = "\n") 
v[[33]]$label  <- NULL
v[[34]]$label  <- NULL
v[[35]]$label  <- NULL
v[[36]]$label  <- NULL
v[[37]]$label  <- NULL
v[[38]]$label  <- NULL
v[[39]]$label  <- NULL
v[[40]]$label  <- paste(intersect(c, d), collapse="\n") 
v[[41]]$label  <- NULL
v[[42]]$label  <- NULL 
v[[43]]$label  <- NULL
v[[44]]$label  <- NULL
v[[45]]$label  <- NULL
v[[46]]$label  <- NULL
grid.newpage()
a <- grid.draw(v)

setwd("D:/SSnks_Var_Therm/Submission/Mar_env_research")
jpeg(file = "v_4.jpeg", width = 9, height = 8, units = "in", res = 2500)
par(omi = c(0, 0, 0, 0), mgp = c(0, 0, 0), mar = c(0, 0, 0, 0) , family = "D")
grid.newpage()
grid.draw(v)
dev.off()

# ------------------------------------------------------------------------------------------------ #

### EndNotRun

