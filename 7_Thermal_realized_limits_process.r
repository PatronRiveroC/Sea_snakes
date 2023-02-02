
# ------------------------------------------------------------------------------------------------ #

### Title: Thermal realized limits process and plot  ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(dismo)
library(tidyverse)
library(reshape)

# ------------------------------------------------------------------------------------------------ #

# Data treatment 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Bma_Max <- raster("F:/Sea_Snake/Var_Temp/BMa_Tem_Max.tif")
Bma_Mean <- raster("F:/Sea_Snake/Var_Temp/BMa_Tem_Mean.tif")
Bma_Min <- raster("F:/Sea_Snake/Var_Temp/BMa_Tem_Min.tif")
Bme_Max <- raster("F:/Sea_Snake/Var_Temp/Bme_Tem_Max.tif")
Bme_Mean <- raster("F:/Sea_Snake/Var_Temp/Bme_Tem_Mean.tif")
Bme_Min <- raster("F:/Sea_Snake/Var_Temp/Bme_Tem_Min.tif")
Bmi_Max <- raster("F:/Sea_Snake/Var_Temp/Bmi_Tem_Max.tif")
Bmi_Mean <- raster("F:/Sea_Snake/Var_Temp/Bmi_Tem_Mean.tif")
Bmi_Min <- raster("F:/Sea_Snake/Var_Temp/Bmi_Tem_Min.tif")
S_Max <- raster("F:/Sea_Snake/Var_Temp/S_Tem_Max.tif")
S_Mean <- raster("F:/Sea_Snake/Var_Temp/S_Tem_Mean.tif")
S_Min <- raster("F:/Sea_Snake/Var_Temp/S_Tem_Min.tif")
M_Max <- raster("F:/Sea_Snake/Var_Temp/M_Tem_Max.tif")
M_Mean <- raster("F:/Sea_Snake/Var_Temp/M_Tem_Mean.tif")
M_Min <- raster("F:/Sea_Snake/Var_Temp/M_Tem_Min.tif")

setwd("F:/Sea_Snake/Occ/3_GridSample")
base <- list.files(pattern=".csv") %>% map_df(~read_csv(.))
cord <- base[,c("Long", "Lat")] 
cord <- as.data.frame(cord)

# ------------------------------------------------------------------------------------------------ #

# Extract values to points #

# ------------------------------------------------------------------------------------------------ #

Tem <- raster::extract(Bma_Max, cord)
Bma_Max <- cbind(base,Tem)
Bma_Max <- as.data.frame(Bma_Max)

Tem <- raster::extract(Bma_Mean, cord)
Bma_Mean <- cbind(base,Tem)
Bma_Mean <- as.data.frame(Bma_Mean)

Tem <- raster::extract(Bma_Min, cord)
Bma_Min <- cbind(base,Tem)
Bma_Min <- as.data.frame(Bma_Min)

Tem <- raster::extract(Bme_Max, cord)
Bme_Max <- cbind(base,Tem)
Bme_Max <- as.data.frame(Bme_Max)

Tem <- raster::extract(Bme_Mean, cord)
Bme_Mean <- cbind(base,Tem)
Bme_Mean <- as.data.frame(Bme_Mean)

Tem <- raster::extract(Bme_Min, cord)
Bme_Min <- cbind(base,Tem)
Bme_Min <- as.data.frame(Bme_Min)

Tem <- raster::extract(Bmi_Max, cord)
Bmi_Max <- cbind(base,Tem)
Bmi_Max <- as.data.frame(Bmi_Max)

Tem <- raster::extract(Bmi_Mean, cord)
Bmi_Mean <- cbind(base,Tem)
Bmi_Mean <- as.data.frame(Bmi_Mean)

Tem <- raster::extract(Bmi_Min, cord)
Bmi_Min <- cbind(base,Tem)
Bmi_Min <- as.data.frame(Bmi_Min)

Tem <- raster::extract(S_Max, cord)
S_Max <- cbind(base,Tem)
S_Max <- as.data.frame(S_Max)

Tem <- raster::extract(S_Mean, cord)
S_Mean <- cbind(base,Tem)
S_Mean <- as.data.frame(S_Mean)

Tem <- raster::extract(S_Min, cord)
S_Min <- cbind(base,Tem)
S_Min <- as.data.frame(S_Min)

Tem <- raster::extract(M_Max, cord)
M_Max <- cbind(base,Tem)
M_Max <- as.data.frame(M_Max)

Tem <- raster::extract(M_Mean, cord)
M_Mean <- cbind(base,Tem)
M_Mean <- as.data.frame(M_Mean)

Tem <- raster::extract(M_Min, cord)
M_Min <- cbind(base,Tem)
M_Min <- as.data.frame(M_Min)

# ------------------------------------------------------------------------------------------------ #

# Estimate max and min realized limits by max, mean and min temperature #

# ------------------------------------------------------------------------------------------------ #

# Max #

Bma_Max_low <- aggregate(Tem ~ Especie, Bma_Max, function(x) min(x))
lower_5 <- Bma_Max_low[,1:2]
Bma_Max_upp <- aggregate(Tem ~ Especie, Bma_Max, function(x) max(x))
upper_5 <- Bma_Max_upp[,2]
Max1 <- cbind(lower_5,upper_5)
Max1 <- as.data.frame(Max1)
Max1$Set <- "Bma"

Bme_Max_low <- aggregate(Tem ~ Especie, Bme_Max, function(x) min(x))
lower_5 <- Bme_Max_low[,1:2]
Bme_Max_upp <- aggregate(Tem ~ Especie, Bme_Max, function(x) max(x))
upper_5 <- Bme_Max_upp[,2]
Max2 <- cbind(lower_5,upper_5)
Max2 <- as.data.frame(Max2)
Max2$Set <- "Bme"

Bmi_Max_low <- aggregate(Tem ~ Especie, Bmi_Max, function(x) min(x))
lower_5 <- Bmi_Max_low[,1:2]
Bmi_Max_upp <- aggregate(Tem ~ Especie, Bmi_Max, function(x) max(x))
upper_5 <- Bmi_Max_upp[,2]
Max3 <- cbind(lower_5,upper_5)
Max3 <- as.data.frame(Max3)
Max3$Set <- "Bmi"

S_Max_low <- aggregate(Tem ~ Especie, S_Max, function(x) min(x))
lower_5 <- S_Max_low[,1:2]
S_Max_upp <- aggregate(Tem ~ Especie, S_Max, function(x) max(x))
upper_5 <- S_Max_upp[,2]
Max4 <- cbind(lower_5,upper_5)
Max4 <- as.data.frame(Max4)
Max4$Set <- "Bsurf"

M_Max$Tem <- M_Max$Tem/100
M_Max_low <- aggregate(Tem ~ Especie, M_Max, function(x) min(x))
lower_5 <- M_Max_low[,1:2]
M_Max_upp <- aggregate(Tem ~ Especie, M_Max, function(x) max(x))
upper_5 <- M_Max_upp[,2]
Max5 <- cbind(lower_5,upper_5)
Max5 <- as.data.frame(Max5)
Max5$Set <- "Msurf"

Set_Max <- rbind(Max1, Max2, Max3, Max4, Max5)
names(Set_Max)[names(Set_Max) == "Tem"] <- "lower_5"

# Mean #

Bma_Mean_low <- aggregate(Tem ~ Especie, Bma_Mean, function(x) min(x))
lower_5 <- Bma_Mean_low[,1:2]
Bma_Mean_upp <- aggregate(Tem ~ Especie, Bma_Mean, function(x) max(x))
upper_5 <- Bma_Mean_upp[,2]
Mean1 <- cbind(lower_5,upper_5)
Mean1 <- as.data.frame(Mean1)
Mean1$Set <- "Bma"

Bme_Mean_low <- aggregate(Tem ~ Especie, Bme_Mean, function(x) min(x))
lower_5 <- Bme_Mean_low[,1:2]
Bme_Mean_upp <- aggregate(Tem ~ Especie, Bme_Mean, function(x) max(x))
upper_5 <- Bme_Mean_upp[,2]
Mean2 <- cbind(lower_5,upper_5)
Mean2 <- as.data.frame(Mean2)
Mean2$Set <- "Bme"

Bmi_Mean_low <- aggregate(Tem ~ Especie, Bmi_Mean, function(x) min(x))
lower_5 <- Bmi_Mean_low[,1:2]
Bmi_Mean_upp <- aggregate(Tem ~ Especie, Bmi_Mean, function(x) max(x))
upper_5 <- Bmi_Mean_upp[,2]
Mean3 <- cbind(lower_5,upper_5)
Mean3 <- as.data.frame(Mean3)
Mean3$Set <- "Bmi"

S_Mean_low <- aggregate(Tem ~ Especie, S_Mean, function(x) min(x))
lower_5 <- S_Mean_low[,1:2]
S_Mean_upp <- aggregate(Tem ~ Especie, S_Mean, function(x) max(x))
upper_5 <- S_Mean_upp[,2]
Mean4 <- cbind(lower_5,upper_5)
Mean4 <- as.data.frame(Mean4)
Mean4$Set <- "Bsurf"

M_Mean$Tem <- M_Mean$Tem/100
M_Mean_low <- aggregate(Tem ~ Especie, M_Mean, function(x) min(x))
lower_5 <- M_Mean_low[,1:2]
M_Mean_upp <- aggregate(Tem ~ Especie, M_Mean, function(x) max(x))
upper_5 <- M_Mean_upp[,2]
Mean5 <- cbind(lower_5,upper_5)
Mean5 <- as.data.frame(Mean5)
Mean5$Set <- "Msurf"

Set_Mean <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)
names(Set_Mean)[names(Set_Mean) == "Tem"] <- "lower_5"

# Min# 

Bma_Min_low <- aggregate(Tem ~ Especie, Bma_Min, function(x) min(x))
lower_5 <- Bma_Min_low[,1:2]
Bma_Min_upp <- aggregate(Tem ~ Especie, Bma_Min, function(x) max(x))
upper_5 <- Bma_Min_upp[,2]
Min1 <- cbind(lower_5,upper_5)
Min1 <- as.data.frame(Min1)
Min1$Set <- "Bma"

Bme_Min_low <- aggregate(Tem ~ Especie, Bme_Min, function(x) min(x))
lower_5 <- Bme_Min_low[,1:2]
Bme_Min_upp <- aggregate(Tem ~ Especie, Bme_Min, function(x) max(x))
upper_5 <- Bme_Min_upp[,2]
Min2 <- cbind(lower_5,upper_5)
Min2 <- as.data.frame(Min2)
Min2$Set <- "Bme"

Bmi_Min_low <- aggregate(Tem ~ Especie, Bmi_Min, function(x) min(x))
lower_5 <- Bmi_Min_low[,1:2]
Bmi_Min_upp <- aggregate(Tem ~ Especie, Bmi_Min, function(x) max(x))
upper_5 <- Bmi_Min_upp[,2]
Min3 <- cbind(lower_5,upper_5)
Min3 <- as.data.frame(Min3)
Min3$Set <- "Bmi"

S_Min_low <- aggregate(Tem ~ Especie, S_Min, function(x) min(x))
lower_5 <- S_Min_low[,1:2]
S_Min_upp <- aggregate(Tem ~ Especie, S_Min, function(x) max(x))
upper_5 <- S_Min_upp[,2]
Min4 <- cbind(lower_5,upper_5)
Min4 <- as.data.frame(Min4)
Min4$Set <- "Bsurf"

M_Min$Tem <- M_Min$Tem/100
M_Min_low <- aggregate(Tem ~ Especie, M_Min, function(x) min(x))
lower_5 <- M_Min_low[,1:2]
M_Min_upp <- aggregate(Tem ~ Especie, M_Min, function(x) max(x))
upper_5 <- M_Min_upp[,2]
Min5 <- cbind(lower_5,upper_5)
Min5 <- as.data.frame(Min5)
Min5$Set <- "Msurf"

Set_Min <- rbind(Min1, Min2, Min3, Min4, Min5)
names(Set_Min)[names(Set_Min) == "Tem"] <- "lower_5"

# ------------------------------------------------------------------------------------------------ #

# Plot #

# ------------------------------------------------------------------------------------------------ #

Max <- melt(Set_Max,measure.vars = c("lower_5","upper_5"))
Min <- melt(Set_Min,measure.vars = c("lower_5","upper_5"))
Mean <- melt(Set_Mean,measure.vars = c("lower_5","upper_5"))

Max$Temperature <- "Max"
Min$Temperature <- "Min"
Mean$Temperature <- "Mean"

All_5 <- rbind(Max, Mean, Min)

levels(All_5$variable) <- gsub("lower_5","Lower", levels(All_5$variable))
levels(All_5$variable) <- gsub("upper_5","Upper", levels(All_5$variable))

x1 <- ggplot(All_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")

p1 <- x1 + facet_grid(Temperature~variable)

# ------------------------------------------------------------------------------------------------ #

# Same process at 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Bma_Max <- raster("F:/Sea_Snake/Var_Temp/10m/BMa_Tem_Max.tif")
Bma_Mean <- raster("F:/Sea_Snake/Var_Temp/10m/BMa_Tem_Mean.tif")
Bma_Min <- raster("F:/Sea_Snake/Var_Temp/10m/BMa_Tem_Min.tif")
Bme_Max <- raster("F:/Sea_Snake/Var_Temp/10m/Bme_Tem_Max.tif")
Bme_Mean <- raster("F:/Sea_Snake/Var_Temp/10m/Bme_Tem_Mean.tif")
Bme_Min <- raster("F:/Sea_Snake/Var_Temp/10m/Bme_Tem_Min.tif")
Bmi_Max <- raster("F:/Sea_Snake/Var_Temp/10m/Bmi_Tem_Max.tif")
Bmi_Mean <- raster("F:/Sea_Snake/Var_Temp/10m/Bmi_Tem_Mean.tif")
Bmi_Min <- raster("F:/Sea_Snake/Var_Temp/10m/Bmi_Tem_Min.tif")
S_Max <- raster("F:/Sea_Snake/Var_Temp/10m/S_Tem_Max.tif")
S_Mean <- raster("F:/Sea_Snake/Var_Temp/10m/S_Tem_Mean.tif")
S_Min <- raster("F:/Sea_Snake/Var_Temp/10m/S_Tem_Min.tif")
M_Max <- raster("F:/Sea_Snake/Var_Temp/10m/M_Tem_Max.tif")
M_Mean <- raster("F:/Sea_Snake/Var_Temp/10m/M_Tem_Mean.tif")
M_Min <- raster("F:/Sea_Snake/Var_Temp/10m/M_Tem_Min.tif")

setwd("F:/Sea_Snake/Occ/3_GridSample")
base <- list.files(pattern=".csv") %>% map_df(~read_csv(.))
cord <- base[,c("Long", "Lat")] 
cord <- as.data.frame(cord)

Tem <- raster::extract(Bma_Max, cord)
Bma_Max <- cbind(base,Tem)
Bma_Max <- as.data.frame(Bma_Max)

Tem <- raster::extract(Bma_Mean, cord)
Bma_Mean <- cbind(base,Tem)
Bma_Mean <- as.data.frame(Bma_Mean)

Tem <- raster::extract(Bma_Min, cord)
Bma_Min <- cbind(base,Tem)
Bma_Min <- as.data.frame(Bma_Min)

Tem <- raster::extract(Bme_Max, cord)
Bme_Max <- cbind(base,Tem)
Bme_Max <- as.data.frame(Bme_Max)

Tem <- raster::extract(Bme_Mean, cord)
Bme_Mean <- cbind(base,Tem)
Bme_Mean <- as.data.frame(Bme_Mean)

Tem <- raster::extract(Bme_Min, cord)
Bme_Min <- cbind(base,Tem)
Bme_Min <- as.data.frame(Bme_Min)

Tem <- raster::extract(Bmi_Max, cord)
Bmi_Max <- cbind(base,Tem)
Bmi_Max <- as.data.frame(Bmi_Max)

Tem <- raster::extract(Bmi_Mean, cord)
Bmi_Mean <- cbind(base,Tem)
Bmi_Mean <- as.data.frame(Bmi_Mean)

Tem <- raster::extract(Bmi_Min, cord)
Bmi_Min <- cbind(base,Tem)
Bmi_Min <- as.data.frame(Bmi_Min)

Tem <- raster::extract(S_Max, cord)
S_Max <- cbind(base,Tem)
S_Max <- as.data.frame(S_Max)

Tem <- raster::extract(S_Mean, cord)
S_Mean <- cbind(base,Tem)
S_Mean <- as.data.frame(S_Mean)

Tem <- raster::extract(S_Min, cord)
S_Min <- cbind(base,Tem)
S_Min <- as.data.frame(S_Min)

Tem <- raster::extract(M_Max, cord)
M_Max <- cbind(base,Tem)
M_Max <- as.data.frame(M_Max)

Tem <- raster::extract(M_Mean, cord)
M_Mean <- cbind(base,Tem)
M_Mean <- as.data.frame(M_Mean)

Tem <- raster::extract(M_Min, cord)
M_Min <- cbind(base,Tem)
M_Min <- as.data.frame(M_Min)

Bma_Max_low <- aggregate(Tem ~ Especie, Bma_Max, function(x) min(x))
lower_5 <- Bma_Max_low[,1:2]
Bma_Max_upp <- aggregate(Tem ~ Especie, Bma_Max, function(x) max(x))
upper_5 <- Bma_Max_upp[,2]
Max1 <- cbind(lower_5,upper_5)
Max1 <- as.data.frame(Max1)
Max1$Set <- "Bma"

Bme_Max_low <- aggregate(Tem ~ Especie, Bme_Max, function(x) min(x))
lower_5 <- Bme_Max_low[,1:2]
Bme_Max_upp <- aggregate(Tem ~ Especie, Bme_Max, function(x) max(x))
upper_5 <- Bme_Max_upp[,2]
Max2 <- cbind(lower_5,upper_5)
Max2 <- as.data.frame(Max2)
Max2$Set <- "Bme"

Bmi_Max_low <- aggregate(Tem ~ Especie, Bmi_Max, function(x) min(x))
lower_5 <- Bmi_Max_low[,1:2]
Bmi_Max_upp <- aggregate(Tem ~ Especie, Bmi_Max, function(x) max(x))
upper_5 <- Bmi_Max_upp[,2]
Max3 <- cbind(lower_5,upper_5)
Max3 <- as.data.frame(Max3)
Max3$Set <- "Bmi"

S_Max_low <- aggregate(Tem ~ Especie, S_Max, function(x) min(x))
lower_5 <- S_Max_low[,1:2]
S_Max_upp <- aggregate(Tem ~ Especie, S_Max, function(x) max(x))
upper_5 <- S_Max_upp[,2]
Max4 <- cbind(lower_5,upper_5)
Max4 <- as.data.frame(Max4)
Max4$Set <- "Bsurf"

M_Max$Tem <- M_Max$Tem/100
M_Max_low <- aggregate(Tem ~ Especie, M_Max, function(x) min(x))
lower_5 <- M_Max_low[,1:2]
M_Max_upp <- aggregate(Tem ~ Especie, M_Max, function(x) max(x))
upper_5 <- M_Max_upp[,2]
Max5 <- cbind(lower_5,upper_5)
Max5 <- as.data.frame(Max5)
Max5$Set <- "Msurf"

Set_Max <- rbind(Max1, Max2, Max3, Max4, Max5)
names(Set_Max)[names(Set_Max) == "Tem"] <- "lower_5"

Bma_Mean_low <- aggregate(Tem ~ Especie, Bma_Mean, function(x) min(x))
lower_5 <- Bma_Mean_low[,1:2]
Bma_Mean_upp <- aggregate(Tem ~ Especie, Bma_Mean, function(x) max(x))
upper_5 <- Bma_Mean_upp[,2]
Mean1 <- cbind(lower_5,upper_5)
Mean1 <- as.data.frame(Mean1)
Mean1$Set <- "Bma"

Bme_Mean_low <- aggregate(Tem ~ Especie, Bme_Mean, function(x) min(x))
lower_5 <- Bme_Mean_low[,1:2]
Bme_Mean_upp <- aggregate(Tem ~ Especie, Bme_Mean, function(x) max(x))
upper_5 <- Bme_Mean_upp[,2]
Mean2 <- cbind(lower_5,upper_5)
Mean2 <- as.data.frame(Mean2)
Mean2$Set <- "Bme"

Bmi_Mean_low <- aggregate(Tem ~ Especie, Bmi_Mean, function(x) min(x))
lower_5 <- Bmi_Mean_low[,1:2]
Bmi_Mean_upp <- aggregate(Tem ~ Especie, Bmi_Mean, function(x) max(x))
upper_5 <- Bmi_Mean_upp[,2]
Mean3 <- cbind(lower_5,upper_5)
Mean3 <- as.data.frame(Mean3)
Mean3$Set <- "Bmi"

S_Mean_low <- aggregate(Tem ~ Especie, S_Mean, function(x) min(x))
lower_5 <- S_Mean_low[,1:2]
S_Mean_upp <- aggregate(Tem ~ Especie, S_Mean, function(x) max(x))
upper_5 <- S_Mean_upp[,2]
Mean4 <- cbind(lower_5,upper_5)
Mean4 <- as.data.frame(Mean4)
Mean4$Set <- "Bsurf"

M_Mean$Tem <- M_Mean$Tem/100
M_Mean_low <- aggregate(Tem ~ Especie, M_Mean, function(x) min(x))
lower_5 <- M_Mean_low[,1:2]
M_Mean_upp <- aggregate(Tem ~ Especie, M_Mean, function(x) max(x))
upper_5 <- M_Mean_upp[,2]
Mean5 <- cbind(lower_5,upper_5)
Mean5 <- as.data.frame(Mean5)
Mean5$Set <- "Msurf"

Set_Mean <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)
names(Set_Mean)[names(Set_Mean) == "Tem"] <- "lower_5"

Bma_Min_low <- aggregate(Tem ~ Especie, Bma_Min, function(x) min(x))
lower_5 <- Bma_Min_low[,1:2]
Bma_Min_upp <- aggregate(Tem ~ Especie, Bma_Min, function(x) max(x))
upper_5 <- Bma_Min_upp[,2]
Min1 <- cbind(lower_5,upper_5)
Min1 <- as.data.frame(Min1)
Min1$Set <- "Bma"

Bme_Min_low <- aggregate(Tem ~ Especie, Bme_Min, function(x) min(x))
lower_5 <- Bme_Min_low[,1:2]
Bme_Min_upp <- aggregate(Tem ~ Especie, Bme_Min, function(x) max(x))
upper_5 <- Bme_Min_upp[,2]
Min2 <- cbind(lower_5,upper_5)
Min2 <- as.data.frame(Min2)
Min2$Set <- "Bme"

Bmi_Min_low <- aggregate(Tem ~ Especie, Bmi_Min, function(x) min(x))
lower_5 <- Bmi_Min_low[,1:2]
Bmi_Min_upp <- aggregate(Tem ~ Especie, Bmi_Min, function(x) max(x))
upper_5 <- Bmi_Min_upp[,2]
Min3 <- cbind(lower_5,upper_5)
Min3 <- as.data.frame(Min3)
Min3$Set <- "Bmi"

S_Min_low <- aggregate(Tem ~ Especie, S_Min, function(x) min(x))
lower_5 <- S_Min_low[,1:2]
S_Min_upp <- aggregate(Tem ~ Especie, S_Min, function(x) max(x))
upper_5 <- S_Min_upp[,2]
Min4 <- cbind(lower_5,upper_5)
Min4 <- as.data.frame(Min4)
Min4$Set <- "Bsurf"

M_Min$Tem <- M_Min$Tem/100
M_Min_low <- aggregate(Tem ~ Especie, M_Min, function(x) min(x))
lower_5 <- M_Min_low[,1:2]
M_Min_upp <- aggregate(Tem ~ Especie, M_Min, function(x) max(x))
upper_5 <- M_Min_upp[,2]
Min5 <- cbind(lower_5,upper_5)
Min5 <- as.data.frame(Min5)
Min5$Set <- "Msurf"

Set_Min <- rbind(Min1, Min2, Min3, Min4, Min5)
names(Set_Min)[names(Set_Min) == "Tem"] <- "lower_5"

Max <- melt(Set_Max,measure.vars = c("lower_5","upper_5"))
Min <- melt(Set_Min,measure.vars = c("lower_5","upper_5"))
Mean <- melt(Set_Mean,measure.vars = c("lower_5","upper_5"))

Max$Temperature <- "Max"
Min$Temperature <- "Min"
Mean$Temperature <- "Mean"

All_10 <- rbind(Max, Mean, Min)
levels(All_10$variable) <- gsub("lower_5","Lower", levels(All_10$variable))
levels(All_10$variable) <- gsub("upper_5","Upper", levels(All_10$variable))

x1 <- ggplot(All_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")


p2 <- x1 + facet_grid(Temperature~variable)

# ------------------------------------------------------------------------------------------------ #

# Final Plot #

# ------------------------------------------------------------------------------------------------ #

plot2 <- p1 + p2

Sea_Snakes <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')

# ------------------------------------------------------------------------------------------------ #

# Subfamily level #

# ------------------------------------------------------------------------------------------------ #

data1 <- All_5
data2 <- split(data1, data1$Especie)

Hyd_5 <- rbind(data2$Aca_per,data2$Aip_apr,data2$Aip_dub,data2$Aip_eyd,data2$Aip_fol,data2$Aip_fus,data2$Aip_lae,
            data2$Aip_mos,data2$Aip_poo,data2$Aip_ten,data2$Asp_lin,data2$Asp_mue,data2$Chi_bel,data2$Chi_ino,
            data2$Chi_lap,data2$Chi_orn,data2$Chi_tor,data2$Dis_kin,data2$Dis_maj,data2$Dis_nig,data2$Dis_sto,
            data2$Emy_ann,data2$Emy_iji,data2$Enh_zwe,data2$Eph_gre,data2$Hyd_atr,data2$Hyd_bro,data2$Hyd_cya,
            data2$Hyd_dar,data2$Hyd_ele,data2$Hyd_fas,data2$Hyd_har,data2$Hyd_jer,data2$Hyd_mac,data2$Hyd_oce,
            data2$Hyd_pla,data2$Hyd_sch,data2$Hyd_spi,data2$Lap_cur,data2$Lei_cog,data2$Lei_cze,data2$Lei_mel,
            data2$Lei_pac,data2$Med_klo,data2$Mic_gra,data2$Pol_cae,data2$Tha_vip)

Lat_5 <- rbind(data2$Lat_col,data2$Lat_cro,data2$Lat_fro,data2$Lat_lat,data2$Lat_sai,data2$Pse_sem)

data1 <- All_10
data2 <- split(data1, data1$Especie)

Hyd_10 <- rbind(data2$Aca_per,data2$Aip_apr,data2$Aip_dub,data2$Aip_eyd,data2$Aip_fol,data2$Aip_fus,data2$Aip_lae,
            data2$Aip_mos,data2$Aip_poo,data2$Aip_ten,data2$Asp_lin,data2$Asp_mue,data2$Chi_bel,data2$Chi_ino,
            data2$Chi_lap,data2$Chi_orn,data2$Chi_tor,data2$Dis_kin,data2$Dis_maj,data2$Dis_nig,data2$Dis_sto,
            data2$Emy_ann,data2$Emy_iji,data2$Enh_zwe,data2$Eph_gre,data2$Hyd_atr,data2$Hyd_bro,data2$Hyd_cya,
            data2$Hyd_dar,data2$Hyd_ele,data2$Hyd_fas,data2$Hyd_har,data2$Hyd_jer,data2$Hyd_mac,data2$Hyd_oce,
            data2$Hyd_pla,data2$Hyd_sch,data2$Hyd_spi,data2$Lap_cur,data2$Lei_cog,data2$Lei_cze,data2$Lei_mel,
            data2$Lei_pac,data2$Med_klo,data2$Mic_gra,data2$Pol_cae,data2$Tha_vip)

Lat_10 <- rbind(data2$Lat_col,data2$Lat_cro,data2$Lat_fro,data2$Lat_lat,data2$Lat_sai,data2$Pse_sem)

# ------------------------------------------------------------------------------------------------ #

# Plot #

# ------------------------------------------------------------------------------------------------ #

x1 <- ggplot(Hyd_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")


p3 <- x1 + facet_grid(Temperature~variable)

x1 <- ggplot(Lat_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")


p4 <- x1 + facet_grid(Temperature~variable)

x1 <- ggplot(Hyd_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")


p5 <- x1 + facet_grid(Temperature~variable)

x1 <- ggplot(Lat_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(y = "Temperature (°C)")

x1 <- x1 + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "bold"))+ theme(plot.title = element_text(hjust = 0.5, size = 10))

x1 <- x1 + theme(legend.position = c(0.8, 0.2))

x1 <- x1 + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
x1 <- x1 + theme(legend.position = "none")


p6 <- x1 + facet_grid(Temperature~variable)

Hydro <- p3 + p5 
Hydro <- Hydro + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')

Latic <- p4 + p6
Latic <- Latic + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')

# ------------------------------------------------------------------------------------------------ #

# Genus level #

# ------------------------------------------------------------------------------------------------ #

# 5 arc-minutes #

data1 <- All_5
data2 <- split(data1, data1$Especie)

Aip_5 <- rbind(data2$Aip_apr,data2$Aip_dub,data2$Aip_eyd,data2$Aip_fol,data2$Aip_fus,data2$Aip_lae,
            data2$Aip_mos,data2$Aip_poo,data2$Aip_ten)
            
            
Asp_5 <- rbind(data2$Asp_lin,data2$Asp_mue)


Chi_5 <- rbind(data2$Chi_bel,data2$Chi_ino,data2$Chi_lap,data2$Chi_orn,data2$Chi_tor)

Dis_5 <- rbind(data2$Dis_kin,data2$Dis_maj,data2$Dis_nig,data2$Dis_sto)

Emy_5 <- rbind(data2$Emy_ann,data2$Emy_iji)

Hyd_5 <- rbind(data2$Hyd_atr,data2$Hyd_bro,data2$Hyd_cya,data2$Hyd_ele,data2$Hyd_fas,data2$Hyd_har,
                data2$Hyd_jer,data2$Hyd_mac,data2$Hyd_oce,data2$Hyd_pla,data2$Hyd_sch,data2$Hyd_spi)
                
Lei_5 <- rbind(data2$Lei_cog,data2$Lei_cze,data2$Lei_mel,data2$Lei_pac)

Lat_10 <- rbind(data2$Lat_col,data2$Lat_cro,data2$Lat_fro,data2$Lat_lat,data2$Lat_sai)

# 10 arc-minutes #

data3 <- All_10
data4 <- split(data3, data3$Especie)


Aip_10 <- rbind(data4$Aip_apr,data4$Aip_dub,data4$Aip_eyd,data4$Aip_fol,data4$Aip_fus,data4$Aip_lae,
            data4$Aip_mos,data4$Aip_poo,data4$Aip_ten)
            
            
Asp_10 <- rbind(data4$Asp_lin,data4$Asp_mue)


Chi_10 <- rbind(data4$Chi_bel,data4$Chi_ino,data4$Chi_lap,data4$Chi_orn,data4$Chi_tor)

Dis_10 <- rbind(data4$Dis_kin,data4$Dis_maj,data4$Dis_nig,data4$Dis_sto)

Emy_10 <- rbind(data4$Emy_ann,data4$Emy_iji)

Hyd_10 <- rbind(data4$Hyd_atr,data4$Hyd_bro,data4$Hyd_cya,data4$Hyd_ele,data4$Hyd_fas,data4$Hyd_har,
                data4$Hyd_jer,data4$Hyd_mac,data4$Hyd_oce,data4$Hyd_pla,data4$Hyd_sch,data4$Hyd_spi)
                
Lei_10 <- rbind(data4$Lei_cog,data4$Lei_cze,data4$Lei_mel,data4$Lei_pac)

Lat_10 <- rbind(data4$Lat_col,data4$Lat_cro,data4$Lat_fro,data4$Lat_lat,data4$Lat_sai)

# ------------------------------------------------------------------------------------------------ #

# Plots #

# ------------------------------------------------------------------------------------------------ #

# 5 arc-minutes #
## Aipysurus ##

b <- ggplot(Aip_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Aipysurus", y = "Temperature (°C)")

b <- b + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

b <- b + theme(legend.position = c(0.8, 0.2))

b <- b + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
b <- b + theme(legend.position = "none")


b <- b + facet_grid(Temperature~variable)
a <- b + theme(axis.text.x = element_blank())

## Aspidomorphus ##

c <- ggplot(Asp_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Aspidomorphus", y = "Temperature (°C)")

c <- c + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

c <- c + theme(legend.position = c(0.8, 0.2))

c <- c + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
c <- c + theme(legend.position = "none")


c <- c + facet_grid(Temperature~variable)
b <- c + theme(axis.text.x = element_blank())

## Chitulia ##

d <- ggplot(Chi_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Chitulia", y = "Temperature (°C)")

d <- d + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

d <- d + theme(legend.position = c(0.8, 0.2))

d <- d + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
d <- d + theme(legend.position = "none")
d <- d + facet_grid(Temperature~variable)
c <- d + theme(axis.text.x = element_blank())

## Disteira ##

e <- ggplot(Dis_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Disteira", y = "Temperature (°C)")

e <- e + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

e <- e + theme(legend.position = c(0.8, 0.2))

e <- e + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
e <- e + theme(legend.position = "none")
e <- e + facet_grid(Temperature~variable)
d <- e + theme(axis.text.x = element_blank())

## Emydocephalus ##

f <- ggplot(Emy_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Emydocephalus", y = "Temperature (°C)")

f <- f + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

f <- f + theme(legend.position = c(0.8, 0.2))

f <- f + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
f <- f + theme(legend.position = "none")
f <- f + facet_grid(Temperature~variable)
e <- f + theme(axis.text.x = element_blank())

## Hydrophis ##

h <- ggplot(Hyd_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Hydrophis", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
h <- h + facet_grid(Temperature~variable)
f <- h + theme(axis.text.x = element_blank())

## Laticauda ##

h <- ggplot(Lat_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Laticauda", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
g <- h + facet_grid(Temperature~variable)


## Leioselasma ##

h <- ggplot(Lei_5, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Leioselasma", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
h <- h + facet_grid(Temperature~variable)



### FINAL PLOT ###

p7 <- ( a + b ) / ( c + d ) / ( e + f ) / ( g + h ) 

Genus_5m <- p7 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')


# 10 arc-minutes #


## Aipysurus ##

b <- ggplot(Aip_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Aipysurus", y = "Temperature (°C)")

b <- b + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

b <- b + theme(legend.position = c(0.8, 0.2))

b <- b + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
b <- b + theme(legend.position = "none")


b <- b + facet_grid(Temperature~variable)
a <- b + theme(axis.text.x = element_blank())

## Aspidomorphus ##

c <- ggplot(Asp_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Aspidomorphus", y = "Temperature (°C)")

c <- c + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

c <- c + theme(legend.position = c(0.8, 0.2))

c <- c + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
c <- c + theme(legend.position = "none")


c <- c + facet_grid(Temperature~variable)
b <- c + theme(axis.text.x = element_blank())

## Chitulia ##

d <- ggplot(Chi_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Chitulia", y = "Temperature (°C)")

d <- d + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

d <- d + theme(legend.position = c(0.8, 0.2))

d <- d + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
d <- d + theme(legend.position = "none")
d <- d + facet_grid(Temperature~variable)
c <- d + theme(axis.text.x = element_blank())

## Disteira ##

e <- ggplot(Dis_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Disteira", y = "Temperature (°C)")

e <- e + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

e <- e + theme(legend.position = c(0.8, 0.2))

e <- e + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
e <- e + theme(legend.position = "none")
e <- e + facet_grid(Temperature~variable)
d <- e + theme(axis.text.x = element_blank())

## Emydocephalus ##

f <- ggplot(Emy_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Emydocephalus", y = "Temperature (°C)")

f <- f + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

f <- f + theme(legend.position = c(0.8, 0.2))

f <- f + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
f <- f + theme(legend.position = "none")
f <- f + facet_grid(Temperature~variable)
e <- f + theme(axis.text.x = element_blank())

## Hydrophis ##

h <- ggplot(Hyd_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Hydrophis", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
h <- h + facet_grid(Temperature~variable)
f <- h + theme(axis.text.x = element_blank())

## Laticauda ##

h <- ggplot(Lat_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Laticauda", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
g <- h + facet_grid(Temperature~variable)


## Leioselasma ##

h <- ggplot(Lei_10, aes(x=Set, y=value)) + 
geom_boxplot(lwd=0.2, position=position_dodge(1), fatten=1,outlier.size = 0.1) + 
labs(title = "Leioselasma", y = "Temperature (°C)")

h <- h + theme(axis.title.x = element_blank(),
plot.title = element_text(face = "italic"))+ theme(plot.title = element_text(hjust = 0.5, size = 20))

h <- h + theme(legend.position = c(0.8, 0.2))

h <- h + theme(legend.key=element_blank(),legend.background=element_blank(), 
legend.key.size = unit(1.5, "cm"), legend.key.width = unit(.8,"cm")) 
h <- h + theme(legend.position = "none")
h <- h + facet_grid(Temperature~variable)


### FINAL PLOT ###

p8 <- ( a + b ) / ( c + d ) / ( e + f ) / ( g + h ) 

Genus_10m <- p8 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face="bold")), 
  tag_levels = 'A', tag_suffix = ')')
  
# ------------------------------------------------------------------------------------------------ #

### EndNotRun
