
# ------------------------------------------------------------------------------------------------ #

### Title: Thermal realized limits process and plot  ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain ecological ###
### 			niches and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(dplyr)
library(reshape)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

# ------------------------------------------------------------------------------------------------ #

# Extract values to points by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

base <- read.csv("E:/1_Ssnks/2_Inputs/5_spp.csv")
cord <- base[, c("Long", "Lat")] 
cord <- as.data.frame(cord)

raster_names <- c("BMa_Tem_Max", "BMa_Tem_Mean", "BMa_Tem_Min", "Bme_Tem_Max", "Bme_Tem_Mean", 
				  "Bme_Tem_Min", "Bmi_Tem_Max", "Bmi_Tem_Mean", "Bmi_Tem_Min", "S_Tem_Max", 
				  "S_Tem_Mean", "S_Tem_Min", "M_Tem_Max", "M_Tem_Mean", "M_Tem_Min")
out_col_names <- c("Bma_Max", "Bma_Mean", "Bma_Min", "Bme_Max", "Bme_Mean", "Bme_Min", "Bmi_Max", 
				   "Bmi_Mean", "Bmi_Min", "S_Max", "S_Mean", "S_Min", "M_Max", "M_Mean", "M_Min")

for (i in 1:length(raster_names)) {
	assign(raster_names[i], raster(paste0("E:/1_Ssnks/5m/", 
	raster_names[i], ".tif")))
}

for (i in 1:length(raster_names)) {
	Tem <- raster::extract(get(raster_names[i]), cord)
	assign(out_col_names[i], cbind(base, Tem))
	assign(out_col_names[i], as.data.frame(get(out_col_names[i])))
}

M_Max$Tem <- M_Max$Tem/100
M_Mean$Tem <- M_Mean$Tem/100
M_Min$Tem <- M_Min$Tem/100

# ------------------------------------------------------------------------------------------------ #

# Estimate max and min realized limits by max, mean and min temperature by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

get_bounds <- function(data, name) {
	low <- aggregate(Tem ~ Species, data, min)
	high <- aggregate(Tem ~ Species, data, max)
	bounds <- data.frame(Species = low$Species, lower_5 = low$Tem, upper_5 = high$Tem, Set = name)
	return(bounds)
}

Max1 <- get_bounds(Bma_Max, "Bma")
Max2 <- get_bounds(Bme_Max, "Bme")
Max3 <- get_bounds(Bmi_Max, "Bmi")
Max4 <- get_bounds(S_Max, "Bsurf")
Max5 <- get_bounds(M_Max, "Msurf")
Set_Max <- rbind(Max1, Max2, Max3, Max4, Max5)

Mean1 <- get_bounds(Bma_Mean, "Bma")
Mean2 <- get_bounds(Bme_Mean, "Bme")
Mean3 <- get_bounds(Bmi_Mean, "Bmi")
Mean4 <- get_bounds(S_Mean, "Bsurf")
Mean5 <- get_bounds(M_Mean, "Msurf")
Set_Mean <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)

Mean1 <- get_bounds(Bma_Min, "Bma")
Mean2 <- get_bounds(Bme_Min, "Bme")
Mean3 <- get_bounds(Bmi_Min, "Bmi")
Mean4 <- get_bounds(S_Min, "Bsurf")
Mean5 <- get_bounds(M_Min, "Msurf")
Set_Min <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)

Max <- melt(Set_Max, measure.vars = c("lower_5", "upper_5"))
Min <- melt(Set_Min, measure.vars = c("lower_5", "upper_5"))
Mean <- melt(Set_Mean, measure.vars = c("lower_5", "upper_5"))

Max$Temperature <- "Max"
Min$Temperature <- "Min"
Mean$Temperature <- "Mean"

All_5 <- rbind(Max, Mean, Min)

levels(All_5$variable) <- gsub("lower_5", "Lower", levels(All_5$variable))
levels(All_5$variable) <- gsub("upper_5", "Upper", levels(All_5$variable))

# ------------------------------------------------------------------------------------------------ #

# Estimate if max, mean and min temperature are normal distributed by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Tem <- function(data) {
    Max_Lower <- subset(data, Temperature == "Max" & variable == "Lower")$value
	Mean_Lower <- subset(data, Temperature == "Mean" & variable == "Lower")$value
	Min_Lower <- subset(data, Temperature == "Min" & variable == "Lower")$value
    Max_Upper <- subset(data, Temperature == "Max" & variable == "Upper")$value
	Mean_Upper <- subset(data, Temperature == "Mean" & variable == "Upper")$value
	Min_Upper <- subset(data, Temperature == "Min" & variable == "Upper")$value
	normality_test_Lower_W <- c(shapiro.test(Max_Lower)$statistic, shapiro.test(Mean_Lower)$statistic, 
							  shapiro.test(Min_Lower)$statistic)
    normality_test_Lower <- c(shapiro.test(Max_Lower)$p.value, shapiro.test(Mean_Lower)$p.value, 
							  shapiro.test(Min_Lower)$p.value)
	normality_test_Upper <- c(shapiro.test(Max_Upper)$p.value, shapiro.test(Mean_Upper)$p.value, 
                              shapiro.test(Min_Upper)$p.value)
	normality_test_Upper_W <- c(shapiro.test(Max_Upper)$statistic, shapiro.test(Mean_Upper)$statistic, 
							  shapiro.test(Min_Upper)$statistic)						  
	return(list(Lower_W = normality_test_Lower_W, Lower = normality_test_Lower, Upper_W = normality_test_Upper_W, Upper = normality_test_Upper))
}

check_normality_and_test_Tem(All_5)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- All_5
data2 <- split(data1, data1$Set)

tests <- c("Max", "Mean", "Min")
sets <- c("Bma", "Bme", "Bmi", "Bsurf", "Msurf")

results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

lin_5 <- results
df_wide <- as.data.frame(pivot_wider(lin_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/lin_5_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Extract values to points by Lineage at 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

raster_names <- c("BMa_Tem_Max", "BMa_Tem_Mean", "BMa_Tem_Min", "Bme_Tem_Max", "Bme_Tem_Mean", 
				  "Bme_Tem_Min", "Bmi_Tem_Max", "Bmi_Tem_Mean", "Bmi_Tem_Min", "S_Tem_Max", 
				  "S_Tem_Mean", "S_Tem_Min", "M_Tem_Max", "M_Tem_Mean", "M_Tem_Min")
out_col_names <- c("Bma_Max", "Bma_Mean", "Bma_Min", "Bme_Max", "Bme_Mean", "Bme_Min", "Bmi_Max", 
				   "Bmi_Mean", "Bmi_Min", "S_Max", "S_Mean", "S_Min", "M_Max", "M_Mean", "M_Min")

for (i in 1:length(raster_names)) {
	assign(raster_names[i], raster(paste0("E:/1_Ssnks/10m/", 
	raster_names[i], ".tif")))
}

for (i in 1:length(raster_names)) {
	Tem <- raster::extract(get(raster_names[i]), cord)
	assign(out_col_names[i], cbind(base, Tem))
	assign(out_col_names[i], as.data.frame(get(out_col_names[i])))
}

M_Max$Tem <- M_Max$Tem/100
M_Mean$Tem <- M_Mean$Tem/100
M_Min$Tem <- M_Min$Tem/100

# ------------------------------------------------------------------------------------------------ #

# Estimate max and min realized limits by max, mean and min temperature by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Max1 <- get_bounds(Bma_Max, "Bma")
Max2 <- get_bounds(Bme_Max, "Bme")
Max3 <- get_bounds(Bmi_Max, "Bmi")
Max4 <- get_bounds(S_Max, "Bsurf")
Max5 <- get_bounds(M_Max, "Msurf")
Set_Max <- rbind(Max1, Max2, Max3, Max4, Max5)

Mean1 <- get_bounds(Bma_Mean, "Bma")
Mean2 <- get_bounds(Bme_Mean, "Bme")
Mean3 <- get_bounds(Bmi_Mean, "Bmi")
Mean4 <- get_bounds(S_Mean, "Bsurf")
Mean5 <- get_bounds(M_Mean, "Msurf")
Set_Mean <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)

Mean1 <- get_bounds(Bma_Min, "Bma")
Mean2 <- get_bounds(Bme_Min, "Bme")
Mean3 <- get_bounds(Bmi_Min, "Bmi")
Mean4 <- get_bounds(S_Min, "Bsurf")
Mean5 <- get_bounds(M_Min, "Msurf")
Set_Min <- rbind(Mean1, Mean2, Mean3, Mean4, Mean5)

Max <- melt(Set_Max, measure.vars = c("lower_5", "upper_5"))
Min <- melt(Set_Min, measure.vars = c("lower_5", "upper_5"))
Mean <- melt(Set_Mean, measure.vars = c("lower_5", "upper_5"))

Max$Temperature <- "Max"
Min$Temperature <- "Min"
Mean$Temperature <- "Mean"

All_10 <- rbind(Max, Mean, Min)

levels(All_10$variable) <- gsub("lower_5", "Lower", levels(All_10$variable))
levels(All_10$variable) <- gsub("upper_5", "Upper", levels(All_10$variable))

# ------------------------------------------------------------------------------------------------ #

# Estimate if max, mean and min temperature are normal distributed by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Tem(All_10)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- All_10
data2 <- split(data1, data1$Set)

results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

lin_10 <- results
df_wide <- as.data.frame(pivot_wider(lin_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/lin_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Subset data into subfamily level by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Lat_5 <- subset(All_5, Species == "Lat_col" | Species == "Lat_cro" | Species == "Lat_fro" | 
			    Species == "Lat_lat" | Species == "Lat_sai" | Species == "Lat_sch" | Species == "Lat_sem")
Hyd_5 <- subset(All_5, !(Species %in% Lat_5$Species))
Lat_10 <- subset(All_10, Species == "Lat_col" | Species == "Lat_cro" | Species == "Lat_fro" | 
			    Species == "Lat_lat" | Species == "Lat_sai" | Species == "Lat_sch" | Species == "Lat_sem")
Hyd_10 <- subset(All_10, !(Species %in% Lat_10$Species))

# ------------------------------------------------------------------------------------------------ #

# Estimate if max, mean and min temperature are normal distributed by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Tem(Hyd_5)
check_normality_and_test_Tem(Lat_5)
check_normality_and_test_Tem(Hyd_10)
check_normality_and_test_Tem(Lat_10)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- Hyd_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Hyd_5 <- results
df_wide <- as.data.frame(pivot_wider(Hyd_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Hyd_5_stats.csv", row.names = FALSE)

data1 <- Lat_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Lat_5 <- results
df_wide <- as.data.frame(pivot_wider(Lat_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Lat_5_stats.csv", row.names = FALSE)

data1 <- Hyd_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Hyd_10 <- results
df_wide <- as.data.frame(pivot_wider(Hyd_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Hyd_10_stats.csv", row.names = FALSE)

data1 <- Lat_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Lat_10 <- results
df_wide <- as.data.frame(pivot_wider(Lat_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Lat_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Subset data by Genus level at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Aip_5 <- subset(All_5, Species %in% c("Aip_apr", "Aip_dub", "Aip_eyd", "Aip_fol", "Aip_fus", "Aip_lae",
                                      "Aip_mos", "Aip_poo", "Aip_ten"))
Aip_10 <- subset(All_10, Species %in% c("Aip_apr", "Aip_dub", "Aip_eyd", "Aip_fol", "Aip_fus", "Aip_lae",
                                       "Aip_mos", "Aip_poo", "Aip_ten"))

Emy_5 <- subset(All_5, Species %in% c("Emy_ann", "Emy_iji"))
Emy_10 <- subset(All_10, Species %in% c("Emy_ann", "Emy_iji"))

Lat_5 <- subset(All_5, Species %in% c("Lat_col", "Lat_cro", "Lat_fro", "Lat_lat", "Lat_sai", "Lat_sch", "Lat_sem"))
Lat_10 <- subset(All_10, Species %in% c("Lat_col", "Lat_cro", "Lat_fro", "Lat_lat", "Lat_sai", "Lat_sch", "Lat_sem"))

all_5 <- rbind(Aip_5, Emy_5, Lat_5)
all_10 <- rbind(Aip_10, Emy_10, Lat_10)
Hyd_5 <- subset(All_5, !(Species %in% all_5$Species))
Hyd_10 <- subset(All_10, !(Species %in% all_10$Species))

# ------------------------------------------------------------------------------------------------ #

# Estimate if max, mean and min temperature are normal distributed by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Tem(Aip_5)
check_normality_and_test_Tem(Emy_5)
check_normality_and_test_Tem(Hyd_5)
check_normality_and_test_Tem(Aip_10)
check_normality_and_test_Tem(Emy_10)
check_normality_and_test_Tem(Hyd_10)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- Aip_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Aip_5 <- results
df_wide <- as.data.frame(pivot_wider(Aip_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Aip_5_stats.csv", row.names = FALSE)

data1 <- Emy_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Emy_5 <- results
df_wide <- as.data.frame(pivot_wider(Emy_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Emy_5_stats.csv", row.names = FALSE)

data1 <- Hyd_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Hyd_5 <- results
df_wide <- as.data.frame(pivot_wider(Hyd_5, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Hyd_5_stats.csv", row.names = FALSE)

data1 <- Aip_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Aip_10 <- results
df_wide <- as.data.frame(pivot_wider(Aip_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Aip_10_stats.csv", row.names = FALSE)

data1 <- Emy_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Emy_10 <- results
df_wide <- as.data.frame(pivot_wider(Emy_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Emy_10_stats.csv", row.names = FALSE)

data1 <- Hyd_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[s]], data2[[s]]$Temperature)
	assign(paste0(s, "_Max"), data3[[1]])
	assign(paste0(s, "_Mean"), data3[[2]])
	assign(paste0(s, "_Min"), data3[[3]])
	for (t in tests) {
		p_value <- fligner.test(value ~ variable, get(paste0(s, "_", t)))$p.value
		row <- data.frame(Set = s, Tem = t, p = p_value)
		results <- rbind(results, row)
	}
}

Hyd_10 <- results
df_wide <- as.data.frame(pivot_wider(Hyd_10, names_from = Set, values_from = p))
write.csv(df_wide, "E:/1_Ssnks/Hyd_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plot functions #

# ------------------------------------------------------------------------------------------------ #

left <- function(data) {
  ggplot(data, aes(x = Set, y = value)) + 
    geom_boxplot(lwd = 0.2, position = position_dodge(1), fatten = 1, outlier.size = 0.1) + 
    labs(y = "Temperature (°C)", x = "") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 4),
          legend.position = "none", 
          legend.key = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(1.5, "cm"), 
          legend.key.width = unit(.8,"cm")) + 
    facet_grid(Temperature~variable, space = "free_x") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
		  axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank())
}

rigth <- function(data) {
  ggplot(data, aes(x = Set, y = value)) + 
    geom_boxplot(lwd = 0.2, position = position_dodge(1), fatten = 1, outlier.size = 0.1) + 
    labs(y = "", x = "") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 4),
          legend.position = "none", 
          legend.key = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(1.5, "cm"), 
          legend.key.width = unit(.8,"cm")) + 
    facet_grid(Temperature~variable, space = "free_x") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
		  axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
		  axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
}

rigth_under <- function(data) {
  ggplot(data, aes(x = Set, y = value)) + 
    geom_boxplot(lwd = 0.2, position = position_dodge(1), fatten = 1, outlier.size = 0.1) + 
    labs(y = "", x = "") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 4),
          legend.position = "none", 
          legend.key = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(1.5, "cm"),
		  legend.key.width = unit(.8,"cm")) + 
    facet_grid(Temperature~variable, space = "free_x") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
		  axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
		  axis.text.x = element_text(angle = 45, hjust = 1))
}

left_under <- function(data) {
  ggplot(data, aes(x = Set, y = value)) + 
    geom_boxplot(lwd = 0.2, position = position_dodge(1), fatten = 1, outlier.size = 0.1) + 
    labs(y = "Temperature (°C)", x = "") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 4),
          legend.position = "none", 
          legend.key = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(1.5, "cm"),
		  axis.text.x = element_text(angle = 45, hjust = 1),		  
          legend.key.width = unit(.8,"cm")) + 
    facet_grid(Temperature~variable, space = "free_x") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
		  axis.text.x = element_text(angle = 45, hjust = 1))
}

# ------------------------------------------------------------------------------------------------ #

# Final plot lineage and Subfamily #

# ------------------------------------------------------------------------------------------------ #

Lat_5 <- subset(All_5, Species == "Lat_col" | Species == "Lat_cro" | Species == "Lat_fro" | 
			    Species == "Lat_lat" | Species == "Lat_sai" | Species == "Lat_sch" | Species == "Lat_sem")
Hyd_5 <- subset(All_5, !(Species %in% Lat_5$Species))
Lat_10 <- subset(All_10, Species == "Lat_col" | Species == "Lat_cro" | Species == "Lat_fro" | 
			    Species == "Lat_lat" | Species == "Lat_sai" | Species == "Lat_sch" | Species == "Lat_sem")
Hyd_10 <- subset(All_10, !(Species %in% Lat_10$Species))

p1 <- left(All_5) + ggtitle("A)")
p2 <- rigth(All_10) + ggtitle("B)")
p3 <- left(Hyd_5) + ggtitle("C)")
p4 <- rigth(Hyd_10) + ggtitle("D)")
p5 <- left_under(Lat_5) + ggtitle("E)")
p6 <- rigth_under(Lat_10) + ggtitle("F)")

lab1 <- "Lineage"
lab2 <- "Subfamily"

dat <- data.frame(y = c(1,1,0,0), x = c(1,0,0,1))

make_line <- function(lab) {
ggplot(dat) + 
geom_path(aes(x,y)) +
scale_x_continuous(expand = c(1.5,0,0,0)) +
scale_y_continuous(expand = c(0.01,0))+
annotate("text", angle = 90, x = 0, y = 0.5, vjust = -0.5, label = lab, size = 3) +
theme_void() + 
theme(plot.margin= margin(0,0,0,0)) }

line1 <- make_line(lab1)
line2 <- make_line(lab2)

layout <-"ABBBBBBCCCCCC
		  DEEEEEEFFFFFF
		  DGGGGGGHHHHHH"
		  
Final_1 <- wrap_plots(line1, p1, p2, line2, p3, p4, p5, p6, design = layout,
                    widths = c(0.5, 0.5), heights = c(0.33, 0.33, 0.33))

setwd("E:/1_Ssnks/2_Figures")
ggsave(file = "Fig3.tiff", plot = Final_1, width = 14, height = 19, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig3.pdf", plot = Final_1, width = 14, height = 19, dpi = 300, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

# Final plot genus #

# ------------------------------------------------------------------------------------------------ #

Aip_5 <- subset(All_5, Species %in% c("Aip_apr", "Aip_dub", "Aip_eyd", "Aip_fol", "Aip_fus", "Aip_lae",
                                      "Aip_mos", "Aip_poo", "Aip_ten"))
Aip_10 <- subset(All_10, Species %in% c("Aip_apr", "Aip_dub", "Aip_eyd", "Aip_fol", "Aip_fus", "Aip_lae",
                                       "Aip_mos", "Aip_poo", "Aip_ten"))

Emy_5 <- subset(All_5, Species %in% c("Emy_ann", "Emy_iji"))
Emy_10 <- subset(All_10, Species %in% c("Emy_ann", "Emy_iji"))

Lat_5 <- subset(All_5, Species %in% c("Lat_col", "Lat_cro", "Lat_fro", "Lat_lat", "Lat_sai", "Lat_sch", "Lat_sem"))
Lat_10 <- subset(All_10, Species %in% c("Lat_col", "Lat_cro", "Lat_fro", "Lat_lat", "Lat_sai", "Lat_sch", "Lat_sem"))

all_5 <- rbind(Aip_5, Emy_5, Lat_5)
all_10 <- rbind(Aip_10, Emy_10, Lat_10)
Hyd_5 <- subset(All_5, !(Species %in% all_5$Species))
Hyd_10 <- subset(All_10, !(Species %in% all_10$Species))

p7 <- left(Aip_5) + ggtitle("A)")
p8 <- rigth(Aip_10) + ggtitle("B)")
p9 <- left(Emy_5) + ggtitle("C)")
p10 <- rigth(Emy_10) + ggtitle("D)")
p11 <- left_under(Hyd_5) + ggtitle("E)")
p12 <- rigth_under(Hyd_10) + ggtitle("F)")

lab1 <- expression(italic("Aipysurus"))
lab2 <- expression(italic("Emydocephalus"))
lab3 <- expression(italic("Hydrophis"))

dat <- data.frame(y = c(1,1,0,0), x = c(1,0,0,1))

make_line <- function(lab) {
ggplot(dat) + 
geom_path(aes(x,y)) +
scale_x_continuous(expand = c(1.5,0,0,0)) +
scale_y_continuous(expand = c(0.01,0))+
annotate("text", angle = 90, x = 0, y = 0.5, vjust = -0.5, label = lab, size = 3) +
theme_void() + 
theme(plot.margin= margin(0,0,0,0)) }

line1 <- make_line(lab1)
line2 <- make_line(lab2)
line3 <- make_line(lab3)

layout <-"ABBBBBBCCCCCC
		  DEEEEEEFFFFFF
		  GHHHHHHIIIIII"
		  
Final_2 <- wrap_plots(line1, p7, p8, line2, p9, p10, line3, p11, p12, design = layout,
                    widths = c(0.5, 0.5), heights = c(0.33, 0.33, 0.33))

Final_2
					
setwd("E:/1_Ssnks/2_Figures")
ggsave(file = "Fig4.tiff", plot = Final_2, width = 14, height = 19, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig4.pdf", plot = Final_2, width = 14, height = 19, dpi = 300, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
