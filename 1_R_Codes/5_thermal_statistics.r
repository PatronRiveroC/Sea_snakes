
# ------------------------------------------------------------------------------------------------ #

### Title: Thermal realized limits process and plot  ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

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

base <- read.csv("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/5_spp.csv")
cord <- base[, c("Long", "Lat")] 
cord <- as.data.frame(cord)

raster_names <- c("BMa_Tem_Max", "BMa_Tem_Mean", "BMa_Tem_Min", "Bme_Tem_Max", "Bme_Tem_Mean", 
				  "Bme_Tem_Min", "Bmi_Tem_Max", "Bmi_Tem_Mean", "Bmi_Tem_Min", "S_Tem_Max", 
				  "S_Tem_Mean", "S_Tem_Min", "M_Tem_Max", "M_Tem_Mean", "M_Tem_Min")
out_col_names <- c("Bma_Max", "Bma_Mean", "Bma_Min", "Bme_Max", "Bme_Mean", "Bme_Min", "Bmi_Max", 
				   "Bmi_Mean", "Bmi_Min", "S_Max", "S_Mean", "S_Min", "M_Max", "M_Mean", "M_Min")

for (i in 1:length(raster_names)) {
	assign(raster_names[i], raster(paste0("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/5m/", 
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
	low <- aggregate(Tem ~ Especie, data, min)
	high <- aggregate(Tem ~ Especie, data, max)
	bounds <- data.frame(Especie = low$Especie, lower_5 = low$Tem, upper_5 = high$Tem, Set = name)
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
    normality_test_Lower <- c(wilcox.test(Max_Lower)$p.value, wilcox.test(Mean_Lower)$p.value, 
							  wilcox.test(Min_Lower)$p.value)
	normality_test_Upper <- c(wilcox.test(Max_Upper)$p.value, wilcox.test(Mean_Upper)$p.value, 
                              wilcox.test(Min_Upper)$p.value)
	return(list(Lower = normality_test_Lower, Upper = normality_test_Upper))
}

check_normality_and_test_Tem(All_5)

# ------------------------------------------------------------------------------------------------ #

# Estimate if Bma, Bme, Bmi, BSurf and MSurf are normal distributed by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Set <- function(data) {
	BMa_Lower <- subset(data, Set == "Bma" & variable == "Lower")$value
	BMe_Lower <- subset(data, Set == "Bme" & variable == "Lower")$value
	BMi_Lower <- subset(data, Set == "Bmi" & variable == "Lower")$value
	BSurf_Lower <- subset(data, Set == "Bsurf" & variable == "Lower")$value
	MSurf_Lower <- subset(data, Set == "Msurf" & variable == "Lower")$value
    BMa_Upper <- subset(data, Set == "Bma" & variable == "Upper")$value
	BMe_Upper <- subset(data, Set == "Bme" & variable == "Upper")$value
	BMi_Upper <- subset(data, Set == "Bmi" & variable == "Upper")$value
	BSurf_Upper <- subset(data, Set == "Bsurf" & variable == "Upper")$value
	MSurf_Upper <- subset(data, Set == "Msurf" & variable == "Upper")$value
	normality_test_Lower <- c(wilcox.test(BMa_Lower)$p.value, wilcox.test(BMe_Lower)$p.value, 
                              wilcox.test(BMi_Lower)$p.value, wilcox.test(BSurf_Lower)$p.value,
							  wilcox.test(MSurf_Lower)$p.value)
	normality_test_Upper <- c(wilcox.test(BMa_Upper)$p.value, wilcox.test(BMe_Upper)$p.value, 
                              wilcox.test(BMi_Upper)$p.value, wilcox.test(BSurf_Upper)$p.value, 
                              wilcox.test(MSurf_Upper)$p.value)
	return(list(Lower = normality_test_Lower, Upper = normality_test_Upper))
}

check_normality_and_test_Set(All_5)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- All_5
data2 <- split(data1, data1$Set)

tests <- c("Max", "Mean", "Min")
sets <- c("Bma", "Bme", "Bmi", "Bsurf", "Msurf")

results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/lin_5_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Extract values to points by Lineage at 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

base <- read.csv("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/5_spp.csv")
cord <- base[, c("Long", "Lat")] 
cord <- as.data.frame(cord)

raster_names <- c("BMa_Tem_Max", "BMa_Tem_Mean", "BMa_Tem_Min", "Bme_Tem_Max", "Bme_Tem_Mean", 
				  "Bme_Tem_Min", "Bmi_Tem_Max", "Bmi_Tem_Mean", "Bmi_Tem_Min", "S_Tem_Max", 
				  "S_Tem_Mean", "S_Tem_Min", "M_Tem_Max", "M_Tem_Mean", "M_Tem_Min")
out_col_names <- c("Bma_Max", "Bma_Mean", "Bma_Min", "Bme_Max", "Bme_Mean", "Bme_Min", "Bmi_Max", 
				   "Bmi_Mean", "Bmi_Min", "S_Max", "S_Mean", "S_Min", "M_Max", "M_Mean", "M_Min")

for (i in 1:length(raster_names)) {
	assign(raster_names[i], raster(paste0("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/10m/", 
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

# Estimate if Bma, Bme, Bmi, BSurf and MSurf are normal distributed by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Set(All_10)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- All_10
data2 <- split(data1, data1$Set)

results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/lin_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plot by Lineage at 5 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

create_boxplot <- function(data) {
  ggplot(data, aes(x = Set, y = value)) + 
    geom_boxplot(lwd = 0.2, position = position_dodge(1), fatten = 1, outlier.size = 0.1) + 
    labs(y = "Temperature (°C)", x = "") + 
    theme(axis.title.x = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
          legend.position = "none", 
          legend.key = element_blank(), 
          legend.background = element_blank(), 
          legend.key.size = unit(1.5, "cm"), 
          legend.key.width = unit(.8,"cm")) + 
    facet_grid(Temperature~variable) + 
    theme_bw()
}

p1 <- create_boxplot(All_5)
p2 <- create_boxplot(All_10)

plot1 <- p1 + p2
Sea_Snakes <- plot1 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

# ------------------------------------------------------------------------------------------------ #

# Subset data into subfamily level by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Lat_5 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_5 <- subset(All_5, !(Especie %in% Lat_5$Especie))
Lat_10 <- subset(All_10, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_10 <- subset(All_10, !(Especie %in% Lat_10$Especie))

# ------------------------------------------------------------------------------------------------ #

# Estimate if max, mean and min temperature are normal distributed by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Tem(Hyd_5)
check_normality_and_test_Tem(Lat_5)
check_normality_and_test_Tem(Hyd_10)
check_normality_and_test_Tem(Lat_10)

# ------------------------------------------------------------------------------------------------ #

# Estimate if Bma, Bme, Bmi, BSurf and MSurf are normal distributed by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Set(Hyd_5)
check_normality_and_test_Set(Lat_5)
check_normality_and_test_Set(Hyd_10)
check_normality_and_test_Set(Lat_10)

# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- Hyd_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Hyd_5_stats.csv", row.names = FALSE)

data1 <- Lat_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Lat_5_stats.csv", row.names = FALSE)

data1 <- Hyd_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Hyd_10_stats.csv", row.names = FALSE)

data1 <- Lat_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Lat_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plot by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Lat_5 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_5 <- subset(All_5, !(Especie %in% Lat_5$Especie))
Lat_10 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_10 <- subset(All_5, !(Especie %in% Lat_5$Especie))

p1 <- create_boxplot(Hyd_5)
p2 <- create_boxplot(Hyd_10)
p3 <- create_boxplot(Lat_5)
p4 <- create_boxplot(Lat_10)

plot2 <- (p1 + p2) / (p3 + p4)
Subfam <- plot2 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

# ------------------------------------------------------------------------------------------------ #

# Subset data into Genus level by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Aip_5 <- subset(All_5, Especie == "Aip_apr" | Especie == "Aip_dub" | Especie == "Aip_eyd" | 
			    Especie == "Aip_fol" | Especie == "Aip_fus" | Especie == "Aip_lae" |
				Especie == "Aip_mos" | Especie == "Aip_poo" | Especie == "Aip_ten")

Aip_10 <- subset(All_10, Especie == "Aip_apr" | Especie == "Aip_dub" | Especie == "Aip_eyd" | 
			    Especie == "Aip_fol" | Especie == "Aip_fus" | Especie == "Aip_lae" |
				Especie == "Aip_mos" | Especie == "Aip_poo" | Especie == "Aip_ten")

Emy_5 <- subset(All_5, Especie == "Emy_ann" | Especie == "Emy_iji")

Emy_10 <- subset(All_10, Especie == "Emy_ann" | Especie == "Emy_iji")

Lat_5 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")

Lat_10 <- subset(All_10, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")

Mic_5 <- subset(All_5, Especie == "Mic_gra")

Mic_10 <- subset(All_10, Especie == "Mic_gra")

all_5 <- rbind(Aip_5, Emy_5, Lat_5, Mic_5)

all_10 <- rbind(Aip_10, Emy_10, Lat_10, Mic_10)

Hyd_5 <- subset(All_5, !(Especie %in% all_5$Especie))

Hyd_10 <- subset(All_10, !(Especie %in% all_10$Especie))

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

# Estimate if Bma, Bme, Bmi, BSurf and MSurf are normal distributed by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

check_normality_and_test_Set(Aip_5)
check_normality_and_test_Set(Emy_5)
check_normality_and_test_Set(Hyd_5)
check_normality_and_test_Set(Aip_10)
check_normality_and_test_Set(Emy_10)
check_normality_and_test_Set(Hyd_10)


# ------------------------------------------------------------------------------------------------ #

# Homogeneity of variances for non normal data by Subfamily at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

data1 <- Aip_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Aip_5_stats.csv", row.names = FALSE)

data1 <- Emy_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Emy_5_stats.csv", row.names = FALSE)

data1 <- Hyd_5
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Hyd_5_stats.csv", row.names = FALSE)

data1 <- Aip_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Aip_10_stats.csv", row.names = FALSE)

data1 <- Emy_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Emy_10_stats.csv", row.names = FALSE)

data1 <- Hyd_10
data2 <- split(data1, data1$Set)
results <- data.frame(Set = character(), S_res = numeric(), p = numeric(), stringsAsFactors = FALSE)

for (s in sets) {
	data3 <- split(data2[[1]], data2[[1]]$Temperature)
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
write.csv(df_wide, "D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/6_Inputs/8_Tem/Hyd_10_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Plot by Genus at 5 and 10 arc-minutes #

# ------------------------------------------------------------------------------------------------ #

Aip_5 <- subset(All_5, Especie == "Aip_apr" | Especie == "Aip_dub" | Especie == "Aip_eyd" | 
			    Especie == "Aip_fol" | Especie == "Aip_fus" | Especie == "Aip_lae" |
				Especie == "Aip_mos" | Especie == "Aip_poo" | Especie == "Aip_ten")

Aip_10 <- subset(All_10, Especie == "Aip_apr" | Especie == "Aip_dub" | Especie == "Aip_eyd" | 
			    Especie == "Aip_fol" | Especie == "Aip_fus" | Especie == "Aip_lae" |
				Especie == "Aip_mos" | Especie == "Aip_poo" | Especie == "Aip_ten")

Emy_5 <- subset(All_5, Especie == "Emy_ann" | Especie == "Emy_iji")

Emy_10 <- subset(All_10, Especie == "Emy_ann" | Especie == "Emy_iji")

Lat_5 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")

Lat_10 <- subset(All_10, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")

Mic_5 <- subset(All_5, Especie == "Mic_gra")

Mic_10 <- subset(All_10, Especie == "Mic_gra")

all_5 <- rbind(Aip_5, Emy_5, Lat_5, Mic_5)

all_10 <- rbind(Aip_10, Emy_10, Lat_10, Mic_10)

Hyd_5 <- subset(All_5, !(Especie %in% all_5$Especie))

Hyd_10 <- subset(All_10, !(Especie %in% all_10$Especie))

p1 <- create_boxplot(Aip_5)
p2 <- create_boxplot(Aip_10)
p3 <- create_boxplot(Emy_5)
p4 <- create_boxplot(Emy_10)
p5 <- create_boxplot(Hyd_5)
p6 <- create_boxplot(Hyd_10)

plot3 <- (p1 + p2) / (p3 + p4) / (p5 + p6)
Genus <- plot3 + plot_annotation(
  theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
  tag_levels = 'A', tag_suffix = ')')

# ------------------------------------------------------------------------------------------------ #

# Final plot #

# ------------------------------------------------------------------------------------------------ #

Lat_5 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_5 <- subset(All_5, !(Especie %in% Lat_5$Especie))
Lat_10 <- subset(All_5, Especie == "Lat_col" | Especie == "Lat_cro" | Especie == "Lat_fro" | 
			    Especie == "Lat_lat" | Especie == "Lat_sai" | Especie == "Lat_sem")
Hyd_10 <- subset(All_5, !(Especie %in% Lat_5$Especie))

p1 <- create_boxplot(All_5) + ggtitle("A)")
p2 <- create_boxplot(All_10) + ggtitle("B)")
p3 <- create_boxplot(Hyd_5) + ggtitle("C)")
p4 <- create_boxplot(Hyd_10) + ggtitle("D)")
p5 <- create_boxplot(Lat_5) + ggtitle("E)")
p6 <- create_boxplot(Lat_10) + ggtitle("F)")
p7 <- create_boxplot(Aip_5) + ggtitle("G)")
p8 <- create_boxplot(Aip_10) + ggtitle("H)")
p9 <- create_boxplot(Emy_5) + ggtitle("I)")
p10 <- create_boxplot(Emy_10) + ggtitle("J)")

lab1 <- "Lineage"
lab2 <- "Subfamily"
lab3 <- "Genus"

dat <- data.frame(y = c(1,1,0,0), x = c(1,0,0,1))

make_line <- function(lab) {
ggplot(dat) + 
geom_path(aes(x,y)) +
scale_x_continuous(expand = c(1.5,0,0,0)) +
scale_y_continuous(expand = c(0.01,0))+
annotate("text", angle = 90, x = 0, y = 0.5, vjust = -0.5, label = lab, size = 5.5) +
theme_void() + 
theme(plot.margin= margin(0,0,0,0)) }

line1 <- make_line(lab1)
line2 <- make_line(lab2)
line3 <- make_line(lab3)

layout <-"ABBBBBBCCCCCC
		  DEEEEEEFFFFFF
		  DGGGGGGHHHHHH
		  IJJJJJJKKKKKK
		  ILLLLLLMMMMMM"
		  
Final <- wrap_plots(line1, p1, p2, line2, p3, p4, p5, p6, line3, p7, p8, p9, p10, design = layout)

setwd("D:/Disco_D/SSnks_Var_Therm/Submission/Diversity/2_Figures")
ggsave(file = "Fig3X_Therm_1000_dpi.jpeg", plot = Final, width = 30, height = 50, dpi = 1000, units = "cm", device = "jpeg")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
