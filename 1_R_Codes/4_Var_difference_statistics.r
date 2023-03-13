
# ------------------------------------------------------------------------------------------------ #

### Title: Statistical variable comparisson ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(dplyr)
library(tidyr)
library(purrr)

# ------------------------------------------------------------------------------------------------ #

# Read and clean function #

# ------------------------------------------------------------------------------------------------ #

read_and_clean_data <- function(path_to_file_1, path_to_file_2) {
	data1 <- read.csv(path_to_file_1)
	colnames(data1)[4:13] <- paste0(colnames(data1)[4:13], "_PC")
    colnames(data1)[14:23] <- gsub("\\.1$", "", colnames(data1)[14:23])
	colnames(data1)[14:23] <- paste0(colnames(data1)[14:23], "_PI")
	df1_PC <- select(data1, Family, Genus, ends_with("_PC"))
	df1_PI <- select(data1, Family, Genus, ends_with("_PI"))
    colnames(df1_PC)[3:12] <- gsub("\\_PC$", "", colnames(df1_PC)[3:12])
	colnames(df1_PI)[3:12] <- gsub("\\_PI$", "", colnames(df1_PI)[3:12])
    df1_PC$Lineage <- "Sea snakes"
	df1_PI$Lineage <- "Sea snakes"
	data2 <- read.csv(path_to_file_2)
	colnames(data2)[4:13] <- paste0(colnames(data2)[4:13], "_PC")
	colnames(data2)[14:23] <- gsub("\\.1$", "", colnames(data2)[14:23])
	colnames(data2)[14:23] <- paste0(colnames(data2)[14:23], "_PI")
	df2_PC <- select(data2, Family, Genus, ends_with("_PC"))
	df2_PI <- select(data2, Family, Genus, ends_with("_PI"))
	colnames(df2_PC)[3:12] <- gsub("\\_PC$", "", colnames(df2_PC)[3:12])
	colnames(df2_PI)[3:12] <- gsub("\\_PI$", "", colnames(df2_PI)[3:12])
	df2_PC$Lineage <- "Sea snakes"
	df2_PI$Lineage <- "Sea snakes"
	return(list(df1_PC = df1_PC, df1_PI = df1_PI, df2_PC = df2_PC, df2_PI = df2_PI))
}

# ------------------------------------------------------------------------------------------------ #

# Read and clean data #

# ------------------------------------------------------------------------------------------------ #

data <- read_and_clean_data("D:/6_Inputs/1_raw_PC_PI_5.csv",
					"D:/6_Inputs/2_raw_PC_PI_10.csv")

df1_PC <- data$df1_PC
df1_PI <- data$df1_PI
df2_PC <- data$df2_PC
df2_PI <- data$df2_PI

# ------------------------------------------------------------------------------------------------ #

# Comparissons for Lineage #

# ------------------------------------------------------------------------------------------------ #

compare_values <- function(var, df1, df2) {
	df1_var <- df1 %>% filter(df1[[var]] != 0) %>% select(Lineage, Family, Genus, {{var}})
	df2_var <- df2 %>% filter(df2[[var]] != 0) %>% select(Lineage, Family, Genus, {{var}})
	common_lineage <- intersect(unique(df1_var$Lineage), unique(df2_var$Lineage))
	lineage_comparisons <- common_lineage %>% map_dfr(~{
		df1_lineage <- df1_var %>% filter(Lineage == .x)
		df2_lineage <- df2_var %>% filter(Lineage == .x)
		p_value <- wilcox.test(df1_lineage[[var]], df2_lineage[[var]])$p.value
		data.frame(Lineage = .x, Variable = var, 
					N_df1 = nrow(df1_lineage), Median_df1 = median(df1_lineage[[var]]),
					N_df2 = nrow(df2_lineage), Median_df2 = median(df2_lineage[[var]]),
					P_value = p_value)
	})
  
  return(lineage_comparisons)
}

variables <- c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
lineage_PC <- map_dfr(variables, compare_values, df1 = df1_PC, df2 = df2_PC)
lineage_PI <- map_dfr(variables, compare_values, df1 = df1_PI, df2 = df2_PI)

l_PC <- lineage_PC %>%
	select(Lineage, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
l_PC <- as.data.frame(l_PC)
l_PC <- rename(l_PC, A = Lineage)

l_PI <- lineage_PI %>%
	select(Lineage, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
l_PI <- as.data.frame(l_PI)
l_PI <- rename(l_PI, A = Lineage)

# ------------------------------------------------------------------------------------------------ #

# Comparissons for Family #

# ------------------------------------------------------------------------------------------------ #

compare_values <- function(var, df1, df2) {
	df1_var <- df1 %>% filter(df1[[var]] != 0) %>% select(Family, Genus, {{var}})
	df2_var <- df2 %>% filter(df2[[var]] != 0) %>% select(Family, Genus, {{var}})
	common_families <- intersect(unique(df1_var$Family), unique(df2_var$Family))
	family_comparisons <- common_families %>% map_dfr(~{
		df1_family <- df1_var %>% filter(Family == .x)
		df2_family <- df2_var %>% filter(Family == .x)
		p_value <- wilcox.test(df1_family[[var]], df2_family[[var]])$p.value
		data.frame(Family = .x, Variable = var, 
					N_df1 = nrow(df1_family), Median_df1 = median(df1_family[[var]]),
					N_df2 = nrow(df2_family), Median_df2 = median(df2_family[[var]]),
					P_value = p_value)
	})
  
	return(family_comparisons)
}

variables <- c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
family_PC <- map_dfr(variables, compare_values, df1 = df1_PC, df2 = df2_PC)
family_PI <- map_dfr(variables, compare_values, df1 = df1_PI, df2 = df2_PI)

f_PC <- family_PC %>%
	select(Family, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
f_PC <- as.data.frame(f_PC)
f_PC <- rename(f_PC, A = Family)

f_PI <- family_PI %>%
	select(Family, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
f_PI <- as.data.frame(f_PI)
f_PI <- rename(f_PI, A = Family)

# ------------------------------------------------------------------------------------------------ #

# Comparissons for Genus #

# ------------------------------------------------------------------------------------------------ #

compare_values <- function(var, df1, df2) {
	df1_var <- df1 %>% filter(df1[[var]] != 0) %>% select(Family, Genus, {{var}})
	df2_var <- df2 %>% filter(df2[[var]] != 0) %>% select(Family, Genus, {{var}})
	common_genus <- intersect(unique(df1_var$Genus), unique(df2_var$Genus))
	genus_comparisons <- common_genus %>% map_dfr(~{
		df1_genus <- df1_var %>% filter(Genus == .x)
		df2_genus <- df2_var %>% filter(Genus == .x)
		p_value <- wilcox.test(df1_genus[[var]], df2_genus[[var]])$p.value
		data.frame(Genus = .x, Variable = var, 
					N_df1 = nrow(df1_genus), Median_df1 = median(df1_genus[[var]]),
					N_df2 = nrow(df2_genus), Median_df2 = median(df2_genus[[var]]),
					P_value = p_value)
	})
  
	return(genus_comparisons)
}

variables <- c("Cal", "Cvel", "Doxy", "Iro", "Nit", "pH", "Pho", "Sal", "Sil", "Tem")
genus_PC <- map_dfr(variables, compare_values, df1 = df1_PC, df2 = df2_PC)
genus_PI <- map_dfr(variables, compare_values, df1 = df1_PI, df2 = df2_PI)

g_PC <- genus_PC %>%
	select(Genus, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
g_PC <- as.data.frame(g_PC)
g_PC <- rename(g_PC, A = Genus)

g_PI <- genus_PI %>%
	select(Genus, Variable, P_value) %>%
	pivot_wider(names_from = Variable, values_from = P_value)
g_PI <- as.data.frame(g_PI)
g_PI <- rename(g_PI, A = Genus)

# ------------------------------------------------------------------------------------------------ #

# Join data and save #

# ------------------------------------------------------------------------------------------------ #

PC <- rbind(l_PC, f_PC, g_PC)
PI <- rbind(l_PI, f_PI, g_PI)

setwd("D:/6_Inputs")
write.csv(PC, "6_comparison_results_PC.csv", row.names = FALSE)
write.csv(PI, "7_comparison_results_PI.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
