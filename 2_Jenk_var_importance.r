
# ------------------------------------------------------------------------------------------------ #

### Title: High relative important variables  ####
### Author: Patrón-Rivero, C. ####
### Project: "Global analysis of the influence of environmental variables to explain distributions #
### and realized thermal niche boundaries of sea snakes" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(BAMMtools)

# ------------------------------------------------------------------------------------------------ #

# Estimate the natural Jenk's breaks by taxonomic level and resolution #

# ------------------------------------------------------------------------------------------------ #

vars <- read.csv("D:/SSnks_Var_Therm/Submission/Mar_env_research/Var_import.csv")

breaks <- getJenksBreaks(vars[, 2], 3) # lineage at 5 arc-minutes
subset(vars[, 1:2], All_5 >= breaks[2])

breaks <- getJenksBreaks(vars[, 3], 3) # lineage at 10 arc-minutes
subset(vars[, c(1, 3)], All_10 >= breaks[2])

breaks <- getJenksBreaks(vars[, 4], 3) # Subf Hyd at 5 arc-minutes
subset(vars[, c(1, 4)], H_5 >= breaks[2])

breaks <- getJenksBreaks(vars[, 5], 3) # Subf Hyd at 10 arc-minutes
subset(vars[, c(1, 5)], H_10 >= breaks[2])

breaks <- getJenksBreaks(vars[, 6], 3) # Subf Lat at 5 arc-minutes
subset(vars[, c(1, 6)], L_5 >= breaks[2])

breaks <- getJenksBreaks(vars[, 7], 3) # Subf Lat at 10 arc-minutes
subset(vars[, c(1, 7)], L_10 >= breaks[2])

# ------------------------------------------------------------------------------------------------ #

### EndNotRun

