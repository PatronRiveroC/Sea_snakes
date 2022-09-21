# ------------------------------------------------------------------------------------------------ #

### Title: Jackknife partitioning loop ####
### Author: Patron-Rivero, C. ####
### Date: 06/22/2022 ###
### Project: "Jackknife function" ###

# ------------------------------------------------------------------------------------------------ #

# Inputs ##

# ------------------------------------------------------------------------------------------------ #

jackk_sea_snakes <- function(csv_dir, train_dir, test_dir){
  setwd(csv_dir)
  base <- list.files(pattern = "csv")
  for (i in 1:length(base)){
    data <- read.csv(base[[i]], header = T)
    for (j in 1:nrow(data)){
      x <- data[-j, ]
      y <- data[j, ]
      x[[i]] <- sub(".csv*", "", base[[i]])
      setwd(train_dir)
      write.csv(x, paste(base[[i]], j, "train.csv", sep = "_"), row.names = F)
      setwd(test_dir)
      write.csv(y, paste(base[[i]], j, "test.csv", sep = "_"), row.names = F)
      setwd(csv_dir)
    }
  }

}

#jackk_sea_snakes(csv, train, test)

# ------------------------------------------------------------------------------------------------ #

## End Not Run

