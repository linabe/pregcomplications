
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# Import data -------------------------------------------------------------

cleandata <- read.xlsx("./raw-data/201214 - IMPROVED cleaned and coded.xlsx")
rawdata <- read.xlsx("./raw-data/210303 - IMPROVED raw w dictionary.xlsx")

# Fix encoding ------------------------------------------------------------

cleandata <- clean_data(cleandata)

# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawData.RData", list = c("cleandata"))
