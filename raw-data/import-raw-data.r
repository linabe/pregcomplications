
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# Import data -------------------------------------------------------------

cleandata <- read.xlsx("./raw-data/IMPROVED cleaned 20210421.xlsx")
# rawdata <- read.xlsx("./raw-data/210303 - IMPROVED raw w dictionary.xlsx")

# Fix encoding ------------------------------------------------------------

names(cleandata) <- vctrs::vec_as_names(names(cleandata), repair = "unique")
cleandata <- clean_data(cleandata)

cleandata <- cleandata %>% 
  mutate_if(is.character, list(~gsub("€", "E", .))) 

# Store as RData in /data folder ------------------------------------------

save(file = "./data/rawData.RData", list = c("cleandata"))
