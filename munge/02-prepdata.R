cleandata <- cleandata %>%
  mutate(country = `Country.(1-SE,.2-NL,.3-IE)`, 
         Neonatal.morbidity = `Neonatal.morbidity/baby.death`) %>% 
  mutate(across(!c("Age",  
                   "BMI", "SBPav", "DBPav"), as.factor))