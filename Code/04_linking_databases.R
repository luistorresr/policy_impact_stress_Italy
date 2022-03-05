
load("./Data/Working_data/practices_ready.rda") 
load("./Data/Working_data/JDR_clean.rda") 

# working databases 

practices_stress <- practices_ready 

JDR_stress_outcome <- JDR_clean

rm(JDR_clean, practices_ready)


### Calculating country index for stress practices in the ESENER

practices_stress <- practices_stress %>% rename(size = size_esener) # change size name

practices_industry <- practices_stress  %>% 
  group_by(industry2) %>%
  summarise(industry_index = mean(na.omit(practice_stress)))  # per industry and size

practices_size <- practices_stress  %>% 
  group_by(industry2, size) %>%
  summarise(csize_index = mean(na.omit(practice_stress)))  # per industry and size


## Joining tables with replacement of NA 

practices_index <- left_join(practices_industry, practices_size, by = "industry2")

practices_index$csize_index <- ifelse(is.na(practices_index$csize_index), 
                                      practices_index$industry_index, practices_index$csize_index) # replace "NA" with industry index


### Renaming the JDR database from the EWCS

JDR_stress_outcome <- JDR_stress_outcome %>% rename(size = size_ewcs)


## Combine databases taking EWCS as the target database

combined_stress <- left_join(JDR_stress_outcome, practices_index, by = c("industry2", "size"))

combined_stress <- left_join(combined_stress, practices_industry, by = "industry2")

combined_stress <- combined_stress %>% rename(industry_index = industry_index.y) 

combined_stress$csize_index <- ifelse(is.na(combined_stress$csize_index), 
                               combined_stress$industry_index, 
                               combined_stress$csize_index) # replace NA with industry index

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels

combined_stress <- combined_stress %>% select(industry2, sex, hours, contract, age, size, experience_stress,
                                              avg_demands, avg_resources, industry_index, csize_index)

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels


save(combined_stress, file = "./Data/Working_data/combined_stress.rda") # r data


### delete workspace

rm(list = ls())

