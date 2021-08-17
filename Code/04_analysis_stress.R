
load("./Data/Working_data/practices_ready.rda") 
load("./Data/Working_data/JDR_clean.rda") 


## Selecting only stress relevant variables 

practices_stress <- practices_ready %>% select(-c(practice_harassment, practice_violence))

rm(practices_ready)

### from JDR ready

JDR_stress_outcome <- JDR_clean %>% select(-c(experience_harassment, experience_violence))

rm(JDR_clean)


## Calculating country index for stress practices in the ESENER

practices_stress <- practices_stress %>% rename(size = size_esener) # change size name

practices_country <- practices_stress %>% 
  group_by(country) %>%
  summarise(country_index = mean(na.omit(practice_stress))) # per country

practices_industry <- practices_stress  %>% 
  group_by(country, industry) %>%
  summarise(industry_index = mean(na.omit(practice_stress))) # per country and industry

practices_size <- practices_stress  %>% 
  group_by(country, industry, size) %>%
  summarise(csize_index = mean(na.omit(practice_stress)))  # per country, industry and size


## Joining tables with replacement of NA following if industry NA = country; if size NA = industry.

practices_index_temp <- left_join(practices_country, practices_industry, by = "country")

practices_index_temp$industry_index <- ifelse(is.na(practices_index_temp$industry_index), 
                                          practices_index_temp$country_index, practices_index_temp$industry_index) # replace "NA" in industry


practices_index <- left_join(practices_index_temp, practices_size, by = c("country", "industry"))

practices_index$csize_index <- ifelse(is.na(practices_index$csize_index), 
                                          practices_index$industry_index, practices_index$csize_index) # replace "NA" in industry


### linking practices and JDR by country, industry and size

JDR_stress_outcome <- JDR_stress_outcome %>% rename(size = size_ewcs)

combined_stress <- left_join(JDR_stress_outcome, practices_index, by = c("country", "industry", "size"))

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels

save(combined_stress, file = "./Data/Working_data/combined_stress .rda") # r data


### delete not useful datasets from memory

rm(JDR_stress_outcome, practices_country, practices_index, practices_index_temp, practices_industry,
   practices_size, practices_stress)


# Correlation matrix

options(digits = 4)

stress_corr <- combined_stress %>% select(csize_index, experience_stress,
                                         avg_demands, avg_resources,
                                         hours, age, experience, size, sex) %>% 
  as.matrix(.) %>%
  rcorr(., type = c("pearson"))

knitr::kable(stress_corr$r) %>% kable_styling()
knitr::kable(stress_corr$P) %>% kable_styling()


# descriptives

options(digits = 3)

combined_stress %>% summarise(mean = mean(na.omit(c(csize_index))), sd = sd(na.omit(csize_index)))
combined_stress %>% summarise(mean = mean(na.omit(experience_stress)), sd = sd(na.omit(experience_stress)))
combined_stress %>% summarise(mean = mean(na.omit(avg_demands)), sd = sd(na.omit(avg_demands)))
combined_stress %>% summarise(mean = mean(na.omit(avg_resources)), sd = sd(na.omit(avg_resources)))
combined_stress %>% summarise(mean = mean(na.omit(hours)), sd = sd(na.omit(hours)))
combined_stress %>% summarise(mean = mean(na.omit(age)), sd = sd(na.omit(age)))
combined_stress %>% summarise(mean = mean(na.omit(experience)), sd = sd(na.omit(experience)))


# Models

## Model 1: using the resource and demand averages


model_stress_1 <- '
                   # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ d1*csize_index
                            
                        
                        # JDR on outcome
                            experience_stress ~ i2*avg_demands
                            experience_stress ~ i3*avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ i4*csize_index
                            avg_resources ~ i5*csize_index
                     
                     # indirect effect
                            ind_demands := i2*i4
                            ind_resources := i3*i5
                        
                     # total effect
                            tot_demands := d1 + (i2*i4)
                            tot_resources := d1 + (i3*i5) 
                    
                    # controls 
                            csize_index ~ size
                            experience_stress ~ size + hours + experience + age
                            avg_demands ~ size + hours + experience + age
                            avg_resources ~ size + hours + experience + age

                    # covariances 
                            avg_demands ~~ avg_resources '

stress_path_1 <- sem(model_stress_1, data = combined_stress, estimator = "MLR", 
                   group = NULL, 
                   se = "robust",
                   test = "boot",
                   bootstrap = 1000) ##### bias corrected bootstrap

summary(stress_path_1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
standardizedsolution(stress_path_1)



## Model 2: Mediation analysis 

model_stress_2 <- '
                   # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ d1*csize_index
                            
                        
                        # JDR on outcome
                            experience_stress ~ i2*avg_demands
                            experience_stress ~ i3*avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ i4*csize_index
                            avg_resources ~ i5*csize_index
                        
                     # indirect effect
                            ind_demands := i2*i4
                            ind_resources := i3*i5
                        
                     # total effect
                            tot_demands := d1 + (i2*i4)
                            tot_resources := d1 + (i3*i5) 
                    
                    # controls 
                            csize_index ~ size
                            experience_stress ~ size + hours + experience + age
                            avg_demands ~ size + hours + experience + age
                            avg_resources ~ size + hours + experience + age

                    # covariances 
                            avg_demands ~~ avg_resources '

stress_path_2 <- sem(model_stress_2, data = combined_stress, estimator = "MLR", 
                     group = "sex", 
                     group.label = 1,
                     se = "robust",
                     test = "boot",
                     bootstrap = 1000) ##### bias corrected bootstrap

summary(stress_path_2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(stress_path_2)





