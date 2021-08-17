
# 1 # Cleaning the datasets: ESENER 2015 and EWCS2015
# 2 # Creating a policy dataset
# 3 # Standardising relevant variables for linking datasets
# 4 # filtering datasets for size, employees, and hours. 


# 1 # Cleaning the datasets: ESENER 2015 and EWCS2015


## EWCS2015 - for JDR model 

### loading the original dataset 

EWCS2015_base <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

l_EWCS2015_base <- get_labels(EWCS2015_base, values = "n") # value labels
q_EWCS2015_base <- as.data.frame(label(EWCS2015_base))


### items 

items_EWCS2015 <- c(
  
  # filtering variables
  
  "Q16b", # full company size (na 88, 99)
  "nace_rev2_1", # industry 
  
  "Q2a", # gender (dont know 9)
  "Q2b", # age as a numeric value (na 888, 999)
  "Q2c", # filter for 1 = in employment (na 88, 99)
  "Q7", # filter for 1 = employee (na 8 and 9)
  "Q2d", # 1 = full time, 2 = part-time (na 8 and 9)
  "Q24", # filter for >= 20 hours per week  (na 888, 999)
  "Q17", # years with your company
  
  # Linking variables 
  
  "Country", # country 

  # Job Demands
  
  # Emotional demands
  
  "Q30g", # Handling angry clients
  "Q30h", # Emotionally disturbing situations 
  "Q61o", # Your job requires that you hide your feelings?
  
  # Quantitative demands 
  
  "Q49a", # Working at very high speed
  "Q49b", # Working to tight deadlines 
  "Q61g", # You have enough time to get the job done?
  "Q51", # How often do you have to interrupt a task you are doing in order to take on an unforeseen task?
  
  # Pace determinants 
  "Q50a", 
  "Q50b",
  "Q50c",
  "Q50d",
  "Q50e",
  
  # Job resources
  
  # Social resources
  
  "Q61a", # Colleague social support index
  "Q70e", 
  "Q89d", 
  "Q61b", # Supervisor social support index
  "Q63a",
  "Q63b",
  "Q63c",
  "Q63d",
  "Q63e",
  "Q63f",
  
  # Work resources
  
  "Q54a", # Job control index 
  "Q54b", 
  "Q54c",
  "Q61f",
  "Q53c", # Skill discretion index
  "Q53e",
  "Q53f",
  "Q61i", 
  "Q61c", # Participation index
  "Q61d",
  "Q61n",
  
  # outcomes 
  
  "Q61m", # experience stress  
  "Q81c", # experience harassment  
  "Q81a" # experience violence
)

### select variables from EWCS 2015

EWCS2015_items <- EWCS2015_base %>% select(all_of(items_EWCS2015)) %>% as_tibble()

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels
q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))

## cleaning the variables

EWCS2015_sub1 <- EWCS2015_items %>% select(-c(Country, nace_rev2_1, Q17)) %>%
  set_na(na =c(8, 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

EWCS2015_sub1 <- EWCS2015_sub1 %>% set_na(na =c(Q61o = 7, Q61g = 7, Q61a = 7, Q70e = 7, Q89d = 7, Q61b = 7, Q63a = 7,
                                                Q63b = 7, Q63c = 7, Q63d = 7, Q63e = 7, Q63f = 7, Q61f = 7, Q61i = 7,
                                                Q61c = 7, Q61d = 7, Q61n = 7, Q61m = 7,
                                                Q50a = 7 , Q50b = 7, Q50c = 7, Q50d = 7, Q50e = 7,
                                                Q2b = c(888, 999), Q24 = c(888, 999), Q16b = c(88, 99)), 
                                          drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

EWCS2015_sub2 <- EWCS2015_items %>% 
  select(Country, nace_rev2_1, Q17) %>% 
  set_na(na = c(Q17 = c(77, 99, 88))) %>% 
  as_tibble(.)


## working dataset

EWCS2015_items <- cbind(EWCS2015_sub2, EWCS2015_sub1) # working dataset

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels
q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))


### Recode variables - the higher, the more

#### outcomes 

EWCS2015_items$Q81a <- dplyr::recode_factor(EWCS2015_items$Q81a, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q81a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)  # YES = 1, I have experienced physical violence

EWCS2015_items$Q81c <- dplyr::recode_factor(EWCS2015_items$Q81c, `1` = 100, `2` = 0)  %>% 
  replace_labels(EWCS2015_items$Q81c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1, I have experienced bullying/harassment

EWCS2015_items$Q61m <- dplyr::recode_factor(EWCS2015_items$Q61m, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61m, labels = c("Always" = 100,
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)# the higher the more stress

##### rename the outcomes variables

EWCS2015_items <- EWCS2015_items %>% rename(experience_stress = Q61m,
                                    experience_harassment = Q81c,
                                    experience_violence = Q81a)


#### recode and rename gender, age, size, sector, and job type

EWCS2015_items  <- EWCS2015_items %>% rename(country = Country, industry = nace_rev2_1, size_ewcs = Q16b, 
                                             sex = Q2a, age = Q2b, 
                                             employment_status = Q2c, worker_type = Q7, contract = Q2d, hours = Q24,
                                             experience = Q17) 

EWCS2015_items$sex <- dplyr::recode_factor(EWCS2015_items$sex, `1` = 0, `2` = 1) %>% 
  replace_labels(EWCS2015_items$sex, labels = c("Female" = 1, "Male" = 0)) %>% as_numeric(., drop.levels = TRUE)  # female = 1

EWCS2015_items$contract <- dplyr::recode_factor(EWCS2015_items$contract, `1` = 0, `2` = 1) %>% 
  replace_labels(EWCS2015_items$contract, labels = c("Full time" = 1, "Part time" = 0)) %>% as_numeric(., drop.levels = TRUE)  # full time = 1

EWCS2015_items$experience <- dplyr::recode(EWCS2015_items$experience, `999` = 0) %>% 
  as_numeric(., drop.levels = TRUE)  # less than a year = 0

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # update value labels


#### job  resources

##### work resources 
###### job control index
EWCS2015_items$Q54a <- dplyr::recode_factor(EWCS2015_items$Q54a, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54b <- dplyr::recode_factor(EWCS2015_items$Q54b, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54c <- dplyr::recode_factor(EWCS2015_items$Q54c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61f <- dplyr::recode_factor(EWCS2015_items$Q61f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61f, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Skill discretion index
EWCS2015_items$Q53c <- dplyr::recode_factor(EWCS2015_items$Q53c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53e <- dplyr::recode_factor(EWCS2015_items$Q53e, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53f <- dplyr::recode_factor(EWCS2015_items$Q53f, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53f, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61i <- dplyr::recode_factor(EWCS2015_items$Q61i, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61i, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                  "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Participation index
EWCS2015_items$Q61c <- dplyr::recode_factor(EWCS2015_items$Q61c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61c, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61d <- dplyr::recode_factor(EWCS2015_items$Q61d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61d, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61n <- dplyr::recode_factor(EWCS2015_items$Q61n, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61n, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### social resources
###### Colleague social support index
EWCS2015_items$Q61a <- dplyr::recode_factor(EWCS2015_items$Q61a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61a, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q70e <- dplyr::recode_factor(EWCS2015_items$Q70e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q70e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q89d <- dplyr::recode_factor(EWCS2015_items$Q89d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q89d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Supervisor social support index
EWCS2015_items$Q61b <- dplyr::recode_factor(EWCS2015_items$Q61b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61b, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63a <- dplyr::recode_factor(EWCS2015_items$Q63a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63a, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63b <- dplyr::recode_factor(EWCS2015_items$Q63b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63b, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63c <- dplyr::recode_factor(EWCS2015_items$Q63c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63c, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                  "Tend to disagree" = 25, 
                                                  "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63d <- dplyr::recode_factor(EWCS2015_items$Q63d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63e <- dplyr::recode_factor(EWCS2015_items$Q63e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63f <- dplyr::recode_factor(EWCS2015_items$Q63f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63f, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

#### job demands
##### emotional demands
EWCS2015_items$Q30g <- dplyr::recode_factor(EWCS2015_items$Q30g, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30g, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q30h <- dplyr::recode_factor(EWCS2015_items$Q30h, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30h, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61o <- dplyr::recode_factor(EWCS2015_items$Q61o, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61o, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Quantitative demands 
EWCS2015_items$Q49a <- dplyr::recode_factor(EWCS2015_items$Q49a, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49a, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q49b <- dplyr::recode_factor(EWCS2015_items$Q49b, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49b, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)


EWCS2015_items$Q51 <- dplyr::recode_factor(EWCS2015_items$Q51, `1` = 100, `2` = 70, `3` = 30, `4` = 0) %>% 
  replace_labels(EWCS2015_items$Q51, labels = c("Very often" = 100, 
                                                "Fairly often" = 70, 
                                                "Occasionally" = 30, 
                                                "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61g <- dplyr::recode_factor(EWCS2015_items$Q61g, `1` = 0, `2` = 25, `3` = 50, `4` = 75, `5` = 100) %>% 
  replace_labels(EWCS2015_items$Q61g, labels = c("Never" = 100, 
                                                 "Rarely" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Most of the time" = 25, 
                                                 "Always" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Pace determinats
EWCS2015_items$Q50a <- dplyr::recode_factor(EWCS2015_items$Q50a,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50b <- dplyr::recode_factor(EWCS2015_items$Q50b,`1` = 100, `2` = 0) %>% 
   replace_labels(EWCS2015_items$Q50b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50c <- dplyr::recode_factor(EWCS2015_items$Q50c,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50d <- dplyr::recode_factor(EWCS2015_items$Q50d,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50d, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50e <- dplyr::recode_factor(EWCS2015_items$Q50e,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

### updating variable names and values list

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels


#####################################################

## ESENER 2014 - To estimate organisational practices

### Load the original dataset

ESENER14_base <- read_spss("./Data/UKDA-8690-spss/spss/spss19/esener2_data_q100rev.sav")

l_ESENER14_base <- get_labels(ESENER14_base, values = "n") # value labels
q_ESENER14_base <- as.data.frame(label(ESENER14_base)) # questions

### Selecting items to estimate organisational practices

items_ESENER14 <- c(
  
  # merging variables
  
  "country",
  
  # filtering variable 
  
  "Size", # full company size based on directly on directly employed people (2 = 10-49)
  "Nace1", # industry 
  
  # Organisational practices indicators
  
  "Q300", # Does your establishment have an action plan to prevent work-related stress? [If Q104 >19 and <99999]
  "Q301", # Is there a procedure in place to deal with possible cases of bullying or harassment? [If Q104 >19 and <99999]
  "Q302" # And is there a procedure to deal with possible cases of threats, abuse or assaults by clients, patients, pupils or other external persons? [If Q104 >19 and <99999 and Q201_5 = 1]
)


### selecting the variables from the original dataset

ESENER14_items <- ESENER14_base %>% select(all_of(items_ESENER14)) %>% as_tibble()

### rename the variables

ESENER14_items <- ESENER14_items %>% 
  dplyr::rename(size_esener = Size, industry = Nace1,
                practice_stress = Q300, practice_harassment = Q301, practice_violence = Q302)


### extracting variable labels 

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels
q_ESENER14_items <- as.data.frame(label(ESENER14_items)) # questions


### creating no answers as NA's only for items

ESENER14_sub1 <- ESENER14_items %>% select(country, size_esener, industry) %>% 
  set_na(na = c(industry = 99), drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)  

ESENER14_sub2 <- ESENER14_items %>% select(-c(country, size_esener, industry)) %>% 
  set_na(na = c(practice_stress = 9, practice_harassment = 9, practice_violence = 9), 
         drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

ESENER14_items <- cbind(ESENER14_sub1, ESENER14_sub2)

### recoding variables (yes = 1, no = 0)

ESENER14_items$practice_stress <- dplyr::recode_factor(ESENER14_items$practice_stress, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_stress, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

ESENER14_items$practice_harassment <- dplyr::recode_factor(ESENER14_items$practice_harassment, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_harassment, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

ESENER14_items$practice_violence <- dplyr::recode_factor(ESENER14_items$practice_violence, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_violence, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels


# 2 # Standardising relevant variables for linking datasets

## Country - taking EWCS2015 as the baseline values

val_labels(EWCS2015_items$country)
val_labels(ESENER14_items$country)

ESENER14_items$country <- dplyr::recode_factor(ESENER14_items$country, `1` = 35, `2` = 1, `3` = 2, `4` = 3, `5` = 34, `6` = 5, `7` = 6, `8` = 11, 
                                               `9` = 7, `10`= 8, `11`= 12, `12`= 26, `13`= 9, `14`= 10, `15`= 4, `16`= 13, `17`= 14, `18`= 999, 
                                               `19`= 15, `20`= 17, `21`= 18, `22`= 16, `23`= 29, `24`= 30, `25`= 19, `26`= 20, `27`= 33, `28`= 21,
                                               `29`= 22, `30`= 23, `31`= 31, `32`= 27, `33`= 25, `34`= 24, `35`= 32, `36`= 28) 

ESENER14_items$country <- replace_labels(ESENER14_items$country, labels = c( "Albania"=35, "Austria"=1, "Belgium"=2, "Bulgaria"=3, 
                                                                             "Switzerland"=34, "Cyprus"=5, "Czech Republic"=6, "Germany"=11, 
                                                                             "Denmark"=7, "Estonia"=8, "Greece"=12, "Spain"=26, "Finland"=9, 
                                                                             "France"=10, "Croatia"= 4, "Hungary"=13, "Ireland"=14, "Iceland"=999,
                                                                             "Italy"=15, "Lithuania"=17, "Luxembourg"=18, "Latvia"=16, "Montenegro"=29, 
                                                                             "FYROM"=30, "Malta"=19, "Netherlands"=20, "Norway"=33, "Poland"=21, 
                                                                             "Portugal"=22, "Romania"=23, "Serbia"=31, "Sweden"=27, "Slovenia"=25, 
                                                                             "Slovakia"=24, "Turkey"=32, "UK"=28)) %>% as_labelled()


## company size - ESENER and EWCS need to be standardised 

val_labels(ESENER14_items$size_esener)
val_labels(EWCS2015_items$size_ewcs)

ESENER14_items$size_esener <- dplyr::recode_factor(ESENER14_items$size_esener, `1` = 1, `2` = 2, `3` = 2, `4` = 3) %>% 
  replace_labels(ESENER14_items$size_esener, labels = c("9 or less" = 1, "10-249" = 2, "250+" = 3)) %>% as_labelled()

EWCS2015_items$size_ewcs <- dplyr::recode_factor(EWCS2015_items$size_ewcs, `1` = 1, `2` = 1, `3` = 2, `4` = 3) %>% 
  replace_labels(EWCS2015_items$size_ewcs, labels = c("9 or less" = 1, "10-249" = 2, "250+" = 3)) %>% as_labelled()


## industry

val_labels(ESENER14_items$industry)
val_labels(EWCS2015_items$industry)

ESENER14_items$industry <- replace_labels(ESENER14_items$industry, labels = c("A Agriculture, forestry and fishing" = 1 ,
                                                                              "B Mining and quarrying" = 2,
                                                                              "C Manufacturing" = 3,
                                                                              "D Electricity, gas, steam and air conditioning supply" = 4,
                                                                              "E Water supply; sewerage, waste management and remediation activities" = 5,
                                                                              "F Construction" = 6,
                                                                              "G Wholesale and retail trade; repair of motor vehicles and motorcycles" = 7,
                                                                              "H Transportation and storage" = 8,
                                                                              "I Accommodation and food service activities" = 9,
                                                                              "J Information and communication" = 10,
                                                                              "K Financial and insurance activities" = 11,
                                                                              "L Real estate activities" = 12,
                                                                              "M Professional, scientific and technical activities" = 13,
                                                                              "N Administrative and support service activities" = 14,
                                                                              "O Public administration and defence; compulsory social security" = 15,
                                                                              "P Education" = 16,
                                                                              "Q Human health and social work activities" = 17,
                                                                              "R Arts, entertainment and recreation" = 18,
                                                                              "S Other service activities" = 19)) %>% as_labelled()


### Selecting Italy data

#### ESENER

ESENER14_filtered <- ESENER14_items %>% filter(country == 15) # Italy

#ESENER14_filtered <- ESENER14_filtered  %>% filter(size_esener != 1) # equal or more than 10-49 employees

ESENER14_filtered %>% group_by(size_esener) %>% summarise(total = n())  # summary per country

#### EWCS 

EWCS2015_filtered <- EWCS2015_items %>% filter(country == 15) # Italy

EWCS2015_filtered <- EWCS2015_filtered %>% filter(employment_status == 1 & worker_type == 1) # in employment, employee, 

#EWCS2015_filtered <- EWCS2015_filtered %>% filter(hours >= 20) # 20 or more hours

#EWCS2015_filtered <- EWCS2015_filtered %>% filter(size_ewcs != 1) #  company size

EWCS2015_filtered %>% group_by(size_ewcs) %>% summarise(total = n())  # summary per country

EWCS2015_filtered %>% count(sex) %>% summarise(percentage = n / sum(n) * 100) # sex percentage

EWCS2015_filtered %>% filter(!is.na(age)) %>% summarise(mean_age = mean(age), sd_age = sd(age)) # sex percentage

### saving ready to link datasets

JDR_ready <- EWCS2015_filtered

practices_ready <- ESENER14_filtered

save(JDR_ready, file = "./Data/Working_data/JDR_ready.rda") # r data for CFA

save(practices_ready, file = "./Data/Working_data/practices_ready.rda") # r data

# End data wrangling 

#rm(list = ls())

