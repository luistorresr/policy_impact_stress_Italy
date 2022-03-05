
# Analysis

load("./Data/Working_data/combined_stress.rda") 

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels

combined_stress %>% nrow() # sample size


# Initial checks

## Correlation matrix

options(digits = 2)

stress_corr <- combined_stress %>% select(csize_index, experience_stress,
                                         avg_demands, avg_resources, size, sex) %>% 
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


# outcome data assessments for linearity and normality

## outcome variable assessment 

boxplot(combined_stress$experience_stress) # outliers in the outcome variables
out <- boxplot.stats(combined_stress$experience_stress)$out
out_ind <- which(combined_stress$experience_stress %in% c(out))
hist(combined_stress$experience_stress, main = "Histogram")

ggplot(combined_stress, aes(x=experience_stress)) + geom_density() # distribution of the the outcome variables
ggplot(combined_stress, aes(sample= scale(experience_stress))) + geom_qq() + geom_abline() # linearity of the the outcome variables


## Skewness 
#### If skewness value:  > +1 or < -1 = highly skewed 
####                    Between +0.5 to -0.5 = moderately skewed
####                    0 = data is symmetric

skewness(na.omit(combined_stress$experience_stress)) # moderately skewed
hist(combined_stress$experience_stress, main = "Histogram") # distribution

skewness(na.omit(combined_stress$csize_index)) # moderately skewed
hist(combined_stress$csize_index, main = "Histogram")

skewness(na.omit(combined_stress$avg_demands)) # moderately skewed
hist(combined_stress$avg_demands, main = "Histogram")

skewness(na.omit(combined_stress$avg_resources)) # moderately skewed (negative)
hist(combined_stress$avg_resources, main = "Histogram")

## regression assumptions 

linear_index <- gvlma.form(experience_stress ~ csize_index, combined_stress) # linear assumptions
linear_demands <- gvlma.form(experience_stress ~ avg_demands, combined_stress) # linear assumptions
linear_resources <- gvlma.form(experience_stress ~ avg_resources, combined_stress) # linear assumptions
linear_cdemands <- gvlma.form(avg_demands ~ csize_index, combined_stress) # linear assumptions
linear_cresources <- gvlma.form(avg_resources ~ csize_index, combined_stress) # linear assumptions

plot.gvlma(linear_index)
plot.gvlma(linear_demands)
plot.gvlma(linear_resources)
plot.gvlma(linear_cdemands)
plot.gvlma(linear_cresources)


# Models

## Model 1: using the resource and demand averages

model_stress_1 <- '
                   # direct effect
      
                        # practice on stress
                            
                            experience_stress ~ csize_index + avg_demands + avg_resources
                            
                        # organisational practices on JDR
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                        csize_index ~~ size   
                        avg_demands ~~ avg_resources '

stress_path_1 <- sem(model_stress_1, data = combined_stress, estimator = "MLR", 
                     model.type = "sem",
                     group = NULL, 
                     se="robust",
                     test = "bootstrap",
                     std.ov = TRUE,
                     missing = "listwise",
                     bootstrap = 1000) ##### bias corrected bootstrap

summary(stress_path_1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(stress_path_1)


## Model 2: Group comparison 

combined_stress_2 <- combined_stress %>% filter(contract == 1) # only full time

stress_path_2 <- sem(model_stress_1, data = combined_stress_2, estimator = "MLR", 
                     group = "sex", 
                     group.label = NULL,
                     se = "robust",
                     test = "bootstrap",
                     std.ov = TRUE,
                     missing = "listwise",
                     warn = FALSE,
                     bootstrap = 1000)

summary(stress_path_2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(stress_path_2)


## Free vs constrained model - overall

free__model <- sem(model_stress_1, data = combined_stress_2, estimator = "MLR",
                   group = "sex",
                   group.label = NULL,
                   se = "robust",
                   std.ov = TRUE,
                   missing = "listwise",
                   test = "bootstrap",
                   warn = FALSE,
                   bootstrap = 1000) # bias corrected bootstrap

constrained__model <- sem(model_stress_1, data = combined_stress_2, estimator = "MLR",
                          group = "sex", 
                          group.label = NULL,
                          se = "robust",
                          test = "bootstrap",
                          warn = FALSE,
                          bootstrap = 1000, # bias corrected bootstrap
                          group.equal = c("intercepts"))

anova(free__model, constrained__model) # scaled chi-squared difference test


## Free vs constrained model - detailed

#### index to stress - not significant 

model_index_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ 0*csize_index + avg_demands + avg_resources
                            
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources 
                              '

index_stress__path <- sem(model_index_stress, data = combined_stress_2, estimator = "MLR",
                          group = "sex",
                          group.label = NULL,
                          se = "robust",
                          test = "bootstrap",
                          warn = FALSE,
                          bootstrap = 1000) # bias corrected bootstrap

summary(index_stress__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

anova(free__model, index_stress__path) # scaled chi-squared difference test


### demands path

#### index to demands - no significant

model_index_demands  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + avg_demands + avg_resources
                            
                        # organisational practices on JDR
                        
                            avg_demands ~ 0*csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


index_demands__path <- sem(model_index_demands, data = combined_stress_2, estimator = "MLR",
                           group = "sex",
                           group.label = NULL,
                           std.ov = TRUE,
                           se = "robust",
                           test = "bootstrap",
                           warn = FALSE,
                           bootstrap = 1000) # bias corrected bootstrap

anova(free__model, index_demands__path) # scaled chi-squared difference test


#### demands to stress - significant

model_demands_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + 0*avg_demands + avg_resources
                    
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


demands_stress__path <- sem(model_demands_stress, data = combined_stress_2, estimator = "MLR",
                            group = "sex",
                            group.label = NULL,
                            se = "robust",
                            std.ov = TRUE,
                            warn = FALSE,
                            test = "bootstrap",
                            bootstrap = 1000) # bias corrected bootstrap

anova(free__model, demands_stress__path) # scaled chi-squared difference test


#### full demands path - significant

model_full_demands  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + 0*avg_demands + avg_resources
                          
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ 0*csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


full_demands_path <- sem(model_full_demands, data = combined_stress_2, estimator = "MLR",
                         group = "sex",
                         group.label = NULL,
                         se = "robust",
                         std.ov = TRUE,
                         warn = FALSE,
                         test = "bootstrap",
                         bootstrap = 1000) # bias corrected bootstrap

lavTestLRT(free__model, full_demands_path) # scaled chi-squared difference test


### resources path

#### index to resources - Significant 

model_index_resources  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + avg_demands + avg_resources
                            
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ 0*csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


index_resources__path <- sem(model_index_resources, data = combined_stress_2, estimator = "MLR",
                             group = "sex",
                             group.label = NULL,
                             se = "robust",
                             std.ov = TRUE,
                             test = "bootstrap",
                             warn = FALSE,
                             bootstrap = 1000) # bias corrected bootstrap

anova(free__model, index_resources__path) # scaled chi-squared difference test



#### resources to stress - No significant 

model_resources_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + avg_demands + 0*avg_resources
                            
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


resources_stress__path <- sem(model_resources_stress, data = combined_stress_2, estimator = "MLR",
                              group = "sex",
                              group.label = NULL,
                              se = "robust",
                              std.ov = TRUE,
                              warn = FALSE,
                              test = "bootstrap",
                              bootstrap = 1000) # bias corrected bootstrap

anova(free__model, resources_stress__path) # scaled chi-squared difference test


#### full resources path - Significant 

model_full_resources  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index + avg_demands + 0*avg_resources

                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ 0*csize_index

                    # covariances 
                            csize_index ~~ size
                            avg_demands ~~ avg_resources '


full_resources_path <- sem(model_full_resources, data = combined_stress_2, estimator = "MLR",
                           group = "sex",
                           group.label = NULL,
                           se = "robust",
                           std.ov = TRUE,
                           warn = FALSE,
                           test = "bootstrap",
                           bootstrap = 1000) # bias corrected bootstrap


anova(free__model, full_resources_path) # scaled chi-squared difference test



### delete workspace

rm(list = ls())

