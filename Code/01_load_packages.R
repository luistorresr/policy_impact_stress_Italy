########################### Load packages ##############################################

### R version
# R version 4.0.4

### R version
# R 4.0 or above

#### packages needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(sjlabelled)) install.packages("sjlabelled", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")
if(!require(lavaan)) install.packages("lavaan", repos = "http://cran.us.r-project.org")
if(!require(semTools)) install.packages("semTools", repos = "http://cran.us.r-project.org")
if(!require(semPlot)) install.packages("semPlot", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
if(!require(tidySEM)) install.packages("tidySEM", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(tidygraph)) install.packages("tidygraph", repos = "http://cran.us.r-project.org")
if(!require(ggraph)) install.packages("ggraph", repos = "http://cran.us.r-project.org")

library(lavaan) # CFA and SEM
library(semTools) # additional toold for CFA and SEM 
library(semPlot) # plots for CFA and SEM
library(sjmisc) # useful to set Na's
library(haven) # read SPSS, Stata, and SAS data
library(tidyverse) # several tools for data manipulation
library(Hmisc) # many functions useful for data analysis
library(sjlabelled) # get_labels function
library(corrr) # for correlations correlate()
library(psych) # descriptives
library(labelled) # working with labelled data
library(flextable) # formatting tables 
library(xlsx) # export data into excel
library(tidySEM) # for SEM plotting
library(kableExtra) # for SEM plotting
library(tidygraph) 
library(ggraph)

# End load packages