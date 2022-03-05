# The impact of company's action plans on work-related stress in Italy

Data for Italy from two different European-level surveys that each used a multistage stratified random sampling design was used. The first was the second European Survey of Enterprises on New and Emerging Risks (ESENER-2) that was carried out in 2014 which records how health and safety is organized at workplaces across 36 European countries (EU-OSHA, 2016). The survey encompasses public and private establishments with more than five employees, with “the person who knows best about health and safety in this establishment” through computer assisted telephone interviewing.   

The second data source was the sixth European Working Conditions Survey (EWCS; Eurofound, 2017). Based on face-to-face interviews in 2015 from 35 European countries, the survey covers a range of employment statuses, working conditions, and worker health to capture the multifaceted dimensions of work in Europe. 

Statistical analysis was conducted according to the following four-stage approach:

1. First, all items in the datasets were standardised as indexes ranging from 0 to 100. 
2. Second, confirmatory factor analysis was implemented on the job demands and job resources factors at the individual level from the Sixth EWCS. Categorical least squares (cat-ULSMV) estimation procedure was used to fit the data to the proposed model. The means of these items were then calculated to obtain an index score per person each for job demands and job resources, as well as each respective sub-factor. 
3. Third, the ESENER-2 and the Sixth EWCS were linked. Responses from ESENER-2 were aggregated to the national, industry and company size level to create a company action plan index. The index is calculated considering companies with below 9 employees, 10-249 and 250+ employees within a specific sector (e.g., construction).  For those groups of company with missing data (e.g., below 9 employees in a particular industry), the industry index was used. This index was assigned to each respondent of the Sixth EWCS following the same criteria. This implies that each individual employees in the Sixth EWCS was assigned the index score calculated from the ESENER-2 which is specific to the company size and industry they belong to. 
4. Four, a path analysis in structural equation model (SEM) was then fitted using the maximum likelihood estimation with robust standard errors (MLR). Bias corrected bootstrapping (set at 1,000 at 95% confidence intervals) to simulate the sampling distribution of the indirect effects was used. These tested the confident intervals for the indirect effects between work-related stress action plans, job demands, job resources and individual level work-related stress.

# How to use this analysis
- Download the combined_stress.rda database
- Run the "01_load_packages" and the "05_analysis" scripts and load the database

There is no need to run the other scripts as they are only needed for creating the combined_stress database.
