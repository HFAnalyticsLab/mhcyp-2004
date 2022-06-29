# mhcyp-2004
Mental health of children & young people survey 2004

## Loading the data
The R scripts in this repo work with the MHCYP survey 2004, available from the UK Data Service [here](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5269)

load.R is the first script to run, and assumes you've downloaded the SPSS file in your parent folder. This also does all the feature engineering used in the other scripts.

## Data dictionary
This contains a data dictionary for the dataset, and the script used to pull this out from the SPSS file.

## Exploratory data analysis
There are 3 scripts in the EDA folder:
  - by_region.R compares prevalence across different areas of Great Britain
  - demographic_covariates.R compares prevalence across major demographic groups like age, sex, ethnicity and income
  - other_predictors.R
  
## Modelling
There are 2 scripts in the Modelling folder:
  - naive_model.R actually contains all the major model specifications and compares their performance
  - fingertips_approach.R looks at the variables used in the post-stratification used to make the localised prevalence estimates contained in [PHE Fingertips](https://fingertips.phe.org.uk/profile-group/mental-health/profile/cypmh/data#page/3/gid/1938133090/pat/6/par/E12000007/ati/102/are/E09000002/iid/91138/age/246/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1)
 
## Weights
These are 2 scripts exploring the use of survey weights and the adjustment factor recommended by the survey authors.
