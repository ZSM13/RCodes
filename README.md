RCodes
======

R Codes for daily, monthly and quarterly data cleaning


Append.R: R code that appends two csv files through the gtools package's smartbind function.

D1S-InsuranceMPT.R: Daily R code used to identify patients that belong to a specific insurance provider from data sets given to us by PCPs. This is to create a master record for each patient.

D2S-InsuranceUtilization.R: Daily R code used to identify patients that belong to a specific insurance provider from data sets given to us by PCPs. This is to create the patient's ED utilization record.

D4-Readmissions.R: Daily R code that allows us to cross check whether the patient's ED records already exist in our database and creates a file to import into our database if it does exist. This is to keep track of readmissions for each patient.

M1.R: R code we use monthly to clean data sets of active residents in public housing units

M4-Insurance.R: Data cleaning steps for the monthly capitation lists from insurance companies

Reporting.R: R code that cleans and merges data from many files for grant reporting

Triage.R: Cleans and prepares data for daily triage for our care management team

excelDatesFunction.R: If the variable is a factor, converts excel dates in the format Jan 01, 1990 to 1990-01-01