# Clean Chart of Accounts for TrackVia Upload

# Read in each sheet in chart of accounts
  # Sheets - TrackVia Table
    # GL Codes 2 - CCHP GL Codes
    # Type 3 - CCHP Type
    # Dept 4 - CCHP Departments
    # Funding Sources 6 - CCHP Funding Sources
    # Program 5 - CCHP Programs
    # Employees 7 - no table

# Save each sheet as a separate CSV file before importing into R
  # Unhide all columns in Excel file

# Install packages
library(dplyr)

# Set working directory where you save the CSVs

#### GL Codes 2 ####
  # Column Names, Excel
    # Account Code 
    # Account Title
    # Account Short Title
    # Status
    # Account Type
    # Description
    # Examples
    # Mapping to Statements
  
  # Column Names For Upload
    # Account Code 
    # Account Title
    # Account Short Title
    # Status
    # Description
    # Examples
    # Mapping to Statements

# Read in gl_code.csv
gl <- read.csv(file = "gl_code.csv", header = F)

# Remove the four rows before column headers row
gl <- gl[-c(1:4),]

# Remove empty columns, V9 and V10, and Account Type
gl$V5 <- NULL
gl$V9 <- NULL
gl$V10 <- NULL

# Turn column headers row into actual column names
gl_2 <- rename(gl, "Account Code" = V1,
                   "Account Title" = V2,
                   "Account Short Title" = V3,
                   "Status" = V4,
                   "Description" = V6,
                   "Examples" = V7,
                   "Mapping to Statements" = V8)

# Drop rows with current column names
gl_3 <- gl_2[-1,]

# Remove "FOR ACCOUNTING USE ONLY" GL codes
# Convert Account Code to number
gl_3$`Account Code` <- as.numeric(as.character(gl_3$`Account Code`))
  
# Keep rows where Account Code is >= 6010 and Description != accounting use only
gl_4 <- filter(gl_3, `Account Code` >= 6010, Description != 'accounting use only')

# Write csv called fa_gl_upload_[insert date]
write.csv(gl_4, file = paste("fa_gl_upload_", format(Sys.Date(), "%Y-%m-%d"), ".csv"), row.names = F)

#### Type 3 ####
# Most likely, no changes in this Chart of Accounts sheet.
# Manually look at Excel file and compare with TrackVia.

#### Dept 4 ####
# Most likely, no changes in this Chart of Accounts sheet.
# Manually look at Excel file and compare with TrackVia to see if there are updates
# Keep rows with Account Title values ending in "Program" or with no "- *" at the end of value

# Column Names, Excel
  # Account Code 
  # Account Title
  # Account Short Title

# Save Dept 4 as CSV and read in sheet
dept <- read.csv(file = "dept.csv", header = F)

# Remove the six rows before column headers row
dept <- dept[-c(1:6),]

# Remove empty columns, V4:V7
dept$V4 <- NULL
dept$V5 <- NULL
dept$V6 <- NULL
dept$V7 <- NULL

# Turn column headers row into actual column names
dept_2 <- rename(dept, 
                 "Account Code" = V1,
                 "Account Title" = V2,
                 "Account Short Title" = V3)

# Drop rows with current column names
dept_3 <- dept_2[-1,]

# Convert Account Code to number, Account Title and Account Short Title to characters
dept_3$`Account Code` <- as.numeric(as.character(dept_3$`Account Code`))
dept_3$`Account Title` <- as.character(dept_3$`Account Title`)
dept_3$`Account Short Title` <- as.character(dept_3$`Account Short Title`)

# Remove Account Title with "- Admin", "INACTIVE", or blank
dept_4 <- filter(dept_3, 
                 `Account Title` != "", 
                 !grepl("- Admin", `Account Title`), 
                 !grepl("INACTIVE", `Account Title`))

# Remove Account Codes 45, 49, 800, 830, 900
dept_5 <- filter(dept_4,
                 !(`Account Code` %in% c(45,49, 800, 830, 900)))

# Write Department CSV
write.csv(dept_5, file = paste("fa_dept_upload_", format(Sys.Date(), "%Y-%m-%d"), ".csv"), row.names = F)

#### Funding Sources 6 ####

# Add an Active column

# Column names, Excel
  # Funding Source #
  # Program #
  # Primary Approver *
  # Secondary Approver
  # Funding Source
  # start date
  # end date
  # Amount
  # Cooper grant #
  # EB rate
  # IC rate
  # CCHP
  # ACO
  # NC

# Column Names For Upload
  # CCHP_Funding_Source_Number
  # Funding_Source
  # Funding_Name
  # Program_Number
  # Primary_Approver
  # Secondary_Approver
  # Start_Date
  # End_Date
  # Amount
  # Cooper_Grant_Number
  # EB_rate
  # IC_rate
  # CCHP (Yes/No)
  # ACO (Yes/No)
  # NC (Yes/No)
  # Active

# Save Funding Sources 6 as CSV and read in sheet
fund <- read.csv(file = "fund.csv", header = F)

# Remove the nine rows before column headers row
fund <- fund[-c(1:9),]

# Split fund into Active and Inactive funding sources
end_active <- grep("Inactive Grants", fund$V1) - 1
fund_active <- fund[c(1:end_active),]
fund_inactive <- fund[c(end_active:nrow(fund)),]

# Remove columns V3, V4 (Funding Approvers), V15 fund_active
fund_active$V3 <- NULL
fund_active$V4 <- NULL
fund_active$V15 <- NULL

# Rename fund_active columns
fund_active_2 <- rename(fund_active, 
                        "CCHP_Funding_Source_Number" = V1,
                        "Program_Number" = V2,
                        "Funding_Name" = V5,
                        "Start_Date" = V6,
                        "End_Date" = V7,
                        "Amount" = V8,
                        "Cooper_Grant_Number" = V9,
                        "EB_rate" = V10,
                        "IC_rate" = V11,
                        "CCHP" = V12,
                        "ACO" = V13,
                        "NC" = V14)

# Replace X in CCHP, ACO, NC columns with "Yes"
fund_active_2$CCHP <- as.character(fund_active_2$CCHP) 
fund_active_2$ACO <- as.character(fund_active_2$ACO)
fund_active_2$NC <- as.character(fund_active_2$NC)

fund_active_2$CCHP[fund_active_2$CCHP == "X"] <- "Yes"
fund_active_2$ACO <- ifelse(is.na(fund_active_2$ACO), "Yes", "")
fund_active_2$NC[fund_active_2$NC == "X"] <- "Yes"

# Add Funding_Source column
fund_active_2$Funding_Source <- NA

# Remove column name row and blank tail rows
fund_active_3 <- filter(fund_active_2,
                        CCHP_Funding_Source_Number != "Funding Source #",
                        CCHP_Funding_Source_Number != "")

# Fill Funding_Source column
fund_active_3$CCHP_Funding_Source_Number <- as.numeric(as.character(fund_active_2$CCHP_Funding_Source_Number))

