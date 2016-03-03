# Clean Chart of Accounts for TrackVia Upload

# Read in each sheet in chart of accounts
  # Sheets - TrackVia Table
    # GL Codes 2 - CCHP GL Codes
    # Type 3 - CCHP Type
    # Dept 4 - CCHP Departments
    # Funding Sources 6 - CCHP Funding Sources
    # Program 5 - CCHP Programs
    # Employees 7 - no table

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

# Outside of R, in Excel:
  # Go to GL Codes 2 sheet
  # Unhide all columns in GL Codes 2 sheet
  # Save GL Codes 2 sheet as CSV, gl_code

# Install packages
library(dplyr)

# Set working directory where chart of accounts is

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
