# Sets working directory, reads file and creates a Nkname
setwd("Y:/Monthly/Insurance")
Insurance<-read.csv("Y:/Monthly/Insurance/OctoberInsuranceCapList.csv")

# Attaches packages the code needs to run
require(reshape)
require(zipcode)

# Concatenates provider information
Insurance$CURR_PCP_FULL_NAME<-paste(Insurance$PROV_LNAME, Insurance$PROV_FNAME, sep=", ")

# Removes unused fields and renames others to match field names in our database
Insurance$PLAN_DESC<-NULL
Insurance$AGE<-NULL
Insurance$EFFECTIVE_DATE<-NULL
Insurance$TERM_DATE<-NULL
Insurance$PLAN_CODE<-NULL
Insurance$PROV_EFF_DATE<-NULL
Insurance$PROV_TERM_DATE<-NULL
Insurance$LINE_OF_BUSINESS<-NULL
Insurance$PANEL_ID<-NULL
Insurance$COSMOS_CUST_SEG<-NULL
Insurance$COSMOS_CUST_SEG_DESC<-NULL
Insurance$PROV_LANG_1<-NULL
Insurance$PROV_LANG_2<-NULL
Insurance$PROV_LANG_3<-NULL
Insurance$PROV_FNAME<-NULL
Insurance$PROV_LNAME<-NULL
Insurance<-reshape::rename(Insurance, c(DATE_OF_BIRTH="DOB"))
Insurance<-reshape::rename(Insurance, c(MEMB_GENDER="GENDER"))
Insurance<-reshape::rename(Insurance, c(PROVIDER_ID="CURR_PCP_ID"))
Insurance<-reshape::rename(Insurance, c(PROV_PHONE="PHONE_NUMBER"))
Insurance<-reshape::rename(Insurance, c(PROV_ADDRESS_LINE_1="CURR_PCP_ADDRESS_LINE_1"))
Insurance<-reshape::rename(Insurance, c(PROV_ADDRESS_LINE_2="CURR_PCP_ADDRESS_LINE_2"))
Insurance<-reshape::rename(Insurance, c(PROV_CITY="CURR_PCP_CITY"))
Insurance<-reshape::rename(Insurance, c(PROV_STATE="CURR_PCP_STATE"))
Insurance<-reshape::rename(Insurance, c(PROV_ZIP="CURR_PCP_ZIP"))
Insurance<-reshape::rename(Insurance, c(PAYEE_NAME="VEND_FULL_NAME"))

# Maps language abbreviations to full language
Insurance$MEMB_LANGUAGE<-as.character(Insurance$MEMB_LANGUAGE)
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="ARA"]<-"Arabic"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="CHI"]<-"Chinese"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="ENG"]<-"English"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="FRE"]<-"French"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="HEB"]<-"Hebrew"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="ITA"]<-"Italian"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="KOR"]<-"Korean"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="N/A"]<-""
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="PER"]<-"Persian"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="POR"]<-"Portuegese"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="SPA"]<-"Spanish"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="TUR"]<-"Turkish"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="UNK"]<-"Unknown"
Insurance$MEMB_LANGUAGE[Insurance$MEMB_LANGUAGE=="VIE"]<-"Vietnamese"

# Sets the MEDICAID_NO field to numeric to get rid of scientific notation
options(scipen=500)
Insurance$MEDICAID_NUMBER<-as.numeric(as.character(Insurance$MEDICAID_NO))
Insurance$MEDICAID_NO <-NULL
Insurance<-reshape::rename(Insurance, c(MEDICAID_NUMBER="MEDICAID_NO"))

# Cleans the home phone number field of non numeric characters
Insurance$HOME_PHONE_NUMBER<-as.character(Insurance$HOME_PHONE_NUMBER)
Insurance$HOME_PHONE_NUMBER<-gsub("\\(|\\)|\\-|\\ ", "", Insurance$HOME_PHONE_NUMBER)

# Formats ZIP codes to actual ZIP format
Insurance$MEMB_ZIP<-clean.zipcodes(MEMB_ZIP)

# Formats birth dates and renames the field
Insurance<-reshape::rename(Insurance, c(DOB="ARCHIVE_DOB"))
Insurance$DOB<-as.Date(Insurance$ARCHIVE_DOB, "%m/%d/%Y")

# Deletes entries with the wrong vendor names
Insurance2 <- subset(Insurance, !(VEND_FULL_NAME=="Wrong vendor name"))

# Filters the file by keeping only the entries where the PCP City is Camden or Pennsauken, or the vendor name is PCPY
InsuranceCP<-subset(Insurance2, CURR_PCP_CITY=="CAMDEN" | CURR_PCP_CITY=="PENNSAUKEN" | VEND_FULL_NAME=="PCPY")

# If the code to rename vendors gives you trouble, use the below line
# InsuranceCP <- data.frame(lapply(InsuranceCP, as.character), stringsAsFactors=FALSE)

# Renames vendors to match Current PCP City
InsuranceCP$VEND_FULL_NAME[InsuranceCP$VEND_FULL_NAME=="PCPA" & InsuranceCP$CURR_PCP_CITY=="CAMDEN"] <- "PCPA_CAMDEN"          
InsuranceCP$VEND_FULL_NAME[InsuranceCP$VEND_FULL_NAME=="PCPA" & InsuranceCP$CURR_PCP_CITY=="PENNSAUKEN"] <- "PCPA_PENNSAUKEN"  
InsuranceCP$VEND_FULL_NAME[InsuranceCP$VEND_FULL_NAME=="PCPB" & InsuranceCP$CURR_PCP_CITY=="CAMDEN"] <- "PCPB_CAMDEN"
InsuranceCP$VEND_FULL_NAME[InsuranceCP$VEND_FULL_NAME=="PCPC" & InsuranceCP$CURR_PCP_CITY=="CAMDEN"] <- "PCPC_CAMDEN"   
InsuranceCP$VEND_FULL_NAME[InsuranceCP$VEND_FULL_NAME=="PCPC" & InsuranceCP$CURR_PCP_CITY=="PENNSAUKEN"] <- "RPCPC_PENNSAUKEN"  

#Excludes entries that are from PCPA with CURR_PCP_CITY Pennsauken, but don't have the address John Street#
#InsuranceCP <- subset(InsuranceCP, !(VEND_FULL_NAME=="PCPA_PENNSAUKEN" & Insurance$CURR_PCP_ADDRESS_LINE_1!="John Street"))

# Maps to practices to their IDs in our database
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPA_CAMDEN"]<-"1"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPA_PENNSAUKEN"]<-"2"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPB_CAMDEN"]<-"3"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPC_CAMDEN"]<-"4"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPC_PENNSAUKEN"]<-"5"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPD"]<-"6"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPE"]<-"7"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPF"]<-"8"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPG"]<-"9"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPH"]<-"10"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPI"]<-"11"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPL"]<-"12"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPM"]<-"13"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPN"]<-"14"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPO"]<-"15"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPP"]<-"16"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPQ"]<-"16"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPR"]<-"16"
InsuranceCP$PRACTICE[InsuranceCP$VEND_FULL_NAME =="PCPS"]<-"16"


# Adds insurance field to identify the source of the data
InsuranceCP$PAYER<-"INSURANCE"

# Replaces NA values with blanks
InsuranceCP[is.na(InsuranceCP)]   <- "" 

# Formats the data as a data frame
InsuranceCP<-as.data.frame(InsuranceCP)

# Removes unused fields
InsuranceCP2<-InsuranceCP
InsuranceCP2$MEMB_LANGUAGE<-NULL
InsuranceCP2$MEMB_ETHNITY<-NULL

# Exports file with the date in the file name for our archives
write.csv(InsuranceCP2, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"AllPayers",  ".csv", sep="")), row.names=FALSE)

# Pulls in PCPX dataset to cross check for duplicate records
pcp<-read.csv("Y:/Monthly/Insurance/2014-10-06-PCPX.csv")

# Formats ZIP codes
pcp$MEMB_ZIP<-clean.zipcodes(MEMB_ZIP)

# Removes non-numeric characters from the home phone number field
pcp<-reshape::rename(pcp, c(HOME_PHONE_NUM="HOME_PHONE_NUMBER"))
pcp$HOME_PHONE_NUMBER<-gsub("\\(|\\)|\\-|\\ ", "", pcp$HOME_PHONE_NUMBER)

# Adds  text identifiers to Subscriber_ID (N and U)
pcp$N<-"N"
pcp$SUBSCRIBER_ID2<-paste(pcp$N, pcp$SUBSCRIBER_ID, sep="")
pcp$N<-NULL

InsuranceCP$U<-"U"
InsuranceCP$U<-as.character(InsuranceCP$U)
InsuranceCP$SUBSCRIBER_ID2<-paste(InsuranceCP$U, InsuranceCP$SUBSCRIBER_ID, sep="")
InsuranceCP$U<-NULL

# Adds a field and populates with insurance name and pcp name
pcp$SOURCE<- "PCP_N"
InsuranceCP$SOURCE <- "Insurance"

# Removes non-numeric characters from the Social Security Number field and renames the field to match our database
pcp$SSN<-gsub("[^0-9]","",pcp$SOCIAL_SEC_NO)
pcp$SOCIAL_SEC_NO<-NULL
pcp<-reshape::rename(pcp, c(SSN="SOCIAL_SEC_NO"))

# Renames MEM_INSURANCE TO PAYER to match Insurance code
pcp<-reshape::rename(pcp, c(MEMB_INSURANCE="PAYER"))

# Populates the field VEND_FULL_NAME field for the pcp dataframe with PCPN
pcp$VEND_FULL_NAME<- "PCPN"

# Removes records that came in with a specific insurance to avoid duplicates with the previous file
#PCP <- subset(pcp, !(MEMB_INSURANCE=="Insurance X" | MEMB_INSURANCE=="Insurance Y" | MEMB_INSURANCE=="Insurance Z" | MEMB_INSURANCE=="Insurance Q" | MEMB_INSURANCE=="Insurance XA")

# Removes unused columns
pcp$Reg.Patient.MRN<-NULL
pcp$MEMB_NAME<-NULL

# Removes records with unrelated vendor name
Insurance3 <- subset(InsuranceCP, !(VEND_FULL_NAME=="Unrelated Vendor"))

# Appends the last Insurance file with the last clean PCP file
require(gtools)
Insurance3 <- data.frame(Insurance3)
pcp <- data.frame(pcp)
data<-smartbind(Insurance3,pcp)

# Removes the field Practice
data$PRACTICE<- NULL

# Adds a column "LastCapitationDate" & fills the column with the 1st of the current month
data$LastCapitationDate<- format(Sys.time(), "%m/01/%Y") 

# Removes "U" from the string to match the database subscriber id
data$SUBSCRIBER_ID2<-gsub("U", "", data$SUBSCRIBER_ID2)

# Renames old Subscriber id field to archive, and subscriber_ID2 to Subscriber_ID
data<-reshape::rename(data, c(SUBSCRIBER_ID="ARCHIVE_SUBSCRIBER_ID"))
data<-reshape::rename(data, c(SUBSCRIBER_ID2="SUBSCRIBER_ID"))

# Clears out all NA values
data[is.na(data)]   <- "" 

# Exports file to send to HIE vendor
write.csv(data, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"All Payers - TrackVia",  ".csv", sep="")), row.names=FALSE)
