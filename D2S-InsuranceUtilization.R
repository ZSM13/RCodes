# Reads all separate files from PCPs
setwd("Y:/API/")
pcp1<-read.csv(paste("Y:/API/", "pcp1-", Sys.Date(), ".csv", sep=""))
pcp2<-read.csv(paste("Y:/API/", "pcp2-", Sys.Date(), ".csv", sep=""))
pcp3<-read.csv(paste("Y:/API/", "pcp3-", Sys.Date(), ".csv", sep=""))
pcp4<-read.csv(paste("Y:/API/", "pcp4-", Sys.Date(), ".csv", sep=""))
pcp5<-read.csv(paste("Y:/API/", "pcp5-", Sys.Date(), ".csv", sep=""))
pcp6<-read.csv(paste("Y:/API/", "pcp6-", Sys.Date(), ".csv", sep=""))
pcp7<-read.csv(paste("Y:/API/", "pcp7-", Sys.Date(), ".csv", sep=""))
pcp8<-read.csv(paste("Y:/API/", "pcp8-", Sys.Date(), ".csv", sep=""))
pcp9<-read.csv(paste("Y:/API/", "pcp9-", Sys.Date(), ".csv", sep=""))
pcp10<-read.csv(paste("Y:/API/", "pcp10-", Sys.Date(), ".csv", sep=""))

# Binds data from the first 9 practices into one file 
PATIENTS<- rbind(pcp1,pcp2,pcp3,pcp4,pcp5,pcp6,pcp7,pcp8,pcp9)

# Deletes unused fields from files
PATIENTS$Practice<-NULL
PATIENTS$PCP.Name<-NULL
pcp10$Patient.ID<-NULL

# Adds  text identifiers to Subscriber_ID (A and B)
pcp10$A<-"A"
pcp10$Subscriber.ID<-paste(pcp10$A, pcp10$Subscriber.ID, sep="")
pcp10$A<-NULL
PATIENTS$B<-"B"
PATIENTS$Subscriber.ID<-paste(PATIENTS$B, PATIENTS$Subscriber.ID, sep="")
PATIENTS$B<-NULL

# Renames fields to bind
require(reshape)
pcp10<-reshape::rename(pcp10, c(Last.Provider="Provider"))

# Binds pcp10 and PATIENTS data into one file
PATIENTS<-rbind(PATIENTS,pcp10)

# Removes "B" from string to match database subscriber id's
PATIENTS$Subscriber.ID<-gsub("B", "", PATIENTS$Subscriber.ID)

# Creates variables to calculate difference in days between today and admit date
PATIENTS$TodaysDate <- Sys.Date()
PATIENTS$date_diff <- as.Date(PATIENTS$TodaysDate, format="%Y/%m/%d")- as.Date(PATIENTS$Admit.Date) 

# Creates subset of patients who have had an admit date that fall within the last 21 days
PATIENTS2<-subset(PATIENTS, date_diff<21)

# Creates CurrentlyAdmitted Field with text from AdmitDate Field
PATIENTS2$CurrentlyAdmitted <- gsub("\\(()\\)","\\1",  PATIENTS2$DischargeDate)

# Removes parenthetical values from DateAdmited fields
PATIENTS2$DischargeDate <- gsub("\\(.*\\)","\\1", PATIENTS2$DischargeDate)

# Removes dates from CurrentlyAdmitted field
PATIENTS2$CurrentlyAdmitted <- ifelse(PATIENTS2$CurrentlyAdmitted == PATIENTS2$DischargeDate, "", PATIENTS2$CurrentlyAdmitted)

# Renames Patient.ID to HIE_ID
require(reshape)
PATIENTS2<-reshape::rename(PATIENTS2, c(Admit.Date="AdmitDate"))
PATIENTS2<-reshape::rename(PATIENTS2, c(Adm.Diagnoses="HistoricalDiagnosis"))
PATIENTS2<-reshape::rename(PATIENTS2, c(Inp..6mo.="Inp6mo"))
PATIENTS2<-reshape::rename(PATIENTS2, c(ED..6mo.="ED6mo"))
PATIENTS2<-reshape::rename(PATIENTS2, c(Patient.Class="PatientClass"))
PATIENTS2<-reshape::rename(PATIENTS2, c(Subscriber.ID="SUBSCRIBER_ID_LINK"))

# Identifies the columns for the file to be exported
PATIENTS2<-PATIENTS2[,c("SUBSCRIBER_ID_LINK","AdmitDate","DischargeDate", "Facility", "PatientClass", "HistoricalDiagnosis", "Inp6mo", "ED6mo", "CurrentlyAdmitted")]

#Export csv file#
write.csv(PATIENTS2, (file=paste("EDUtilization-Insurance-", format(Sys.Date(), "-%Y-%m-%d"), ".csv", sep="")), row.names=FALSE)
