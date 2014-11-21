# Sets working directory
setwd("Y:/Readmissions/")

# Reads the daily report from the Camden HIE
hie-report<-read.csv(paste("Y:/Readmissions/", "report-", Sys.Date(), ".csv", sep=""))

# Reads the master patient database file
mpt<-read.csv(paste("Y:/Readmissions/", "mpt-", Sys.Date(), ".csv", sep=""))

# Builds the UniqueID in the hie-report to be able to compare to mpt file
# Changes capitalized Name fields to title case
hie-report$Name<-tolower(hie-report$Name)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

hie-report$Name<-sapply(hie-report$Name, simpleCap)

# Splits the "Names" column into First and Last Name
hie-report$LastName = as.character(lapply(strsplit(as.character(hie-report$Name), split=", "), "[", 1))
hie-report$FirstName = as.character(lapply(strsplit(as.character(hie-report$Name), split=", "), "[", 2))

# Creates the shortened name fields that will comprise the first part of the Unique ID
hie-report$FN<-substr(hie-report$FirstName, 1, 2)
hie-report$LN<-substr(hie-report$LastName, 1, 3)

# Prepares the Date.of.Birth Field to be concatenated for the Unique ID field
hie-report$DOB1 <- as.POSIXct(hie-report$DOB, format="%m/%d/%Y")
as.numeric(hie-report$DOB1)
hie-report$DOB2<-format(hie-report$DOB1, "%m%d%Y")

# Concatenates the 3 fields that form the Unique ID2 field
hie-report$UniqueID <- do.call(paste, c(hie-report[c("FN", "LN", "DOB2")], sep = ""))

# Subsets patients who are enrolled or are a control in the RCT study from the mpt file
mpt2<-subset(mpt, Enrolled.=="Yes" | RCTStudyGroup=="Control")

#Keeps the records in hie-report that exist in the mpt file
readmit<-hie-report[hie-report$UniqueID %in% mpt2$UniqueID,]

# If the individual exists in the MPT, then it adds their RCTSTudyGroup
require("data.table")
readmit<-data.table(readmit, key="UniqueID")
mpt2<-data.table(mpt2, key="UniqueID")
readmit2<-mpt2[readmit]

# Creates a field we can use to later identify how the record originated
readmit2$BulkImport<-"Import"

# Removes parenthetical values from DateAdmited fields
readmit2$Discharge.Date..Day. <- gsub("\\(.*\\)","\\1", readmit2$Discharge.Date..Day.)

# Selects the fields to be exported
readmit3<-data.frame(readmit2$UniqueID, readmit2$Patient.ID, readmit2$Admit.Date, readmit2$Discharge.Date..Day., readmit2$Visit.Type, readmit2$Facility, readmit2$BulkImport, readmit2$Enrolled., readmit2$RCTStudyGroup, readmit2$RCTGroup)

# Renames fields to match database tables
readmit3<-reshape::rename(readmit3, c(readmit2.Admit.Date="AdmitDate"))
readmit3<-reshape::rename(readmit3, c(readmit2.Facility="Facility"))
readmit3<-reshape::rename(readmit3, c(readmit2.RCTGroup="RCTGroup"))
readmit3<-reshape::rename(readmit3, c(readmit2.RCTStudyGroup="RCTStudyGroup"))
readmit3<-reshape::rename(readmit3, c(readmit2.UniqueID="UniqueID"))
readmit3<-reshape::rename(readmit3, c(readmit2.Visit.Type="VisitType"))
readmit3<-reshape::rename(readmit3, c(readmit2.Enrolled.="Enrolled"))
readmit3<-reshape::rename(readmit3, c(readmit2.BulkImport="BulkImport"))
readmit3<-reshape::rename(readmit3, c(readmit2.Discharge.Date..Day.="DischargeDate"))
readmit3<-reshape::rename(readmit3, c(readmit2.Patient.ID="Patient ID"))

# If there are any records here that are TRUE, review them before importing into the database. They should all be FALSE
readmit3$FoundError<-ifelse(try(readmit3$Enrolled=="No" & readmit3$RCTStudyGroup!="Control")==TRUE, "Review Record", "")

# Exports file
write.csv(readmit3, (file=paste ("Readmissions", format(Sys.Date(), "-%Y-%m-%d"), ".csv", sep="")), row.names=FALSE)
