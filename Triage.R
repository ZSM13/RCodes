# Sets working directory, reads file and creates a nickname
setwd("Z:/Triage")
Triage <- read.csv("Z:/Triage/report.csv")

# Exports the file with a date in file name for our archives
write.csv(Triage, (file=paste ("CMI Admitted Past Month", Sys.Date(), ".csv", sep="")), row.names=FALSE)

# Splits the "Name" column into First and Last Name
Triage$LastName = as.character(lapply(strsplit(as.character(Triage$Name), split=", "), "[", 1))
Triage$FirstName = as.character(lapply(strsplit(as.character(Triage$Name), split=", "), "[", 2))

# Removes the full Name field
Triage$Name <- NULL

# Creates the name fields that will comprise the first part of the PatientID
Triage$FN<-substr(Triage$FirstName, 1, 2)
Triage$LN<-substr(Triage$LastName, 1, 3)

# Prepares the DOB field to be concatenated for the PatientID
Triage$DOB2 <- as.POSIXct(Triage$DOB, format="%m/%d/%Y")
as.numeric(Triage$DOB2)
Triage$DOB3<-format(Triage$DOB2, "%m%d%Y")

# Concatenates the 3 fields that form the PatientID
Triage$PatientID2 <- do.call(paste, c(Triage[c("FN", "LN", "DOB3")], sep = ""))

# Deletes the unused fields
Triage$FN<- NULL
Triage$LN<- NULL
Triage$DOB2 <-NULL
Triage$DOB3 <-NULL

# Only keeps records that come from Hospital A and B
Triage2<-subset(Triage, Facility %in% c("A", "B") )

# Converts the values in the "Age" column to characters, removes "yo" from the end of the value and converts the values back to number
Triage2$Age <-as.character(Triage2$Age)
Triage2$Age2 <- substr(Triage2$Age, 1, nchar(Triage2$Age)-2)
Triage2$Age2<-as.numeric(Triage2$Age2)

# Only keeps records of those who are 18 and over
Triage3<-subset(Triage2, Age2 >=18)

# Creates a new column "CurrentlyAdmitted" with text from "AdmitDate" Field#
Triage3$CurrentlyAdmitted <- gsub("\\(()\\)","\\1",  Triage3$Discharge.Date..Day.)

# Removes parenthetical values from DischargeDate field
Triage3$Discharge.Date..Day. <- gsub("\\(.*\\)","\\1", Triage3$Discharge.Date..Day.)

# Removes dates from CurrentlyAdmitted field
Triage3$CurrentlyAdmitted <- ifelse(Triage3$CurrentlyAdmitted == Triage3$Discharge.Date..Day., "", Triage3$CurrentlyAdmitted)

# Maps A and B to the full name of hospitals, Gender M to Male, F to Female#
Triage3$nFacility[Triage3 $Facility=="A"] <- "Hospital A"
Triage3$nFacility[Triage3 $Facility=="B"] <- "Hospital B"
Triage3$nGender[Triage3 $Gender=="M"] <- "Male"
Triage3$nGender[Triage3 $Gender=="F"] <- "Female"

# Removes unused columns
Triage3$Practice <- NULL
Triage3$Adm.Diagnoses <- NULL
Triage3$Age<-NULL
Triage3$Facility<-NULL
Triage3$Gender<-NULL

# Renames columns to prepare for export
require(reshape)
Triage3<-reshape::rename(Triage3, c(Patient.ID="HIEID"))
Triage3<-reshape::rename(Triage3, c(DOB="Date of Birth"))
Triage3<-reshape::rename(Triage3, c(Admit.Date="Admit Date"))
Triage3<-reshape::rename(Triage3, c(Discharge.Date..Day.="Discharge Date"))
Triage3<-reshape::rename(Triage3, c(Total.Days..6mo.="TotalDays6months"))
Triage3<-reshape::rename(Triage3, c(Inp..6mo.="Inp6mo"))
Triage3<-reshape::rename(Triage3, c(ED..6mo.="ED6mo"))
Triage3<-reshape::rename(Triage3, c(Provider="HIEProvider"))
Triage3<-reshape::rename(Triage3, c(Insurance="HIEInsurance"))
Triage3<-reshape::rename(Triage3, c(Age2="AgeTriage"))
Triage3<-reshape::rename(Triage3, c(nFacility="Facility"))
Triage3<-reshape::rename(Triage3, c(nGender="Gender"))
Triage3<-reshape::rename(Triage3, c(LastName="Last Name"))
Triage3<-reshape::rename(Triage3, c(FirstName="First Name"))


# Identifies the columns to be exported for the two files
TriageOutcome<-Triage3[,c("Last Name", 
                          "First Name", 
                          "Date of Birth", 
                          "PatientID2", 
                          "Gender", 
                          "HIEID")]

TriageReview<-Triage3[,c("PatientID2", 
                         "AgeTriage",
                         "Admit Date", 
                         "Discharge Date", 
                         "Facility", 
                         "CurrentlyAdmitted", 
                         "TotalDays6months", 
                         "Inp6mo", 
                         "ED6mo", 
                         "HIEProvider", 
                         "HIEInsurance")]

# Renames PatientID2 to PatientID for the Triage Review File#
TriageReview<-reshape::rename(TriageReview, c(PatientID2="PatientID"))

# Exports files
write.csv(TriageOutcome, file="TriageOutcome.csv", row.names=FALSE)
write.csv(TriageReview, file="TriageReview.csv", row.names=FALSE)
