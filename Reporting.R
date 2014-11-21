# Reads 4 separate files
setwd("Y:/")
TR<-read.csv("Triage Review Table.csv", na.strings = "NA")
MPT<-read.csv("Master Patient Table.csv", na.strings = "NA")
EVI<-read.csv("Enrollment Visit Intake.csv", na.strings = "NA")
RS<-read.csv("Risk Stratification.csv", na.strings = "NA")

# Renames fields in the EVI data set for calculation ease
library("reshape")
EVI<-rename(EVI, c(Primary.Insurance.Type="Primary"))
EVI<-rename(EVI, c(Primary.Insurance.ID..="PrimeID"))
EVI<-rename(EVI, c(Secondary.Insurance.Type="Secondary"))
EVI<-rename(EVI, c(Secondary.Insurance.ID..="SecondID"))

# Medicaid ID of patients may exist in one of two fields. The following lines collect the correct ID into one field in the EVI file
EVI$PrimeID<-as.character(EVI$PrimeID)
EVI$SecondID<-as.character(EVI$SecondID)
EVI$medicaid<-grepl("medicaid", EVI$Primary, ignore.case=TRUE)
EVI$MedicaidNumber<-ifelse(EVI$medicaid==TRUE, EVI$PrimeID, EVI$SecondID)
EVI$medicaid<-NULL

# Medicare ID of patients may exist in one of two fields. The following lines collect the correct ID into one field in the EVI file
EVI$medicare<-grepl("medicare", EVI$Primary, ignore.case=TRUE)
EVI$MedicareNumber<-ifelse(EVI$medicare==TRUE, EVI$PrimeID, EVI$SecondID)
EVI$medicare<-NULL

# Adds prefix in front of column names to identify the original file of the column. This is in case we need to go back to our database to fix #errors
colnames(TR) <- paste("TR", colnames(TR), sep = "_")
colnames(MPT) <- paste("MPT", colnames(MPT), sep = "_")
colnames(EVI) <- paste("EVI", colnames(EVI), sep = "_")
colnames(RS) <- paste("RS", colnames(RS), sep = "_")

# Unifies the name of the columns we use to merge the four files
library("reshape")
TR<-rename(TR, c(TR_RCT_ID="rctid"))
MPT<-rename(MPT, c(MPT_RCT_ID="rctid"))
EVI<-rename(EVI, c(EVI_RCT_ID="rctid"))
RS<-rename(RS, c(RS_RCT_ID="rctid"))

# Merges the files by keeping TR data set as master
MPTmerge<-merge(TR, MPT, by="rctid", all.x=TRUaE)
RSmerge<-merge(MPTmerge, RS, by="rctid", all.x=TRUE)
reporting<-merge(RSmerge, EVI, by="rctid", all.x=TRUE)

# Patient SSNs live in both the TR and the RS data sets. The following lines collect the SSNs into one field.
reporting[reporting==""]<-NA
reporting$TR_SSN<-as.character(reporting$TR_SSN)
reporting$RS_SSN<-as.character(reporting$RS_SSN)
reporting$Master_SSN<-ifelse(is.na(reporting$TR_SSN)==TRUE, reporting$RS_SSN, reporting$TR_SSN)

# Medicaid IDs live in the TR, RS and EVI data sets. The following lines collect the IDs into one field.
reporting$RS_Medicaid.Number<-as.character(reporting$RS_Medicaid.Number)
reporting$TR_Medicaid.ID<-as.character(reporting$TR_Medicaid.ID)
reporting$MasterMedicaidID<-ifelse(is.na(reporting$RS_Medicaid.Number)==TRUE, reporting$EVI_MedicaidNumber, reporting$RS_Medicaid.Number)
reporting$MasterMedicaidID<-ifelse(is.na(reporting$MasterMedicaidID)==TRUE, reporting$TR_Medicaid.ID, reporting$MasterMedicaidID)

# Medicare IDs live in the TR, RS and EVI data sets. The following lines collect the IDs into one field.
reporting$RS_Medicare.Number<-as.character(reporting$RS_Medicare.Number)
reporting$TR_Medicare.ID<-as.character(reporting$TR_Medicare.ID)
reporting$MasterMedicareID<-ifelse(is.na(reporting$RS_Medicare.Number)==TRUE, reporting$EVI_MedicareNumber, reporting$RS_Medicare.Number)
reporting$MasterMedicareID<-ifelse(is.na(reporting$MasterMedicareID)==TRUE, reporting$TR_Medicare.ID, reporting$MasterMedicareID)

# Patient gender lives in the MPT, TR and RS data sets. The following lines collect gender into one field.
reporting$MPT_Gender<-as.character(reporting$MPT_Gender)
reporting$TR_X.PatientID..Gender<-as.character(reporting$TR_X.PatientID..Gender)
reporting$RS_Gender<-as.character(reporting$RS_Gender)
reporting$IntGender<-ifelse(is.na(reporting$TR_X.PatientID..Gender)==TRUE, reporting$MPT_Gender, reporting$TR_X.PatientID..Gender)
reporting$MasterGender<-ifelse(is.na(reporting$IntGender)==TRUE, reporting$RS_Gender, reporting$IntGender)

# Changes values of the Facility field to their numerical equivalent
reporting$TR_Facility<-as.character(reporting$TR_Facility)
reporting$TR_Facility[reporting$TR_Facility=="Hospital 1, Hospital 2"]<-"310014,310029"
reporting$TR_Facility[reporting$TR_Facility=="Hospital 1"]<-"310014"
reporting$TR_Facility[reporting$TR_Facility=="Hospital 2"]<-"310029"

# Adds a new field to designate if patient was Pre-enrolled and populates it with Yes if Pre-Enroll Interview Date is not blank
reporting$PreEnrolled<-ifelse(is.na(reporting$MPT_Pre.Enrollment.Interview.Date)==TRUE, "No", "Yes")

# Renames fields to prepare file for export
reporting<-rename(reporting, c(TR_MRN..="MRN"))
reporting<-rename(reporting, c(rctid="RCT_ID"))
reporting<-rename(reporting, c(Master_SSN="SSN"))
reporting<-rename(reporting, c(TR_PatientID="UniqueID"))
reporting<-rename(reporting, c(MasterGender="Gender"))
reporting<-rename(reporting, c(TR_Enrolled="Enrolled"))
reporting<-rename(reporting, c(TR_Last.Name="Last Name"))
reporting<-rename(reporting, c(MPT_CMIGroup="CMI Group"))
reporting<-rename(reporting, c(TR_First.Name="First Name"))
reporting<-rename(reporting, c(TR_Admit.Date="Admit Date"))
reporting<-rename(reporting, c(TR_RCTStudyGroup="RCT Group"))
reporting<-rename(reporting, c(MasterMedicareID="MedicareID"))
reporting<-rename(reporting, c(MasterMedicaidID="MedicaidID"))
reporting<-rename(reporting, c(TR_Triage.Outcome="TriageOutcome"))
reporting<-rename(reporting, c(TR_X.PatientID..Date.of.Birth="DOB"))
reporting<-rename(reporting, c(MPT_Enrollment.Date="EnrollmentDate"))
reporting<-rename(reporting, c(MPT_DischargeStatus="Discharge Status"))
reporting<-rename(reporting, c(TR_NotAssignedReason="Not Assigned Reason"))
reporting<-rename(reporting, c(TR_Reason.Not.Enrolled="Reason Not Enrolled"))
reporting<-rename(reporting, c(TR_OtherNotAssignedReason="Other Not Assigned Reason"))
reporting<-rename(reporting, c(TR_Facility="Admit Hospital National Provider Number"))
reporting<-rename(reporting, c(MPT_Care.Team.Assignment.Date="Care Team Assignment Date"))
reporting<-rename(reporting, c(MPT_Pre.Enrollment.Interview.Date="Pre Enrollment Interview Date"))
reporting<-rename(reporting, c(TR_HistoricalHCIAEligibleNotPreEnrolled="Historical_HCIA_Eligible"))

# Subsets the data into three different files depending on criteria
eligible<-subset(reporting, TriageOutcome=="HCIA Eligible" & is.na(Historical_HCIA_Eligible)==TRUE)
z<-subset(reporting, RCT_ID=="5014426762" | RCT_ID=="5014426916")
x<-subset(reporting, TriageOutcome=="HCIA Eligible-Not Assigned")
y<-subset(reporting, Historical_HCIA_Eligible=="Yes")
require("gtools")
not_assigned<- smartbind(x, y, fill=NA)
eligible<-smartbind(eligible, z, fill=NA)

# Only keeps records in the eligible file that have an existing record in the MPT
require("stringr")
eligible2<-eligible[eligible$RCT_ID %in% MPT$rctid,]

# Adds the records removed from the previous step to the not_assigned data frame
eligible_removed<-eligible[!eligible$RCT_ID %in% MPT$rctid,]
not_assigned<-smartbind(not_assigned, eligible_removed, fill=NA)

# Identifies columns to be exported for each file
eligible2 <- eligible2[,c("UniqueID", 
                          "RCT_ID", 
                          "MRN", 
                          "First Name", 
                          "Last Name", 
                          "DOB", 
                          "Gender", 
                          "SSN", 
                          "MedicareID", 
                          "MedicaidID", 
                          "Admit Date",
                          "TriageOutcome",
                          "Not Assigned Reason", 
                          "Other Not Assigned Reason", 
                          "Admit Hospital National Provider Number",
                          "Pre Enrollment Interview Date", 
                          "PreEnrolled", 
                          "Enrolled", 
                          "EnrollmentDate",
                          "Care Team Assignment Date",
                          "Reason Not Enrolled", 
                          "Discharge Status", 
                          "RCT Group", 
                          "CMI Group")]


not_assigned <- not_assigned [,c("UniqueID",
                                 "RCT_ID", 
                                 "MRN", 
                                 "First Name", 
                                 "Last Name", 
                                 "DOB", 
                                 "Gender", 
                                 "SSN", 
                                 "MedicareID", 
                                 "MedicaidID", 
                                 "TriageOutcome", 
                                 "Not Assigned Reason", 
                                 "Other Not Assigned Reason",
                                 "Admit Hospital National Provider Number", 
                                 "Historical_HCIA_Eligible")]


# Replaces NA values with blanks
eligible2[is.na(eligible2)]   <- "" 
not_assigned[is.na(not_assigned)]   <- "" 

# Exports files
write.csv(eligible2, file="Reporting_HCIA_Eligible.csv")
write.csv(not_assigned, file="Reporting_HCIA_Eligible_NotAssigned.csv")
