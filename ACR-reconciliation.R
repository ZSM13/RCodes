# Attaches packages the code needs to run#
library("reshape")

# Sets working directory, reads file and creates a nickname#
path<-setwd("Y:/Data Share Daily/ACR/R Project")
date<-as.Date("2015-07-21")

# reads all csv files with the word document in the filename from the Y:/ACR directory
broadwayfam<-read.csv(paste(path,"/",date,"-broadway family practice", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#coopamb<-read.csv(paste(path,"/",date,"-cooper ambulatory pediatrics", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#coopfam<-read.csv(paste(path,"/",date,"-cooper family practice", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
coopphys<-read.csv(paste(path,"/",date,"-cooper physicians", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#jeffklee<-read.csv(paste(path,"/",date,"-jeffrey kleeman", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
lourdes<-read.csv(paste(path,"/",date,"-lourdes medical associates", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
phope<-read.csv(paste(path,"/",date,"-project hope", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
racosta<-read.csv(paste(path,"/",date,"-ramon acosta", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
riverprimary<-read.csv(paste(path,"/",date,"-river primary care", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
stlukes<-read.csv(paste(path,"/",date,"-st lukes", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#virtmedical<-read.csv(paste(path,"/",date,"-virtua medical group", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
#virtphys<-read.csv(paste(path,"/",date,"-virtua physicians associated", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)

#creates a source field to identify the origin of data
broadwayfam$Source<-"broadwayfam"
coopphys$Source<-"coopphys"
lourdes$Source<-"lourdes"
phope$Source<-"phope"
racosta$Source<-"racosta"
riverprimary$Source<-"riverprimary"
stlukes$Source<-"stlukes"

# reads csv files from TrackVia
cap<-read.csv(paste(path,"/","united_cap_list_for_acr_", date, "_1", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
ut<-read.csv(paste(path,"/","united_utilizations_for_acr_", date, "_1", ".csv", sep=""), header=TRUE, stringsAsFactors = FALSE)

# merges all acr files into one
acr<-rbind(broadwayfam,coopphys,lourdes,phope,racosta,riverprimary,stlukes)

# removes leading spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
acr$Patient.First.Name<-trim(acr$Patient.First.Name)
acr$Practice.Name<-trim(acr$Practice.Name) 

# subsets records with PCP city Camden and Pennsauken
acr<-subset(acr, acr$Assigned.PCP.City=="CAMDEN" | acr$Assigned.PCP.City=="PENNSAUKEN")

# creates PatientClass field
acr$PatientClass<-"I"

# keeps acr records that have a corresponding Subscriber ID in the cap list
acr_add <- acr[acr$Insurer.ID %in% cap$SUBSCRIBER_ID, ]

# creates a new list of records that don't have a corresponding Subscriber ID in the cap list
acr_missing <- acr[!acr$Insurer.ID %in% cap$SUBSCRIBER_ID, ]
acr_missing<-acr_missing[,c("Insurer.ID",
                            "Patient.First.Name",
                            "Patient.Last.Name",
                            "DOB" ,
                            "Eff.Admit.Date",
                            "Admit.Hospital",
                            "Source")]

# converts all facility names to proper case
acr_add$Admit.Hospital<-tolower(acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,acr_add$Admit.Hospital, perl=TRUE)

# cleans facility names to match acronyms in TrackVia
acr_add$Admit.Hospital<-gsub("Cooper Hospital/u m c","CUH",acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("Lourdes Med Ctr Of Burlington Cty","RGA",acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("Our Lady Of Lourdes Medical Center","LGA",acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("Virtua Mem Hospital Of Burlington County","RGA",acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("Virtua-West Jersey Health System","Virtua West Jersey",acr_add$Admit.Hospital)
acr_add$Admit.Hospital<-gsub("Alfred i Dupont Hsp For Children","Alfred I Dupont Hsp For Children",acr_add$Admit.Hospital)

# adds field to both files to designate them as from the ACR
acr_add$ACR<-"ACR"

# subsets columns for export
acr_add<-acr_add[,c("Insurer.ID",
                    "Eff.Admit.Date",
                    "Admit.Hospital",
                    "PatientClass",
                    "Confirmed.Discharged.Date",
                    "Diagnosis.Desc",
                    "Assigned.PCP.Name",
                    "Source",
                    "ACR")]

# renames SUBSCRIBER_ID field in cap for the vlookup function
cap<-rename(cap, c(SUBSCRIBER_ID="Insurer.ID"))

# looks up HIE ID of the Subscriber in the cap list
acr_add <- (merge(cap, acr_add, by = 'Insurer.ID'))

# renames columns
acr_add<-rename(acr_add, c(Insurer.ID="SUBSCRIBER_ID_LINK2",
                           Patient.ID.HIE="HIE.Import.Link",
                           Eff.Admit.Date="AdmitDate",
                           Admit.Hospital="Facility",
                           Confirmed.Discharged.Date="DischargeDate",
                           Assigned.PCP.Name="Provider",
                           Diagnosis.Desc="HistoricalDiagnosis"))

#creates utilization id in the acr file
acr_add$AdmitDate2<-as.Date(acr_add$AdmitDate, format="%m/%d/%Y")
acr_add$ID<-paste(acr_add$HIE.Import.Link, acr_add$AdmitDate2, acr_add$Facility, acr_add$PatientClass, sep="")

#Function to convert TrackVia dates to universal date format
exceldate <- function(date){
  
  if (!is.character(date)) {
    
    return(date)
    
  } else {
    
    date<-gsub(" ", "/",date)
    date<-gsub("Jan", "01",date)
    date<-gsub("Feb", "02",date)
    date<-gsub("Mar", "03",date)
    date<-gsub("Apr", "04",date)
    date<-gsub("May", "05",date)
    date<-gsub("Jun", "06",date)
    date<-gsub("Jul", "07",date)
    date<-gsub("Aug", "08",date)
    date<-gsub("Sep", "09",date)
    date<-gsub("Oct", "10",date)
    date<-gsub("Nov", "11",date)
    date<-gsub("Dec", "12",date)
    date<-as.Date(date, format="%m/%d/%Y")
    
    return(date)
    
  }
}

#creates Utilization id in the utilizations file
ut$AdmitDate2<-exceldate(ut$AdmitDate)
ut$ID<-paste(ut$HIE.Import.Link, ut$AdmitDate2, ut$Facility, ut$PatientClass, sep="")

#identifies records in the acr that are not already in trackvia
acr_add <- acr_add[!acr_add$ID %in% ut$ID, ]

#removes unused fields
acr_add$AdmitDate2<-NULL
acr_add$ID<-NULL

# starts a version of the acr file to import into the mpt
mpt_add <- acr_add

# identifies fields to export for mpt
mpt_add<-mpt_add[,c("SUBSCRIBER_ID_LINK2",
                    "HIE.Import.Link")]

# renames columns identified above
mpt_add<-rename(mpt_add, c(SUBSCRIBER_ID_LINK2="SUBSCRIBER_ID_LINK"))

# deduplicates mpt export
mpt_add<-unique(mpt_add)

# exports files to import into database
write.csv(mpt_add, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"MPT ACR Import",  ".csv", sep="")), row.names=FALSE)
write.csv(acr_add, (file=paste(format(Sys.Date(), "%Y-%m-%d-"),"Utilizations ACR Import",  ".csv", sep="")), row.names=FALSE)
