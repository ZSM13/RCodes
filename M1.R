# Set working directory
setwd("Y:/")

# Reads the file and refers to it as 'data'
data <- read.csv("Y:/Active Residents October 2014.csv")

# Sets all blanks cells to NA
data[data==""] <- NA

# Removes rows that are all NA
data2<-data[apply(data, 1, function(y) !all(is.na(y))),]

# Separates the X.Household.Member.Name field into First and Last Names
data2$LastName = as.character(lapply(strsplit(as.character(data2$X.Household.Member.Name), split=", "), "[", 1))
data2$FirstName = as.character(lapply(strsplit(as.character(data2$X.Household.Member.Name), split=", "), "[", 2))

# Deletes the row with the Super's Unit
data3<-data2[!(data2$X.Household.Member.Name=="Super'S, Unit"),]

# Separates the Apt field
data3$Floor = as.character(lapply(strsplit(as.character(data3$X.Apt.), split="-"), "[", 1))
data3$AptNum = as.character(lapply(strsplit(as.character(data3$X.Apt.), split="-"), "[", 2))

# Removes LR or HR from the AptNum field
data3$AptNum2 <- gsub("LR", "", data3$AptNum)
data3$AptNum3 <- gsub("HR","",  data3$AptNum2)

# Deletes unused fields
data3$AptNum<-NULL
data3$AptNum2<-NULL

# Determines if the apartment is a LR or HR by looking to see if the record's floor is equal to 15
data3$FN<-ifelse(data3$Floor==15, "HR", "LR")

# Concatenates AptNum3 field and the FN field#
data3$Apt.No<-paste0(data3$AptNum3, data3$FN, "")
data4<-reshape::rename(data3, c(Apt="Apt_old"))

# Deletes unused fields
data4$Floor<-NULL  
data4$AptNum3<-NULL
data4$FN<-NULL

# Adds a new field and populates with the first day of the current month
data4$DateLastResidentList<- format(Sys.time(), "%m-01-%Y")

# Removes X. from column names
names(data4) <- sub("X.", "", sub("\\(.*\\)", "", names(data4)))

# Renames Fields
require("reshape")
data4<-reshape::rename(data4, c(Apt.="Apt_old"))
data4<-reshape::rename(data4, c(Apt.No="Apt.No."))
data4<-reshape::rename(data4, c(Household.Member.Name="HouseholdMemberName"))
#data4<-reshape::rename(data4, c(Move.In.Date="Move.In.Date.old"))
#data4<-reshape::rename(data4, c(DOB="DOB.old"))

# Deletes the 3  totals rows in the bottom of the spreadsheet
data5<- data4[!(data4$HouseholdMemberName=="# Of Members:" | data4$HouseholdMemberName=="# Of Households:" | data4$HouseholdMemberName=="# Of Students:"),]

# Renames Household Member Name
data5<-reshape::rename(data5, c(Household.Member.Name="Household Member Name"))

# Removes all NA values
data5[is.na(data5)]<-""

# Exports csv file
write.csv(data5,(file=paste ( format(Sys.Date(), "%Y-%m-%d-"),"Active Residents Clean File", ".csv", sep="")), row.names=FALSE)
