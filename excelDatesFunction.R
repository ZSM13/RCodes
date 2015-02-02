
#Function to convert TrackVia dates to R dates
exceldate <- function(date){
  
  if (!is.factor(date)) {

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
