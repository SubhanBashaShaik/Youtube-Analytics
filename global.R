# Working Directory
library(lubridate)
library(dplyr)
videodata<-read.csv("IND Videos.csv")
videodata$tags<-gsub('"',"",videodata$tags)
delimit_pipe = function(x, n, i){
  do.call(c, lapply(x, function(X)
    paste(unlist(strsplit(X, "\\|"))[(n+1):(i)], collapse = "|")))
}
videodata$tags_searh<- delimit_pipe(x = videodata$tags, n = 2, i = 3)
tagsExtract_Names<-unique(videodata$tags_searh)
tagsExtract_Names<-tagsExtract_Names[tagsExtract_Names != "NA"]
tagsExtract_Names<-append(tagsExtract_Names, "ALL", 0)

videodata$Publish_Date<-as.Date(videodata$publish_time)
videodata$year_month<-paste(months(videodata$Publish_Date),"-",year(videodata$Publish_Date),sep = "")
year_month_Names<-unique(videodata$year_month)
year_month_Names<-year_month_Names[year_month_Names != "NA"]
