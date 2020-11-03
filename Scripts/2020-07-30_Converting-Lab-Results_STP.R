## WQO Compilation Script
## Code: Converting lab results to correct file format 
## 
# by K. Harker (Karly.Harker@gov.bc.ca)
# August 2019

install.packages("tidyverse")
install.packages("lubridate")

#install/load packages
library(tidyverse)
library(lubridate)

rm(list=ls(all=TRUE)) 

################################## reconfiguring testing

setwd("C:\\Columbia_WQO\\STP_Sampling")


#bring in results summary spreadsheet
teckdata <- read.csv(file="2015-09-18_Celgar-AEMP-TotalMetals.csv", header=FALSE, na.strings=c(""," ", "NA"),
                 fileEncoding="UTF-8-BOM")

#save units and detection limits
units <- dplyr::select(teckdata, V1:V2, V14)
units <- units[-c(1:5), ]
colnames(units) <- c("Parameter", "Unit", "RDL") #make the first row the column names

#remove units and detection limits from main data
teckdata <- dplyr::select(teckdata, V1, V3:V4,V6,V7,V9,V11,V13,V16)
#teckdata <- teckdata[-c(43:47), ]

teckdata$V1 <- gsub("\\s*\\([^\\)]+\\)","",as.character(teckdata$V1)) #takes out brackets


teckdata <- t(teckdata) #transpose


colnames(teckdata) <- teckdata[1,] #make the first row the column names
teckdata <- teckdata[-c(1), ] #get rid of first row
colnames(teckdata)[4] <- "Client.Sample.ID"

#remove columns that are entirely NA
teckdata <- teckdata[,colSums(is.na(teckdata))<nrow(teckdata)]


teckdata <- dplyr::as_tibble(teckdata) #make a tibble for work in dplyr

#get rid of spaces in column names :) 
names(teckdata)<-make.names(names(teckdata),unique = TRUE)

################################################################################################
################################################################################################
#check parameters are correct in gather function before proceeding, not always Hardness first/COD last

#reorganize data from wide to long format
teckdata <- gather(teckdata, key = "Parameter", value = "Measurement", 
                   Total.Hardness:Total.Sulphur)


#### To seperate out total/dissolved, remove from parameter name

teckdata <- teckdata %>% 
  dplyr::mutate(Fraction = if_else(stringr::str_detect(teckdata$Parameter, "Dissolved."), 
                                   "Dissolved", "Total"))
working <- teckdata

teckdata<- working

#remove the Dissolved from start of Parameter
teckdata$Parameter <- gsub("^.*\\.","", teckdata$Parameter)

#### To identify values below RDL

#create a column that indicates if the measurement is below RDL based upon < signal
teckdata$Below.RDL <- stringr::str_detect(teckdata$Measurement, "<", negate = FALSE)

#remove the < from data to change it to numeric
teckdata$Measurement <- gsub("<", "", teckdata$Measurement)

#save a copy of file to show the change of removing empty rows
output<-teckdata

#convert measurement data to number format
output$Measurement<- as.numeric(output$Measurement)

#get rid of rows with no data in measurement column
output<-na.omit(output, cols="Measurement")

names(output)<-make.names(names(output),unique = TRUE)

colnames(output)[4] <- "Client.Sample.ID"
colnames(output)[2] <- "Date.Sampled"

#re-order columns
output <- dplyr::select(output, Client.Sample.ID, Date.Sampled, Maxxam.ID, COC.Number, Parameter,
                        Fraction, Measurement, Below.RDL)

################################################################################################
################################################################################################
#change name of output file
#output code
write.csv(output,"2015-09-18_Celgar-AEMP-TotalMetals-Clean.csv")




#######################################################################################################
#######################################################################################################
#likely only need to do this once - depending on time span of lab results

###saving unit/detection limit outputs
#units <- units[-c(1:7), ]

#colnames(units) <- c("Parameter", "Lowest.Detection.Limit", "Units")

units <- units[rowSums(is.na(units)) != ncol(units),]

units$RDL<- as.numeric(as.character(units$RDL))

#get rid of rows with no data in detection limit/unit column
units<-na.omit(units, cols="RDL")


units$Parameter <- gsub("\\s*\\([^\\)]+\\)","",as.character(units$Parameter))

units <- units %>% 
  dplyr::mutate(Fraction = if_else(stringr::str_detect(units$Parameter, "Dissolved."), "Dissolved", "Total"))

#remove everything after - 
units$Parameter <- gsub("^.*\\ ","", units$Parameter)

output %<>% left_join(units, by = c("Parameter", "Fraction"))

write.csv(output,"2015-09-18_Celgar-AEMP-TotalMetals-Clean.csv")

write.csv(units,"2015-09-18_Celgar-AEMP-TotalMetals-UnitKey.csv")

#THE END ############################################################################################## 
#######################################################################################################
#######################################################################################################
#######################################################################################################

