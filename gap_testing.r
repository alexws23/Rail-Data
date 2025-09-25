library(tidyverse)
library(purrr)
library(data.table)
library(hms)
setwd("./Data")

Chautauqua <- read.csv("./Chautauqua_20220317.csv")

mdy <- mdy(Chautauqua$Date)
ymd <- ymd(Chautauqua$Date) 
mdy[is.na(mdy)] <- ymd[is.na(mdy)] # some dates are ambiguous, here we give 
Chautauqua$Date <- mdy        # mdy precedence over dmy

if(nrow(Chautauqua) == 0){
  Chautauqua_counts <- empty_data
}else{
  #Chautauqua$date_2 <- as.Date(strptime (Chautauqua$date_2, '%Y-%m-%d'))
  counts <- aggregate(
    count ~ wkday + hour,
    data=transform(Chautauqua,
                   wkday=Date,
                   hour=format(as.POSIXct(Time,format="%H:%M:%S"), "%H"),
                   count=1),
    FUN=sum
  )
  Chautauqua_counts <- counts[order(counts$wkday),]
  colnames(Chautauqua_counts) <- c("Date","Hour","Chautauqua_Count")}

write.csv(Chautauqua_counts, "./Chautauqua_counts.csv")
View(Chautauqua_counts)

#Chad testing
#Set Working Directory to folder with all of the MOTUS tower data you want to work with.
setwd("./Chad")
#Create and object that lists all file names
files <- list.files()
names(files) = files
#read in all CSV files with the names in the corresponding object
csvs <- lapply(files, read_csv)

#create column with original filename
csvs <- lapply(names(csvs), function(name){
  bind_cols(
    data_frame(filename = rep(gsub('.csv','',name), nrow(csvs[[name]]))),
    csvs[[name]])})

#renames tibbles to their original filename
names(csvs) <- files

columns_to_remove <- c("X", "...") #create object with names of columns you want to remove
cleaned_csvs <- map(csvs, ~ .x %>% select(-starts_with(columns_to_remove))) #remove all columns that match the specified cases. In this case, I removed all columns starting with "..." and "X"
newcolnames <- c("filename","Date","Time","RXID","Freq","Antenna","Protocol","Code","Power","Squelch", "Noise Level","Pulse Width 1","Pulse Width 2","Pulse Width 3","Pulse Width 4","Pulse Interval 1", "Pulse Interval 2","Pulse Interval 3")
cleaned_csvs <- lapply(cleaned_csvs, setNames, nm = newcolnames)

list2env(cleaned_csvs, envir = .GlobalEnv) #Add all of the tibbles in the list to the R environment as separate data frames


Chad_20230512_revised.csv_test <- Chad_20230512_revised.csv %>% 
  mutate(Time = as_hms(Time))

Chad <- rbind(Chad_20210428.csv,Chad_20210503.csv,Chad_20210506.csv
,Chad_20210511.csv, Chad_20210519.csv,Chad_20210529.csv,Chad_20210604.csv,Chad_20210610.csv,
Chad_20210621.csv,Chad_20210628.csv,Chad_20210629.csv,Chad_20211001.csv,Chad_20211007.csv,Chad_20211018.csv,
Chad_20211027.csv,Chad_20211110.csv,Chad_20211216.csv,Chad_20220303.csv,Chad_20220426.csv,Chad_20220510.csv,
Chad_20220517.csv,Chad_20220526.csv,Chad_20220609.csv,Chad_20220805.csv,Chad_20230220.csv,Chad_20230414_revised.csv,
Chad_20230503_revised.csv,Chad_20230512_revised.csv,Chad_20230609_revised.csv,Chad_20230821.csv
)

mdy <- mdy(Chad$Date) 
ymd <- ymd(Chad$Date) 
mdy[is.na(mdy)] <- ymd[is.na(mdy)] # some dates are ambiguous, here we give 
Chad$Date <- mdy        # mdy precedence over dmy

Chad_errors <- Chad %>% 
  subset(year(Date) == 2001)

unique(Chad_errors$file_name)
Chad_errors %>% 
  group_by(file_name) %>% 
  count()
  

##Chad_11.csv = Chad_20210629.csv 4
##Chad_20.csv = Chad_20220510.csv 891
##Chad_21.csv = Chad_20220517.csv 2581
##Chad_22.csv = Chad_20220526.csv 378
##Chad_23.csv = Chad_20220609.csv 418
##Chad_25.csv = Chad_20230220.csv 96094

if(nrow(Chad) == 0){
  Chad_counts <- empty_data
}else{
  #Chad$date_2 <- as.Date(strptime (Chad$date_2, '%Y-%m-%d'))
  counts <- aggregate(
    count ~ wkday + hour,
    data=transform(Chad,
                   wkday=Date,
                   hour=format(as.POSIXct(Time,format="%H:%M:%S"), "%H"),
                   count=1),
    FUN=sum
  )
  Chad_counts <- counts[order(counts$wkday),]
  colnames(Chad_counts) <- c("Date","Hour","Chad_Count")}


write.csv(Chad_counts, "Chad_counts_2.csv")
View(Chad_counts)

#Determine wrong years
list <- list(Chad_1,Chad_2, Chad_3)

mdy <- mdy(Chad$Date)
ymd <- ymd(Chad$Date) 
mdy[is.na(mdy)] <- ymd[is.na(mdy)] # some dates are ambiguous, here we give 
Chad$Date <- mdy        # mdy precedence over dmy

mdy <- mdy(Chad_1$Date)
Chad_1$Date <- mdy

mdy <- mdy(Chad_2$Date)
Chad_2$Date <- mdy

mdy <- mdy(Chad_3$Date)
Chad_3$Date <- mdy

Chad_1_Test <- Chad_1 %>%
  subset(year(Date) == 2001)

for (i in 1:nrow) {
  
}
  
