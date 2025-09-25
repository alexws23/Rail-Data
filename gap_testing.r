library(tidyverse)
library(purrr)
library(data.table)
library(hms)
library(visdat)
library(rlist)
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
csvs <- lapply(files, function(f) {
  read_csv(f, col_types = cols(Date = col_character()))
})

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

target_class <- class(csvs[[2]]$Antenna)

# Coerce the 'Date' column in all tibbles to that class
cleaned_csvs <- lapply(cleaned_csvs, function(df) {
  if (!inherits(df$Antenna, target_class)) {
    # Use as.<class>() to convert, assuming the base type is compatible
    df$Antenna <- switch(target_class[1],
                      character = as.character(df$Antenna),
                      df$Antenna)  # Default: no change
  }
  df
})

Chad <- bind_rows(cleaned_csvs) #Create one df with all of the data

Chad %>% 
  slice_sample(prop = .006) %>% 
  vis_dat()

mdy <- mdy(Chad$Date)
ymd <- ymd(Chad$Date) 
mdy[is.na(mdy)] <- ymd[is.na(mdy)] # some dates are ambiguous, here we give 
Chad$Date <- mdy        # mdy precedence over dmy

Chad_errors <- Chad %>% 
  subset(year(Date) == 2001) #subset the dataset to only include data from 2001, representing errors

unique(Chad_errors$filename)
Chad_errors %>% 
  group_by(filename) %>% 
  count()
  

##Chad_20210629.csv 4
##Chad_20220510.csv 891
##Chad_20220517.csv 2581
##Chad_20220526.csv 378
##Chad_20220609.csv 418
##Chad_20230220.csv 96094

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
  
