library(tidyverse)
library(purrr)
library(visdat)
library(rlist)

#Goal: Look at if any of the rails that are missing departure data are a result of missing data.

setwd("C:/Users/awsmilor/Git/Ward Lab/Rail-Data/Data")

#Read CSV into R environment
rails_spring <- read.csv("./Master_list_of_Tagged_Rails.csv") %>% 
  subset(select = -c(Trap, Wing.Cord,Culmen, Tarsus,Mass))
rails_fall <- read.csv("./Fall_Master_list_of_Tagged_Rails.csv") %>% 
  subset(select = -c(Trap, Wing.Cord,Culmen, Tarsus,Mass))
rails <- rbind(rails_spring,rails_fall)

#filter to only include rails with unknown departure dates whose tags didn't fall off
rail_unk_dep <- rails %>% 
  filter(Date.Departed %in% c("","unk",NA)) %>% 
  filter(!grepl("trans",Departure.Defined.by.Local.Array)) %>% 
  mutate(Date_Tagged = mdy(Date_Tagged)) #format Date_Tagged variable as a date
  #mutate(Date.Departed = mdy(Date.Departed)) #format Date.Departed variable as a date

#Filter all rail data to just include ones with a departure date
"%ni%" <- Negate("%in%")
good_rails <- rails %>% 
  filter(Date.Departed %ni% c("","unk",NA)) %>% 
  mutate(Date_Tagged = mdy(Date_Tagged)) %>% #format Date_Tagged variable as a date
  mutate(Date.Departed = mdy(Date.Departed)) #format Date.Departed variable as a date

good_codes <- good_rails$Code

good_rails <- good_rails %>% 
  mutate(diff = 1+(as.numeric(difftime(Date.Departed, Date_Tagged, units = "days")))) #Not needed, but this is how to calculate minimum stopover duration

mean(good_rails$Min.Stopover.Duration) #determine mean minimum stopover duration (~11)
max(good_rails$Min.Stopover.Duration) #determine maximum minimum stopover duration (67)
sd(good_rails$Min.Stopover.Duration) #calculate SD (~12)

#Filter all of the data from chad to only include files with the power issue (dates showing 2001)
error_data <- All_Data %>%
  filter(filename %in% c("Chad_20210629", "Chad_20220510", "Chad_20220517", "Chad_20220526", "Chad_20220609", "Chad_20230220", "BannerEast_20230503", "BannerWest_20220320", "Pumphouse_20230512", "SwanBay_20230821"))

#Create a list with all of the dates included in the files with issues
suspect_dates <- unique(error_data$Date)

#Create a list with all of the dates when rails were tagged
tagged_dates <- unique(rail_unk_dep$Date_Tagged)

#Determines the difference between the tagged date and the suspect dates and if they occurred within 35 (2 SD of mean departure after tagging date) days.
within_10_days <- sapply(tagged_dates, function(tagged_dates) {
  any(abs(difftime(tagged_dates, suspect_dates, units = "days")) <= 66)
})

#Determines which dates met the criteria above
matching_tagged_dates <- tagged_dates[within_10_days]

#Filter the rail data to only include birds that were tagged within 66 days of the problematic data. 
suspect_records <- rail_unk_dep %>% 
  filter(Date_Tagged %in% matching_tagged_dates)

#Filter all detections to only include the detections of birds with suspect codes.
suspect_codes <- suspect_records$Code #create list object of suspect codes

suspect_detections <- Chad
  #filter(Code %in% suspect_codes)

#Filter to only include records with the 2001 error
search_2001 <- suspect_detections %>% 
  filter(year(Date) == 2001) %>% 
  filter(Code %ni% good_codes)

#Print all of the codes affected by the 2001 error
unique(search_2001$Code) #321 325 111 327 125 367 499
## 321: Suspected Tag Fell off
## 325: Suspected tag fell off
## 111: no clear departure. Last detected 5/20/22 at 08:08. Data gap started on 5/20 thru 6/9.
## 327: suspect departure but weak and noisy.
## 125: not detected (may have been detected the next spring)
## 367: last likely detections stopped on 4/25.
## 499: tag was moving until 8/12/21, then laid there?? (likely bred)

All_Data %>% 
  filter(Code == 33) %>% 
  #filter(year(Date) == 2001) %>% 
  #filter(year(Date) != 2012) %>% 
  #filter(year(Date) == 2022) %>% 
  group_by(Date) %>%
  ggplot(aes(x = Date, y = Power, colour = filename)) + 
  geom_point()

