library(tidyverse)
library(ggthemes)

#Set directory
setwd("C:/Users/awsmilor/Git/Ward Lab/MOTUS/Data")

#Import CSV to R
data <- read.csv("./2025-09-24/tag_test.txt",header = FALSE)
options(digits=14)
colnames(data) <- c("port","time","freq","power","noise","S2N")

data_1 <- data[data$port != "C", ]
data <- data_1[data_1$port != "G", ]
data <- data[data_1$port != "S", ]
data <- data[data_1$port != "NA", ]
data <- data[substr(data$port, 1, 1) != '{', ]
rownames(data) <- NULL
data <- data[, 1:6]

#Change time from character to Date class
data <- data %>% 
  mutate(time = as_datetime(time))

## First, will describe summary statistics for the tests where the test transmitter was placed in the woods, which started around 13:24
#Filter data to only include rows with times after 13:24
wood_test_data <- data %>% 
  filter(time >= as_datetime("2025-09-24 18:24:00"))

#Set frequency threshold
upper_freq <- 5
lower_freq <-3

#remove columns associated with port 2, which was inactive
wood_test_clean <- wood_test_data %>% 
  filter(!port %in% c("p2","S")) %>% #remove extraneous ports
  filter(S2N > 18) %>% #filter to only include data with an S2N greater than 18. Usually, we use filter of 10, but that was still leaving some low values. Feel free to adjust it
  filter(freq > lower_freq & freq < upper_freq) %>% 
  group_by(port) %>% 
  mutate(diff = time - lag(time, default = dplyr::first(time))) %>% #calculate the time difference between pulses
  filter(diff > 0.25) %>% #filter out any pulses that occur less than a quarter second after the previous pulse
  ungroup() %>% 
  mutate(dongle = port) %>% #create a new column, duplicating the $port column
  mutate(dongle = ifelse(dongle=="p1","RTL",dongle)) %>% #convert ports into their respective dongles for readability
  mutate(dongle = ifelse(dongle=="p3","NE",dongle)) %>% 
  mutate(dongle = ifelse(dongle=="p4","FUN",dongle)) %>% 
  mutate(power = as.numeric(power)) #convert power column to the numeric class

#Scatter plots to visualize statistics
wood_test_clean %>%
  group_by(port) %>% 
  ggplot(aes(x=time, y=S2N, colour = dongle)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

wood_test_clean %>%
  group_by(port) %>% 
  ggplot(aes(x=time, y=power, colour = dongle)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

wood_test_clean %>%
  group_by(port) %>% 
  ggplot(aes(x=time, y=noise, colour = dongle)) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  theme_minimal()

#Calculating summary statistics
mean_S2N <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(mean(S2N))

mean_power <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(mean(power))
  
mean_noise <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(mean(noise))

max_noise <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(max(noise))

max_power <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(max(power))

max_S2N <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(max(S2N))

min_S2N <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(min(S2N))

min_power <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(min(power))

min_noise <- wood_test_clean %>%
  group_by(dongle) %>% 
  summarise(min(noise))

#Compiling summary statistics into a list
list_of_stats <- list(mean_noise,mean_power,mean_S2N,max_noise,max_power,max_S2N,min_noise,min_power,min_S2N)

#Merging list into one dataframe
summarystats <- purrr::reduce(.x = list_of_stats, merge, by = "dongle", all = T)
view(summarystats)

#Writing CSVs
write.csv(summarystats,file = "./dongleteststats_pulsefilter.csv")
write.csv(wood_test_clean,file = "./dongletestdata_cleaned_pulsefilter.csv")
write.csv(wood_test_data,file = "./dongletestdata_raw.csv")

