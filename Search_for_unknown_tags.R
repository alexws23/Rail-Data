
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(zoo)


############# Filter #####################
## baseline_freq <- 166.376
## tag_freq <- 166.380

upper_freq <- 5
lower_freq <-3

signal_duration <- 0.015

#####################################################
S2N_cutoff <- 10

#### set working directory 
setwd("C:/Users/awsmilor/Git/Ward Lab/MOTUS/Data")
# read in the file
data <- read.csv("./2025-09-24/tag_test.txt",header = FALSE)
options(digits=14)
colnames(data) <- c("port","time","freq","power","noise","S2N")

###### might need to delete problematic rows with odd characters by hand in notepad
##################################################################################################
data_1 <- data[data$port != "C", ]
data <- data_1[data_1$port != "G", ]
data <- data[data_1$port != "S", ]
data <- data[data_1$port != "NA", ]
data <- data[substr(data$port, 1, 1) != '{', ]
rownames(data) <- NULL
data <- data[, 1:6]

##################################################################################################
########## subset by port / antenna ######
data_p1 <- data[which(data$port == "p1"),]
data_p2 <- data[which(data$port == "p2"),]
data_p3 <- data[which(data$port == "p3"),]
data_p4 <- data[which(data$port == "p4"),]

########## difference between the sensor gnome frequency and the frequency of tranmitter(s)
# freq set at 166.376
data_p1_2 <- subset(data_p1,freq > lower_freq & freq < upper_freq)
data_p2_2 <- subset(data_p2,freq > lower_freq & freq < upper_freq)
data_p3_2 <- subset(data_p3,freq > lower_freq & freq < upper_freq)
data_p4_2 <- subset(data_p4,freq > lower_freq & freq < upper_freq)

######################################## Port 1 ###########################################################
###################### Get time difference #################################################################
df_p1 <- data_p1_2 %>%
  mutate(diff = time - lag(time, default = dplyr::first(time)))
options(digits=3)

######### If difference in time is less than 0.015 delete the row  ######  
df_p1_2 <- subset(df_p1, diff > signal_duration )

###### Signal 2 Noise filter #####################################################
df_p1_3 <- subset(df_p1_2, S2N > S2N_cutoff )

################## round to 3 decimal places #############################################
df_p1_3$diff <- round(df_p1_3$diff, 3)


######################################## Port 2 ###########################################################
###################### Get time difference #################################################################
df_p2 <- data_p2_2 %>%
  mutate(diff = time - lag(time, default = dplyr::first(time)))
options(digits=3)

######### If difference in time is less than 0.015 delete the row  ######  
df_p2_2 <- subset(df_p2, diff > signal_duration )

###### Signal 2 Noise filter #####################################################
df_p2_3 <- subset(df_p2_2, S2N > S2N_cutoff )

################## round to 3 decimal places #############################################
df_p2_3$diff <- round(df_p2_3$diff, 3)

######################################## Port 3 ###########################################################
###################### Get time difference #################################################################
df_p3 <- data_p3_2 %>%
  mutate(diff = time - lag(time, default = dplyr::first(time)))
options(digits=3)

######### If difference in time is less than 0.015 delete the row  ######  
df_p3_2 <- subset(df_p3, diff > signal_duration )

###### Signal 2 Noise filter #####################################################
df_p3_3 <- subset(df_p3_2, S2N > S2N_cutoff )

################## round to 3 decimal places #############################################
df_p3_3$diff <- round(df_p3_3$diff, 3)

######################################## Port 4 ###########################################################
###################### Get time difference #################################################################
df_p4 <- data_p4_2 %>%
  mutate(diff = time - lag(time, default = dplyr::first(time)))
options(digits=3)

######### If difference in time is less than 0.015 delete the row  ######  
df_p4_2 <- subset(df_p4, diff > signal_duration )

###### Signal 2 Noise filter #####################################################
df_p4_3 <- subset(df_p4_2, S2N > S2N_cutoff )

################## round to 3 decimal places #############################################
df_p4_3$diff <- round(df_p4_3$diff, 3)

################################################################
###Function to generate sliding windows of length 'window_size'
################################################################

generate_windows <- function(vector, window_size) {
  lapply(1:(length(vector) - window_size + 1), function(i) {
    vector[i:(i + window_size - 1)]
  })
}

# Function to hash sequences and count occurrences

count_sequences <- function(column, window_size) {
  sequences <- generate_windows(column, window_size)
  hash_table <- list()
  for (seq in sequences) {
    hash <- paste(seq, collapse = "_")
    if (hash %in% names(hash_table)) {
      hash_table[[hash]] <- hash_table[[hash]] + 1
    } else {
      hash_table[[hash]] <- 1
    }
  }
  return(hash_table)
}

# Count occurrences of repeating sequences of length 3
############# Port 1 #####################################################
sequence_counts <- count_sequences(df_p1_3$diff, 3)
sequences <- as.data.frame(sequence_counts)
seq_1 <- t(sequences)
write.csv(seq_1,"possible_tags_port_1.csv")
row.names(df_p1_3) <- NULL
nrow(df_p1_3)
colnames(df_p1_3) <- c("port_1","time","freq_1","power_1","noise_1","S2N_1","diff_1")

############# Port 2 #####################################################
sequence_counts <- count_sequences(df_p2_3$diff, 3)
sequences <- as.data.frame(sequence_counts)
seq_2 <- t(sequences)
write.csv(seq_2,"possible_tags_port_2.csv")
row.names(df_p2_3) <- NULL
nrow(df_p2_3)
colnames(df_p2_3) <- c("port_2","time","freq_2","power_2","noise_2","S2N_2","diff_2")

############# Port 3 #####################################################
sequence_counts <- count_sequences(df_p3_3$diff, 3)
sequences <- as.data.frame(sequence_counts)
seq_3 <- t(sequences)
write.csv(seq_3,"possible_tags_port_3.csv")
row.names(df_p3_3) <- NULL
nrow(df_p3_3)
colnames(df_p3_3) <- c("port_3","time","freq_3","power_3","noise_3","S2N_3","diff_3")

############# Port 4 #####################################################
sequence_counts <- count_sequences(df_p4_3$diff, 3)
sequences <- as.data.frame(sequence_counts)
seq_3 <- t(sequences)
write.csv(seq_3,"possible_tags_port_4.csv")
row.names(df_p4_3) <- NULL
nrow(df_p4_3)
colnames(df_p4_3) <- c("port_4","time","freq_4","power_4","noise_4","S2N_4","diff_4")

###################################################################
##################################################################
x <- merge(df_p1_3,df_p2_3, by="time", all=TRUE)
x <- merge(x,df_p3_3, by="time", all=TRUE)
x <- merge(x,df_p4_3, by="time", all=TRUE)

options(digits=14)

all_data <- x[order(x$time), ]
all_data$time <- as.POSIXct(all_data$time, origin="1970-01-01")

