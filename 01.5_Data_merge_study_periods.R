#### DATA MERGE ####
library(ggplot2)
library(ggpubr)

#### Coatis ####
# FFT data 
FFT <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Coati_sleep/DATA/Coati_full_night_and_day_data.csv") 
FFT$local_timestamp <- as.POSIXct(x= FFT$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element
FFT <- FFT[-which(FFT$tag == "Serge_4670"),]
# BCI '23 data 
BCI23 <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Coati_sleep/DATA/Coati_full_night_and_day_data_BCI23.csv") 
BCI23$local_timestamp <- as.POSIXct(x= BCI23$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element

# same columns 
BCI23 <- try(BCI23[ , c( names(FFT) ), ]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.
#columns with collection time 
BCI23$data_col <- "BCI23"
FFT$data_col <- "FFT"
# make one dataframe 
Coatis <- rbind(FFT, BCI23)

#distribution check 
ave = ggplot(Coatis) + geom_density(aes(x= ave_vedba, color = data_col))
log = ggplot(Coatis) + geom_density(aes(x= log_vedba, color = data_col))
ggarrange(ave, log)

BBB = ggplot(BCI23) + geom_density(aes(x= log_vedba, color = tag))
FFF = ggplot(FFT)+ geom_density(aes(x= log_vedba, color = tag))
ggarrange(BBB, FFF)

#save df
sum(is.na(Coatis))
write.csv(Coatis, "/Users/capuchin_monkey/Documents/Sleep_on_BCI/Coati_sleep/DATA/Coati_full_night_and_day_data_all.csv", row.names = F) #saves all Coati data


#### Kinkajous ####
# FFT data 
FFT <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Kinkajou_sleep/DATA/Kinkajou_full_night_and_day_data.csv")  
FFT$local_timestamp <- as.POSIXct(x= FFT$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element
# BCI '23 data 
BCI23 <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Kinkajou_sleep/DATA/Kinkajou_full_night_and_day_data_BCI23.csv") 
BCI23$local_timestamp <- as.POSIXct(x= BCI23$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element

# same columns 
BCI23 <- try(BCI23[ , c( names(FFT) ), ]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.
#columns with collection time 
BCI23$data_col <- "BCI23"
FFT$data_col <- "FFT"
# make one dataframe 
Kinkajous <- rbind(FFT, BCI23)

#distribution check 
ave = ggplot(Kinkajous) + geom_density(aes(x= ave_vedba, color = data_col))
log = ggplot(Kinkajous) + geom_density(aes(x= log_vedba, color = data_col))
ggarrange(ave, log)

BBB = ggplot(BCI23) + geom_density(aes(x= log_vedba, color = tag))
FFF = ggplot(FFT)+ geom_density(aes(x= log_vedba, color = tag))
ggarrange(BBB, FFF)

#save df
sum(is.na(Kinkajous))
write.csv(Kinkajous, "/Users/capuchin_monkey/Documents/Sleep_on_BCI/Kinkajou_sleep/DATA/Kinkajou_full_night_and_day_data_all.csv", row.names = F) #saves all Kinkajou data 


#### Spider monkeys ####
# FFT data 
FFT <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Spider_sleep/DATA/Spider_full_night_and_day_data.csv") 
FFT$local_timestamp <- as.POSIXct(x= FFT$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element
# BCI '23 data 
BCI23 <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Spider_sleep/DATA/Spider_full_night_and_day_data_BCI23.csv") 
BCI23$local_timestamp <- as.POSIXct(x= BCI23$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element

# same columns 
BCI23 <- try(BCI23[ , c( names(FFT) ), ]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.
#columns with collection time 
BCI23$data_col <- "BCI23"
FFT$data_col <- "FFT"
# make one dataframe 
Spiders <- rbind(FFT, BCI23)

#distribution check 
ave = ggplot(Spiders) + geom_density(aes(x= ave_vedba, color = data_col))
log = ggplot(Spiders) + geom_density(aes(x= log_vedba, color = data_col))
ggarrange(ave, log)

BBB = ggplot(BCI23) + geom_density(aes(x= log_vedba, color = tag))
FFF = ggplot(FFT)+ geom_density(aes(x= log_vedba, color = tag))
ggarrange(BBB, FFF)

#save df
sum(is.na(Spiders))
write.csv(Spiders, "/Users/capuchin_monkey/Documents/Sleep_on_BCI/Spider_sleep/DATA/Spider_full_night_and_day_data_all.csv", row.names = F) #saves all Spider monkey data 


#### Capuchin monkeys ####
# FFT data 
FFT <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/Cap_sleep/DATA/Caps_full_night_and_day_data.csv") 
FFT$local_timestamp <- as.POSIXct(x= FFT$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='America/Panama' ) ## turns the timestamp into a POSIX element

#column with collection time 
FFT$data_col <- "FFT"

# make one dataframe for species
Caps <- FFT

#distribution check 
ggplot(Caps) + geom_density(aes(x= ave_vedba, color = data_col))
ggplot(Caps) + geom_density(aes(x= log_vedba, color = tag))


#### All species ####
#combine data from all species 
ALL <- rbind(Coatis, Kinkajous,Spiders, Caps)

# column for night 
ALL$night <- lubridate::date( ALL$local_timestamp - lubridate::hours(12)) 
# column for day 
ALL$day <- lubridate::date( ALL$local_timestamp) 
# column for individual name 
ALL$ID <- str_split( ALL$tag, "_", simplify = T )[,1]

# save df
write.csv(ALL, "/Users/capuchin_monkey/Documents/Sleep_on_BCI/DATA/All_full_night_and_day_data.csv", row.names = F)












