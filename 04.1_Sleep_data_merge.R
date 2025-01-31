#### Sleep data merge ####
### of Spider monkeys, Capuchin monkeys, Coatis, and Kinkajous ###

library( stringr )
library(dplyr)
library(ggplot2)

################### Data read in ###################

## capuchin monkey's sleep
Cap_sleep <- read.csv( "/Users/Josie/Documents/Masterarbeit/DATA/Caps_sleep_per_nona425.csv" )

Cap_sleep$sunset <- as.POSIXct(as.character(Cap_sleep$sunset), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Cap_sleep$sunrise <- as.POSIXct(as.character(Cap_sleep$sunrise), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Cap_sleep$dark_start <- as.POSIXct(as.character(Cap_sleep$dark_start), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Cap_sleep$dark_end <- as.POSIXct(as.character(Cap_sleep$dark_end), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Cap_sleep$night <- as.Date(Cap_sleep$night)

## spider monkey's sleep
Spider_sleep <- read.csv( "/Users/Josie/Documents/Masterarbeit/DATA/Spider_sleep_per_nona485.csv" )

Spider_sleep$sunset <- as.POSIXct(as.character(Spider_sleep$sunset), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Spider_sleep$sunrise <- as.POSIXct(as.character(Spider_sleep$sunrise), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Spider_sleep$dark_start <- as.POSIXct(as.character(Spider_sleep$dark_start), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Spider_sleep$dark_end <- as.POSIXct(as.character(Spider_sleep$dark_end), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Spider_sleep$night <- as.Date(Spider_sleep$night)

### Add in some data 
Spider_sleep$data_col <- "FFT"
Spider_sleep[which(Spider_sleep$tag == "Magnolia_10505" | Spider_sleep$tag == "Daniel_10502" | Spider_sleep$tag == "Bean_10507"),]$data_col <- 'BCI23'
Spider_sleep$ID <- str_split_fixed(Spider_sleep$tag, '_', 2 )[ , 1]


## Coati's sleep
Coati_sleep <- read.csv( "/Users/Josie/Documents/Masterarbeit/DATA/Coati_sleep_per_nona49.csv" )

Coati_sleep$sunset <- as.POSIXct(as.character(Coati_sleep$sunset), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Coati_sleep$sunrise <- as.POSIXct(as.character(Coati_sleep$sunrise), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Coati_sleep$dark_start <- as.POSIXct(as.character(Coati_sleep$dark_start), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Coati_sleep$dark_end <- as.POSIXct(as.character(Coati_sleep$dark_end), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Coati_sleep$night <- as.Date(Coati_sleep$night)


## Kinkajou's sleep
Kinkajou_sleep <- read.csv( "/Users/Josie/Documents/Masterarbeit/DATA/Kinkajou_sleep_per_nona51.csv" )

Kinkajou_sleep$sunset <- as.POSIXct(as.character(Kinkajou_sleep$sunset), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Kinkajou_sleep$sunrise <- as.POSIXct(as.character(Kinkajou_sleep$sunrise), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Kinkajou_sleep$dark_start <- as.POSIXct(as.character(Kinkajou_sleep$dark_start), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Kinkajou_sleep$dark_end <- as.POSIXct(as.character(Kinkajou_sleep$dark_end), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
Kinkajou_sleep$day <- as.Date(Kinkajou_sleep$day)

## new column with species
Cap_sleep$species <- "Cebus capucinus"
Spider_sleep$species <- "Ateles geoffroyi"
Coati_sleep$species <- "Nasua narica" 
Kinkajou_sleep$species <- "Potos flavus"  

names(Cap_sleep)


#### Weather data ####

#Reading in the data 
rain_data = read.csv("/Users/Josie/Documents/Masterarbeit/DATA/bci_elect_cl_ra/bci_cl_ra_elect.csv")
temp_data = read.csv("/Users/Josie/Documents/Masterarbeit/DATA/bci_elect_cl_at/bci_cl_at_elect.csv")

#converting datetime
rain_data$datetime <- as.POSIXct(x= rain_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )
temp_data$datetime <- as.POSIXct(x= temp_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )

rain_data$date <- as.Date(rain_data$date, format = c('%d/%m/%Y'))
temp_data$date <- as.Date(temp_data$date, format = c('%d/%m/%Y'))

#check range of weather data 
range(rain_data$raw) 
range(temp_data$raw) 

## clean of missing/bad data 
rain_data = rain_data[which(rain_data$chk_note == 'good' | rain_data$chk_note == 'adjusted' ),]
temp_data = temp_data[which(temp_data$chk_note == 'good' | temp_data$chk_note == 'adjusted' ),]

# subset to study times
checkdates=data.frame(unique(cbind(Cap_sleep$tag, as.character(Cap_sleep$night))))
checkdates = rbind(checkdates,data.frame(unique(cbind(Spider_sleep$tag, as.character(Spider_sleep$night)))) )
checkdates = rbind(checkdates,data.frame(unique(cbind(Coati_sleep$tag, as.character(Coati_sleep$night)))) )
checkdates = rbind(checkdates,data.frame(unique(cbind(Kinkajou_sleep$tag, as.character(Kinkajou_sleep$day)))) )

colnames(checkdates)=c("tag","night_or_day")
checkdates$night_or_day <- as.Date( checkdates$night_or_day, origin = "1970-01-01", tz = "America/Panama" )

rain_data2 <- rain_data[which(rain_data$date %in% checkdates$night_or_day ==TRUE),]
temp_data2 <- temp_data[which(temp_data$date %in% checkdates$night_or_day ==TRUE),]

head(temp_data2)
tail(temp_data2) # seems like its recorded every 15 minutes

head(rain_data2)
tail(rain_data2) # recorded every 5 minutes

#creates means for the temperature per hour 
temp_data2$hour = as.factor(as.character(lubridate::hour(temp_data2$datetime)))
temp_data2 = temp_data2 %>% 
  group_by(hour) %>%
  summarise(mean = mean (raw))

#plot rain against hour of the day 
temp_data2$hour=as.numeric(as.character(temp_data2$hour))
ggplot(temp_data2, aes(x=hour, y=mean)) + geom_line()+theme_classic()

## create a column of the sum of rain every 15 min in temp_data 
temp_data$rain=NA
for(i in 2:nrow(temp_data)){
  if(length(which(rain_data$datetime<=temp_data$datetime[i] & rain_data$datetime>=temp_data$datetime[i-1]))==0){
    next
  }
  rain=rain_data$raw[which(rain_data$datetime<=temp_data$datetime[i] & rain_data$datetime>=temp_data$datetime[i-1])]
  rain=sum(rain)
  temp_data$rain[i]=rain
}


#### Append weather data to sleep data ####

## night start and end time 
night_end_time <- "07:00:00"
night_start_time <- "17:00:00"

## Capuchins 
Cap_sleep$rain_sleep_per = NA ## to write in the sum of rain during the dominant sleep period
Cap_sleep$rain_active_per = NA ## sum of rain during the dominant waking period 
Cap_sleep$min_temp_sleep_per = NA ## minimum temperature during the dominant sleep period 
Cap_sleep$max_temp_sleep_per = NA ## maximum temperature during the dominant sleep period 
Cap_sleep$mean_temp_sleep_per = NA ## mean temperature during the dominant sleep period 
Cap_sleep$min_temp_active_per = NA ## minimum temperature during the dominant waking period 
Cap_sleep$max_temp_active_per = NA ## maximum temperature during the dominant waking period 
Cap_sleep$mean_temp_active_per = NA ## mean temperature during the dominant waking period 

##Loop to merge weather to sleep data
for(i in 1:nrow(Cap_sleep)){
  ### first for the sleep period
  ## on the day at 17.00
  night_start <- as.POSIXct( paste( str_split_fixed( Cap_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  ## the next day at 07.00
  night_end <- as.POSIXct( paste( str_split_fixed( Cap_sleep$sunrise[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  temp <- temp_data[which(temp_data$datetime >= night_start & temp_data$datetime < night_end),]
  
  ## add to dataframe
  Cap_sleep$rain_sleep_per[i]=sum(temp$rain, na.rm = TRUE) 
  Cap_sleep$min_temp_sleep_per[i]=min(temp$at) 
  Cap_sleep$max_temp_sleep_per[i]=max(temp$at) 
  Cap_sleep$mean_temp_sleep_per[i]=mean(temp$at, na.rm = TRUE) 
  
  ### now for the previous active period
  ## on the day at 7.00
  day_start <- as.POSIXct( paste( str_split_fixed( Cap_sleep$sunset[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  ## on the day at 17.00
  day_end <- as.POSIXct( paste( str_split_fixed( Cap_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  temp2 <- temp_data[which(temp_data$datetime >= day_start & temp_data$datetime < day_end),]
  
  ## add to dataframe
  Cap_sleep$rain_active_per[i]=sum(temp2$rain, na.rm = TRUE) 
  Cap_sleep$min_temp_active_per[i]=min(temp2$at) 
  Cap_sleep$max_temp_active_per[i]=max(temp2$at) 
  Cap_sleep$mean_temp_active_per[i]=mean(temp2$at, na.rm = TRUE) 
  
}

head(Cap_sleep)
hist(Cap_sleep$min_temp_sleep_per, breaks = 100)

## Spider monkeys 
Spider_sleep$rain_sleep_per = NA ## to write in the sum of rain during the dominant sleep period
Spider_sleep$rain_active_per = NA ## sum of rain during the dominant waking period 
Spider_sleep$min_temp_sleep_per = NA ## minimum temperature during the dominant sleep period 
Spider_sleep$max_temp_sleep_per = NA ## maximum temperature during the dominant sleep period 
Spider_sleep$mean_temp_sleep_per = NA ## mean temperature during the dominant sleep period 
Spider_sleep$min_temp_active_per = NA ## minimum temperature during the dominant waking period 
Spider_sleep$max_temp_active_per = NA ## maximum temperature during the dominant waking period 
Spider_sleep$mean_temp_active_per = NA ## mean temperature during the dominant waking period 

##Loop to merge weather to sleep data
for(i in 1:nrow(Spider_sleep)){
  ### first for the sleep period
  ## on the day at 17.00
  night_start <- as.POSIXct( paste( str_split_fixed( Spider_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  ## the next day at 07.00
  night_end <- as.POSIXct( paste( str_split_fixed( Spider_sleep$sunrise[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  temp <- temp_data[which(temp_data$datetime >= night_start & temp_data$datetime < night_end),]
  
  ## add to dataframe
  Spider_sleep$rain_sleep_per[i]=sum(temp$rain, na.rm = TRUE) 
  Spider_sleep$min_temp_sleep_per[i]=min(temp$at) 
  Spider_sleep$max_temp_sleep_per[i]=max(temp$at) 
  Spider_sleep$mean_temp_sleep_per[i]=mean(temp$at, na.rm = TRUE) 
  
  ### now for the previous active period
  ## on the day at 7.00
  day_start <- as.POSIXct( paste( str_split_fixed( Spider_sleep$sunset[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  ## on the day at 17.00
  day_end <- as.POSIXct( paste( str_split_fixed( Spider_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  temp2 <- temp_data[which(temp_data$datetime >= day_start & temp_data$datetime < day_end),]
  
  ## add to dataframe
  Spider_sleep$rain_active_per[i]=sum(temp2$rain, na.rm = TRUE) 
  Spider_sleep$min_temp_active_per[i]=min(temp2$at) 
  Spider_sleep$max_temp_active_per[i]=max(temp2$at) 
  Spider_sleep$mean_temp_active_per[i]=mean(temp2$at, na.rm = TRUE) 
  
}

head(Spider_sleep)
hist(Spider_sleep$min_temp_active_per, breaks = 100)

## Coatis
Coati_sleep$rain_sleep_per = NA ## to write in the sum of rain during the dominant sleep period
Coati_sleep$rain_active_per = NA ## sum of rain during the dominant waking period 
Coati_sleep$min_temp_sleep_per = NA ## minimum temperature during the dominant sleep period 
Coati_sleep$max_temp_sleep_per = NA ## maximum temperature during the dominant sleep period 
Coati_sleep$mean_temp_sleep_per = NA ## mean temperature during the dominant sleep period 
Coati_sleep$min_temp_active_per = NA ## minimum temperature during the dominant waking period 
Coati_sleep$max_temp_active_per = NA ## maximum temperature during the dominant waking period 
Coati_sleep$mean_temp_active_per = NA ## mean temperature during the dominant waking period 

##Loop to merge weather to sleep data
for(i in 1:nrow(Coati_sleep)){
  ### first for the sleep period
  ## on the day at 17.00
  night_start <- as.POSIXct( paste( str_split_fixed( Coati_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  ## the next day at 07.00
  night_end <- as.POSIXct( paste( str_split_fixed( Coati_sleep$sunrise[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  temp <- temp_data[which(temp_data$datetime >= night_start & temp_data$datetime < night_end),]
  
  ## add to dataframe
  Coati_sleep$rain_sleep_per[i]=sum(temp$rain, na.rm = TRUE) 
  Coati_sleep$min_temp_sleep_per[i]=min(temp$at) 
  Coati_sleep$max_temp_sleep_per[i]=max(temp$at) 
  Coati_sleep$mean_temp_sleep_per[i]=mean(temp$at, na.rm = TRUE) 
  
  ### now for the previous active period
  ## on the day at 7.00
  day_start <- as.POSIXct( paste( str_split_fixed( Coati_sleep$sunset[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  ## on the day at 17.00
  day_end <- as.POSIXct( paste( str_split_fixed( Coati_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  temp2 <- temp_data[which(temp_data$datetime >= day_start & temp_data$datetime < day_end),]
  
  ## add to dataframe
  Coati_sleep$rain_active_per[i]=sum(temp2$rain, na.rm = TRUE) 
  Coati_sleep$min_temp_active_per[i]=min(temp2$at) 
  Coati_sleep$max_temp_active_per[i]=max(temp2$at) 
  Coati_sleep$mean_temp_active_per[i]=mean(temp2$at, na.rm = TRUE) 
  
}

head(Coati_sleep)
hist(Coati_sleep$min_temp_sleep_per, breaks = 100)


### Kinkajous
Kinkajou_sleep$rain_sleep_per = NA ## to write in the sum of rain during the dominant sleep period
Kinkajou_sleep$rain_active_per = NA ## sum of rain during the dominant waking period 
Kinkajou_sleep$min_temp_sleep_per = NA ## minimum temperature during the dominant sleep period 
Kinkajou_sleep$max_temp_sleep_per = NA ## maximum temperature during the dominant sleep period 
Kinkajou_sleep$mean_temp_sleep_per = NA ## mean temperature during the dominant sleep period 
Kinkajou_sleep$min_temp_active_per = NA ## minimum temperature during the dominant waking period 
Kinkajou_sleep$max_temp_active_per = NA ## maximum temperature during the dominant waking period 
Kinkajou_sleep$mean_temp_active_per = NA ## mean temperature during the dominant waking period 


## night start and end time 
night_end_time <- "05:00:00"
night_start_time <- "19:00:00"


##Loop to merge weather to sleep data
for(i in 1:nrow(Kinkajou_sleep)){
  ### first for the sleep period
  
  ## on the day at 5.00
  day_start <- as.POSIXct( paste( str_split_fixed( Kinkajou_sleep$sunset[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  ## on the day at 19.00
  day_end <- as.POSIXct( paste( str_split_fixed( Kinkajou_sleep$sunset[i], " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
  
  temp <- temp_data[which(temp_data$datetime >= day_start & temp_data$datetime < day_end),]
  
  
  ## add to dataframe
  Kinkajou_sleep$rain_sleep_per[i]=sum(temp$rain, na.rm = TRUE) 
  Kinkajou_sleep$min_temp_sleep_per[i]=min(temp$at) 
  Kinkajou_sleep$max_temp_sleep_per[i]=max(temp$at) 
  Kinkajou_sleep$mean_temp_sleep_per[i]=mean(temp$at, na.rm = TRUE) 
  
  ### now for the previous active period
  ## on the day before at 19.00
  previous_day <-  as.character((as.Date( Kinkajou_sleep$day[i], origin = "1970-01-01", tz = "America/Panama" )  - 1 ))
  
  night_start <- as.POSIXct( paste( previous_day, night_start_time ), tz = 'America/Panama' )
  
  ## on the day at 05.00
  night_end <- as.POSIXct( paste( str_split_fixed( Kinkajou_sleep$sunrise[i], " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
  
  temp2 <- temp_data[which(temp_data$datetime >= night_start & temp_data$datetime < night_end),]
  
  ## add to dataframe
  Kinkajou_sleep$rain_active_per[i]=sum(temp2$rain, na.rm = TRUE) 
  Kinkajou_sleep$min_temp_active_per[i]=min(temp2$at) 
  Kinkajou_sleep$max_temp_active_per[i]=max(temp2$at) 
  Kinkajou_sleep$mean_temp_active_per[i]=mean(temp2$at, na.rm = TRUE) 
  
}

head(Kinkajou_sleep)
hist(Kinkajou_sleep$min_temp_active_per, breaks = 100)







#### Bind all together ####

#let's see what we'll need
names(Cap_sleep)

## start with diurnals 
sleep_all_diurnal = rbind(Cap_sleep, Spider_sleep , Coati_sleep)

names(sleep_all_diurnal)
names(Kinkajou_sleep)

## rename the columns to match Kinkajou names
names( sleep_all_diurnal ) <- c( "day", "tag", "sunset", "sunrise", "day_dur", "dark_start", "dark_end", "light_period_dur", "TST_whole_day", "TST_sleep_per", "WASO", "prev_active_per_sleep", "follow_active_per_sleep", "sleep_eff", "wake_bouts", "frag_wake_bouts",  "summed_VeDBA", "day_VeDBA_corr", "ave_vedba", "prev_active_per_ave_vedba", "poss_dark_bursts", "n_dark_bursts", "poss_day_bursts", "n_day_bursts", "max_time_diff", "n_bursts", "max_dark_time_diff", "max_day_time_diff", "day_num", "fraction", "phase", "angle",  "data_col", "ID", "species", "rain_sleep_per", "rain_active_per", "min_temp_sleep_per", "max_temp_sleep_per", "mean_temp_sleep_per", "min_temp_active_per", "max_temp_active_per", "mean_temp_active_per"  )

## bind all 4 species
sleep_all = rbind(sleep_all_diurnal, Kinkajou_sleep)

## rename again
names( sleep_all ) <- c( "night_or_day", "tag", "sunset", "sunrise", "night_or_day_dur", "dark_start", "dark_end", "dark_or_light_period_dur", "TST_whole_day", "TST_sleep_per", "WASO", "prev_active_per_sleep", "follow_active_per_sleep", "sleep_eff", "wake_bouts", "frag_wake_bouts",  "summed_VeDBA", "night_or_day_VeDBA_corr", "ave_vedba", "prev_active_per_ave_vedba", "poss_dark_bursts", "n_dark_bursts", "poss_day_bursts", "n_day_bursts", "max_time_diff", "n_bursts", "max_dark_time_diff", "max_day_time_diff", "night_or_day_num", "fraction", "phase", "angle",  "data_col","ID", "species", "rain_sleep_per", "rain_active_per", "min_temp_sleep_per", "max_temp_sleep_per", "mean_temp_sleep_per", "min_temp_active_per", "max_temp_active_per", "mean_temp_active_per"  )

## keep only the necessary columns
sleep_all <- sleep_all[ , c( 'ID', 'night_or_day', 'tag' ,'species', 'sunset', 'sunrise', 'night_or_day_dur', 'TST_whole_day', 'TST_sleep_per', 'WASO', 'prev_active_per_sleep', 'follow_active_per_sleep', 'sleep_eff', 'wake_bouts', 'frag_wake_bouts', 'prev_active_per_ave_vedba', 'data_col', 'rain_sleep_per', 'rain_active_per', 'min_temp_sleep_per', 'max_temp_sleep_per', 'mean_temp_sleep_per', 'min_temp_active_per', 'max_temp_active_per', 'mean_temp_active_per' ) ] 



## do the same just for the diurnals 
## rename again
names( sleep_all_diurnal ) <- c( "night", "tag", "sunset", "sunrise", "night_dur", "dark_start", "dark_end", "dark_period_dur", "TST_whole_day", "TST_sleep_per", "WASO", "prev_active_per_sleep", "follow_active_per_sleep", "sleep_eff", "wake_bouts", "frag_wake_bouts",  "summed_VeDBA", "night_VeDBA_corr", "ave_vedba", "prev_active_per_ave_vedba", "poss_dark_bursts", "n_dark_bursts", "poss_day_bursts", "n_day_bursts", "max_time_diff", "n_bursts", "max_dark_time_diff", "max_day_time_diff", "night_num", "fraction", "phase", "angle", "data_col", "ID", "species", "rain_sleep_per", "rain_active_per", "min_temp_sleep_per", "max_temp_sleep_per", "mean_temp_sleep_per", "min_temp_active_per", "max_temp_active_per", "mean_temp_active_per"  )
## keep only the necessary columns
sleep_all_diurnal <- sleep_all_diurnal[ , c( 'ID', 'night', 'tag' ,'species', 'sunset', 'sunrise', 'night_dur', 'TST_whole_day', 'TST_sleep_per', 'WASO', 'prev_active_per_sleep', 'follow_active_per_sleep', 'sleep_eff', 'wake_bouts', 'frag_wake_bouts', 'prev_active_per_ave_vedba', 'data_col', 'rain_sleep_per', 'rain_active_per', 'min_temp_sleep_per', 'max_temp_sleep_per', 'mean_temp_sleep_per', 'min_temp_active_per', 'max_temp_active_per', 'mean_temp_active_per' ) ] 


#### save dataframes ####

## all single species ones
write.csv(Cap_sleep, "/Users/Josie/Documents/Masterarbeit/Output/Cap_sleep_with_weather.csv", row.names = F)
write.csv(Spider_sleep, "/Users/Josie/Documents/Masterarbeit/Output/Spider_sleep_with_weather.csv", row.names = F) 
write.csv(Coati_sleep, "/Users/Josie/Documents/Masterarbeit/Output/Coati_sleep_with_weather.csv", row.names = F) 
write.csv(Kinkajou_sleep, "/Users/Josie/Documents/Masterarbeit/Output/Kinkajou_sleep_with_weather.csv", row.names = F) 

## combined species dataframes
write.csv(sleep_all_diurnal, "/Users/Josie/Documents/Masterarbeit/Output/Diurnal_sleep_with_weather.csv", row.names = F) 

write.csv(sleep_all, "/Users/Josie/Documents/Masterarbeit/Output/All_species_sleep_with_weather.csv", row.names = F) 





