#### Validation dataframe prep ####

library(lubridate)
library(readr)

# load behavior data 
df = read.csv("/Users/capuchin_monkey/Documents/Cap_Validation/DATA/BORIS_output/BORIS-2023-10-03-1-cap-agg-events-all-behaviors.csv")
names(df)
head(df)
unique(df$Comment.stop)

night <- as.Date("2023-10-03")

#### prep dataframe ####

# subset data for necessary columns
df2 <- df[ , c( 'Observation.id', 'Behavior' ,'Behavior.type', 'Start..s.', 'Stop..s.', 'Duration..s.', 'Image.index.start', 'Image.index.stop', 'Comment.start' ),] 

# rename columns 
names(df2) <- c( 'Observation_ID', 'behavior', 'behavior_type', 'start_s', 'stop_s', 'duration_s', 'image_start', 'image_stop', 'comment' )

#new dataframe just for observations & for timestamps 
observation <- df2[which(df2$behavior_type == "STATE"),]
timestamps <- df2[which(df2$behavior_type == "POINT"),]


#### Checking for potential drift of the camera
#save the timestamp at the begining 
timestamp_begin <- as.POSIXct(paste(night, timestamps$comment[1]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
timestamp_end <- as.POSIXct(paste("2023-10-04", timestamps$comment[8]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
difftime(timestamp_begin, timestamp_end, units = "mins")

#video times at start and beginning 
time_start <- timestamps$start_s[1]
time_end <- timestamps$start_s[8]
diff(c(time_end, time_start) )

#compare to timestamps
drift <- diff(c(diff(c(time_end, time_start) ), difftime(timestamp_begin, timestamp_end, units = "secs")))
#see length of video
video_dur <- difftime(timestamp_end, timestamp_begin, units = "secs")
#after which time do we have 0.01s drift?
difftime(timestamp_begin, timestamp_end, units = "mins")/4
#drift per hour?
drift_per_hour <- (drift/as.numeric(video_dur))*60*60
drift_per_min <- (drift/as.numeric(video_dur))*60

# subset observation df with only needed behaviors 
observation2 <- observation
observation <- observation[which(observation$behavior == "out of sight" | observation$behavior =="wake" |observation$behavior == "sleep" | observation$behavior =="resting wakeful"),]


#### timestamps ####
options( digits.secs = 6 )
#see time at start of video
as.POSIXct(timestamps$comment[1], format = "%H:%M:%OS", tz = "UTC") 
timestamps$start_s[1]
time_start <- fast_strptime("2023-10-03 22:34:29.800", format = "%Y-%m-%d %H:%M:%OS")
  
#create columns with UTC timestamps
observation$time_start_UTC <- as.POSIXct(ceiling_date(time_start + observation$start_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$time_end_UTC <- as.POSIXct(ceiling_date(time_start +observation$stop_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$dur_crr <- observation$time_end - observation$time_start

# GPS time is used in the collars
timestamp_correction <- 18 #seconds to correct from UTC to GPS

observation$start_timestamp_corr <- observation$time_start_UTC + timestamp_correction
observation$stop_timestamp_corr <- observation$time_end_UTC  + timestamp_correction

#create a night column 
observation$local_timestamp <- lubridate::with_tz(observation$start_timestamp_corr, tzone = "America/Panama")
observation$night <- lubridate::date( observation$local_timestamp - lubridate::hours(12)) 



#### Append to VeDBA data ####

full_dat <- read.csv( '/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Validation_full_dat425.csv')
full_dat <- full_dat[ , c( 'ID', "timestamp" ,"local_timestamp" , "eobs_start_timestamp", "acceleration_short", "night", "ave_vedba", "log_vedba", "sleep_bouts" ),] 


# turn timestamp into POSIX element 
full_dat$local_timestamp <- as.POSIXct(full_dat$local_timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )
full_dat$timestamp <- as.POSIXct(full_dat$timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz = 'UTC' )
full_dat$eobs_start_timestamp <- as.POSIXct(full_dat$eobs_start_timestamp, format=c("%Y-%m-%d %H:%M:%OS"), tz = 'UTC' )
full_dat$night <- as.Date(full_dat$night)
full_dat$observation <-NA

#subset to match observation
start_obs <- observation$start_timestamp_corr[1]
end_obs <- max(observation$stop_timestamp_corr) -6.66
full_dat1 <- full_dat[which(full_dat$eobs_start_timestamp >= start_obs & full_dat$eobs_start_timestamp <= end_obs),]


for (i in 1:nrow(full_dat1)){
  start <- max(observation[which(observation$start_timestamp_corr <= full_dat1$eobs_start_timestamp[i] ),]$start_timestamp_corr)
  end <- min(observation[which( observation$stop_timestamp_corr >= full_dat1$eobs_start_timestamp[i]+ 6.66),]$stop_timestamp_corr)
  trim_obs = observation[which(observation$start_timestamp_corr == start | observation$stop_timestamp_corr == end),]
  if (nrow(trim_obs) >1 ){
    #full_dat2$observation[i] <-  "mixed" 
    #full_dat2$observation[i] <- paste(as.character(unique(trim_obs$behavior)), collapse = "_")
    print(unique(trim_obs$behavior))
    if (trim_obs$behavior[1] == "wake" | trim_obs$behavior[2] == "wake"){
      full_dat1$observation[i] <- "wake"
    } 
    if (trim_obs$behavior[1] == "resting wakeful" | trim_obs$behavior[2] == "resting wakeful"){
      full_dat1$observation[i] <- "resting wakeful"
    }
  } else {
    full_dat1$observation[i] <- trim_obs$behavior
  }
    
} 
  
#### Observation 2 ####
df = read.csv("/Users/capuchin_monkey/Documents/Cap_Validation/DATA/BORIS_output/BORIS-2023-10-03-2-cap-agg-events-all-behaviors.csv")
df2 <- df[ , c( 'Observation.id', 'Behavior' ,'Behavior.type', 'Start..s.', 'Stop..s.', 'Duration..s.', 'Image.index.start', 'Image.index.stop', 'Comment.start' ),] 
names(df2) <- c( 'Observation_ID', 'behavior', 'behavior_type', 'start_s', 'stop_s', 'duration_s', 'image_start', 'image_stop', 'comment' )

#new dataframe just for observations & for timestamps 
observation <- df2[which(df2$behavior_type == "STATE"),]
timestamps <- df2[which(df2$behavior_type == "POINT"),]

# subset observation df with only needed behaviors 
observation2 <- observation
observation <- observation[which(observation$behavior == "out of sight" | observation$behavior =="wake" |observation$behavior == "sleep" | observation$behavior =="resting wakeful"),]

### timestamps ###
options( digits.secs = 6 )
#see time at start of video
as.POSIXct(timestamps$comment[1], format = "%H:%M:%OS", tz = "UTC") 
timestamps$start_s[1]
time_start <- fast_strptime("2023-10-04 02:34:10.960", format = "%Y-%m-%d %H:%M:%OS")

#create columns with UTC timestamps
observation$time_start_UTC <- as.POSIXct(ceiling_date(time_start + observation$start_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$time_end_UTC <- as.POSIXct(ceiling_date(time_start +observation$stop_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$dur_crr <- observation$time_end - observation$time_start

# GPS time is used in the collars
timestamp_correction <- 18 #seconds to correct from UTC to GPS

observation$start_timestamp_corr <- observation$time_start_UTC + timestamp_correction
observation$stop_timestamp_corr <- observation$time_end_UTC  + timestamp_correction

#create a night column 
observation$local_timestamp <- lubridate::with_tz(observation$start_timestamp_corr, tzone = "America/Panama")
observation$night <- lubridate::date( observation$local_timestamp - lubridate::hours(12)) 

#subset to match observation
start_obs <- observation$start_timestamp_corr[1]
end_obs <- max(observation$stop_timestamp_corr) -6.66
full_dat2 <- full_dat[which(full_dat$eobs_start_timestamp >= start_obs & full_dat$eobs_start_timestamp <= end_obs),]


for (i in 1:nrow(full_dat2)){
  start <- max(observation[which(observation$start_timestamp_corr <= full_dat2$eobs_start_timestamp[i] ),]$start_timestamp_corr)
  end <- min(observation[which( observation$stop_timestamp_corr >= full_dat2$eobs_start_timestamp[i]+ 6.66),]$stop_timestamp_corr)
  trim_obs = observation[which(observation$start_timestamp_corr == start | observation$stop_timestamp_corr == end),]
  if (nrow(trim_obs) >1 ){
    #full_dat2$observation[i] <-  "mixed" 
    #full_dat2$observation[i] <- paste(as.character(unique(trim_obs$behavior)), collapse = "_")
    print(unique(trim_obs$behavior))
    if (trim_obs$behavior[1] == "wake" | trim_obs$behavior[2] == "wake"){
      full_dat2$observation[i] <- "wake"
    } 
    if (trim_obs$behavior[1] == "resting wakeful" | trim_obs$behavior[2] == "resting wakeful"){
      full_dat2$observation[i] <- "resting wakeful"
    }
  } else {
    full_dat2$observation[i] <- trim_obs$behavior
  }
  
} 


#### Observation 3 ####
df = read.csv("/Users/capuchin_monkey/Documents/Cap_Validation/DATA/BORIS_output/BORIS-2023-10-03-3-cap-agg-events-all-behaviors.csv")
df2 <- df[ , c( 'Observation.id', 'Behavior' ,'Behavior.type', 'Start..s.', 'Stop..s.', 'Duration..s.', 'Image.index.start', 'Image.index.stop', 'Comment.start' ),] 
names(df2) <- c( 'Observation_ID', 'behavior', 'behavior_type', 'start_s', 'stop_s', 'duration_s', 'image_start', 'image_stop', 'comment' )

#new dataframe just for observations & for timestamps 
observation <- df2[which(df2$behavior_type == "STATE"),]
timestamps <- df2[which(df2$behavior_type == "POINT"),]

# subset observation df with only needed behaviors 
observation2 <- observation
observation <- observation[which(observation$behavior == "out of sight" | observation$behavior =="wake" |observation$behavior == "sleep" | observation$behavior =="resting wakeful"),]

### timestamps ###
options( digits.secs = 6 )
#see time at start of video
as.POSIXct(timestamps$comment[1], format = "%H:%M:%OS", tz = "UTC") 
timestamps$start_s[1]
time_start <- fast_strptime("2023-10-04 06:39:25.920", format = "%Y-%m-%d %H:%M:%OS")

#create columns with UTC timestamps
observation$time_start_UTC <- as.POSIXct(ceiling_date(time_start + observation$start_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$time_end_UTC <- as.POSIXct(ceiling_date(time_start +observation$stop_s, unit = ".001s"), format = "%Y-%m-%d %H:%M:%OS3")
observation$dur_crr <- observation$time_end - observation$time_start

# GPS time is used in the collars
timestamp_correction <- 18 #seconds to correct from UTC to GPS

observation$start_timestamp_corr <- observation$time_start_UTC + timestamp_correction
observation$stop_timestamp_corr <- observation$time_end_UTC  + timestamp_correction

#create a night column 
observation$local_timestamp <- lubridate::with_tz(observation$start_timestamp_corr, tzone = "America/Panama")
observation$night <- lubridate::date( observation$local_timestamp - lubridate::hours(12)) 

#subset to match observation
start_obs <- observation$start_timestamp_corr[1]
end_obs <- max(observation$stop_timestamp_corr) -6.66
full_dat3 <- full_dat[which(full_dat$eobs_start_timestamp >= start_obs & full_dat$eobs_start_timestamp <= end_obs),]


for (i in 1:nrow(full_dat3)){
  start <- max(observation[which(observation$start_timestamp_corr <= full_dat3$eobs_start_timestamp[i] ),]$start_timestamp_corr)
  end <- min(observation[which( observation$stop_timestamp_corr >= full_dat3$eobs_start_timestamp[i]+ 6.66),]$stop_timestamp_corr)
  trim_obs = observation[which(observation$start_timestamp_corr == start | observation$stop_timestamp_corr == end),]
  if (nrow(trim_obs) >1 ){
    #full_dat3$observation[i] <-  "mixed" 
    #full_dat3$observation[i] <- paste(as.character(unique(trim_obs$behavior)), collapse = "_")
    print(unique(trim_obs$behavior))
    if (trim_obs$behavior[1] == "wake" | trim_obs$behavior[2] == "wake"){
      full_dat3$observation[i] <- "wake"
    } 
    if (trim_obs$behavior[1] == "resting wakeful" | trim_obs$behavior[2] == "resting wakeful"){
      full_dat3$observation[i] <- "resting wakeful"
    }
  } else {
    full_dat3$observation[i] <- trim_obs$behavior
  }
  
} 


full_dat3

#### NEXT ####
new_full_dat <- rbind(full_dat1, full_dat2, full_dat3)

valid_dat <- new_full_dat[ !is.na( new_full_dat$sleep_bouts ), ]
valid_dat <- valid_dat[which(valid_dat$observation == "wake" | valid_dat$observation == "resting wakeful" | valid_dat$observation == "sleep"),]
unique( valid_dat$observation)
valid_dur <- nrow(valid_dat) 
valid_sleep <- nrow(valid_dat[which(valid_dat$observation == "sleep"),])/valid_dur
valid_wake <- nrow(valid_dat[which(valid_dat$observation == "wake"),])/valid_dur
valid_rw <- nrow(valid_dat[which(valid_dat$observation == "resting wakeful"),])/valid_dur

confusion_matrix <- table( valid_dat$sleep_bouts, valid_dat$observation )

accur <- ( confusion_matrix[ '0', 'wake' ] + confusion_matrix[ '0', 'resting wakeful' ] + confusion_matrix[ '1', 'sleep' ] ) / sum( confusion_matrix )

print( accur )

print(confusion_matrix)

write.csv(valid_dat, "/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Validation_VeDBA_and_observation_data.csv", row.names = F) #saves the vedba 
write.csv(new_full_dat, "/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Validation_VeDBA_and_all_observation_data.csv", row.names = F) #saves the vedba 

# remove accdata df so that they don't take up so much ram
rm( night) #allows to delete things from the environment, that are not needed anymore
gc() #garbage collection to free up ram 


#### GRAVEyard #####
#Carters code:
observation <- observation[ order( observation$start_timestamp_corr), ]

sec_focal_dat <- data.frame( ID = "Pepe", timestamp = seq( floor_date( min( observation$start_timestamp_corr ), unit = "min" ), ceiling_date( max( observation$stop_timestamp_corr ), unit = "min" ), by = '1 secs' ), behavior = NA )

for( i in 1:nrow( observation ) ){
  
  start_ind <- min( which( sec_focal_dat$timestamp > observation$start_timestamp_corr[ i ] ) )
  stop_ind <- max( which( sec_focal_dat$timestamp < ( observation$stop_timestamp_corr[ i ] - 1 ) ) )
  
  if( start_ind < stop_ind ){
    sec_focal_dat$behavior[ start_ind:stop_ind] = observation$behavior[ i ]
  }
}
sec_merge <- merge( x = full_dat1, y = sec_focal_dat, by.x = c( 'ID', 'timestamp' ), by.y = c( 'ID', 'timestamp' ), all.x = T, all.y = F, sort = F )

#Carters code:
observation <- observation[ order( observation$start_timestamp_corr), ]

sec_focal_dat <- data.frame( ID = "Pepe", timestamp = seq( floor_date( min( observation$start_timestamp_corr ), unit = "min" ), ceiling_date( max( observation$stop_timestamp_corr ), unit = "min" ), by = '1 secs' ), behavior = NA )

for( i in 1:nrow( observation ) ){
  
  start_ind <- min( which( sec_focal_dat$timestamp > observation$start_timestamp_corr[ i ] ) )
  stop_ind <- max( which( sec_focal_dat$timestamp < ( observation$stop_timestamp_corr[ i ] - 1 ) ) )
  
  if( start_ind < stop_ind ){
    sec_focal_dat$behavior[ start_ind:stop_ind] = observation$behavior[ i ]
  }
}
sec_merge2 <- merge( x = full_dat2, y = sec_focal_dat, by.x = c( 'ID', 'timestamp' ), by.y = c( 'ID', 'timestamp' ), all.x = T, all.y = F, sort = F )

