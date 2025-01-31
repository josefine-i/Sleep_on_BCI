#### Sleep Analysis ####
#### KINKAJOUS ####

library( stringr )
library( lubridate )
# library( plotrix )
library( suncalc )
library( hms )

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}

## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00
ts_func <- function( time_vec ){
  num_time <- as.numeric( as_hms( time_vec ) )
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  return( corr_time )
}

## function to transform GPS data
# this is for Panama: "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
lonlat_to_utm <- function( df, lon_name = 'lon', lat_name = 'lat', crs_utm = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)){
  library( sp )
  library( rgdal )
  crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  df_sp <- SpatialPointsDataFrame(coords = df[, c(lon_name, lat_name)], proj4string = crs_longlat, data = df)
  
  df_sp <- spTransform(df_sp, crs_utm)
  
  df <- as.data.frame(df_sp)
  
  names(df) [ names(df) == paste( lon_name, 1, sep = '.') ] <- 'x'
  names(df) [ names(df) == paste( lat_name, 1, sep = '.') ] <- 'y'
  
  return( df )
}


#### sleep period function

## missing_mins: this is the maximum total number of minutes of data that can be missing from a day and still have that day included in the analysis

## time_gap: this is the maximum allowable time gap between two accelerometer bursts (in seconds) that can exist in a day without removing this day from the data

## percentile: this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity 

## move_window: this is the size of the moving window (in minutes) used in calculating the rolling median 

## block_size: duration in minutes of the blocks of continuous inactivity that will be considered sleep

## title: if title == T, the plot will include the tag number and the day number at the top of the plot

## x_axis: if x_axis == F, the plot will be plotted without an x axis

## las: las sets las in the plotting window

################## Data read in ###################

#read in acc data 
d1 <- read.csv( "/Users/Josie/Documents/Masterarbeit/DATA/Kinkajou_full_night_and_day_data_all.csv" )
unique(d1$tag)
class(d1)

## turn timestamp into POSIX element and time into character
d1$local_timestamp <- as.POSIXct(as.character(d1$local_timestamp), format=c("%Y-%m-%d %H:%M:%S"), tz = 'EST' )

# round local_timestamps to the nearest minute
d1$local_timestamp <- lubridate::floor_date( d1$local_timestamp, unit = 'minute' )

# recreate the time column with the rounded times
d1$time <- str_split_fixed( d1$local_timestamp, ' ', 2 )[ , 2 ]

# create column for date 
d1$day <- date(d1$local_timestamp)

# just confirms that there are no duplicate timestamps even after the floor_date rounding above
sum( duplicated( d1[ , c( 'tag', 'local_timestamp' ) ] ) )

d1$year <- lubridate::year(d1$day)

## delete last and first tagging day 
# via day column, so it doesn't throw out only half of the day 
tag_names <- unique(d1$tag)

for ( i in 1:length(tag_names)){
  Min <- min(d1[which(d1$tag == tag_names[i] ),]$day)
  Max <- max(d1[which(d1$tag == tag_names[i] ),]$day)
  d1 = d1[-which(d1$tag == tag_names[i] & d1$day == Min),]
  d1 = d1[-which(d1$tag == tag_names[i] & d1$day == Max),]
}

#check it worked
sum( duplicated( d1[ , c( 'tag', 'local_timestamp' ) ] ) )

## assign each minute of data to a given day. A day lasts from midnight to midnight 
time_shift <- d1$local_timestamp 

## save the date of the first day of the study 
start_date <- lubridate::date( min( d1$local_timestamp ))

## assign day as number of days from the start of the study, with all data before the first midnight representing day 1
d1$day_num <- as.numeric( as.Date( time_shift, tz = "America/Panama" ) - start_date + 1 )

## show how many kinkajou-days there are
nrow( unique( d1[ , c( 'tag', 'day' ) ] ) )

## check where the day changes from one day to the next to see if it is at midnight
head(d1[ ( diff( c( d1$day_num ) ) == 1),])
head(d1[ ( diff( c( as.Date( d1$day ) ) ) == 1),])
identical( d1[ ( diff( c( d1$day_num ) ) == 1),], d1[ ( diff( c( as.Date( d1$day ) ) ) == 1),] )

## view the dataframe and it's summary
head(d1)
summary(d1)


############## Make a dataframe of sunrise and sunset times ###############
##Read in GPS data to get location data for sunset and sunrise information 
GPSdata = read.csv("/Users/Josie/Documents/Masterarbeit/DATA/FFT_GPS_all_cleaned.csv")
GPSdata = GPSdata[which(GPSdata$individual.taxon.canonical.name == "Potos flavus"),]
unique(GPSdata$individual.local.identifier)

## calculate average location 
ave_lon=mean(GPSdata$location.long, na.rm=TRUE)
ave_lat=mean(GPSdata$location.lat, na.rm=TRUE)

## make an empty dataframe with each date of the study. For each date, we will fill in when sunset occurred, when the dark period began (the end of evening astronomical twilight), when the dark period ended the following morning (the beginning of morning astronomical twilight), and when sunrise the following morning occured
sun_dat <- data.frame( date = c(  seq( from=(min( as.Date( d1$local_timestamp ), na.rm = TRUE ) - 2 ),to = (max( as.Date( d1$local_timestamp ) , na.rm = TRUE) + 1 ), by="1 day"  ), sunset = NA, night_start = NA, night_end = NA, sunrise = NA ))

## fill in rise time and night end time (dark period end time) on each date with those from the following date with the getsunlighttimes function
sun_dat[, c( 'sunrise', 'night_end' ) ] <- getSunlightTimes( date = ( sun_dat$date ), lon = ave_lon, lat = ave_lat )[, c( 'sunrise', 'nightEnd' ) ]

## fill in sunset time and night start time (dark period start time) on each date with the getsunlighttimes function
sun_dat[, c( 'sunset', 'night_start' ) ] <- getSunlightTimes( date = sun_dat$date, lon = ave_lon, lat = ave_lat )[, c( 'sunset', 'night' ) ]

## put sun data in local time
sun_dat[ , 2:5 ] <- lubridate::with_tz(sun_dat[ , 2:5 ], tzone = "America/Panama")
sun_dat=data.frame(na.omit(sun_dat))


############## Make a dataframe of moon phases ###############

## save the unique dates associated with the local timestamps in d1
dates <- seq( from=(min( as.Date( d1$local_timestamp ), na.rm = TRUE ) - 2 ),to = (max( as.Date( d1$local_timestamp ) , na.rm = TRUE) + 1 ), by="1 day"  )

## using the getmoonillumination function, get information about the moon on the study dates
moon_dat <- getMoonIllumination( date = c( min( dates ) - 1, dates, max( dates ) + 1 ) )



####################### Determining sleep ####################################


mins_in_day <- 60*24 ## save a variable denoting the total number of minutes in the day

missing_mins <- 120 ## this is the maximum total number of minutes of data that can be missing from a day and still have that day included in the analysis (for sleep period time and sleep based analyses; i.e. not ave_vedba)

day_missing_mins <- 45 ## this is the maximum total number of minutes of data that can be missing from a light period and still have that day included in the analysis

time_gap <- 20*60 ## this is the maximum allowable time gap between two accelerometer bursts (in seconds) that can exist in a noon-to-noon period without removing this noon-to-noon period from the data

mov_window <- 21 ## this is the size of the moving window (in minutes) used in calculating the rolling median of the average VeDBA ## I had this at 17 due to visual inspection but if we want it to match the validated algorithm, we need to keep it at 9

waso_window <- 1 ## this is the size of the moving window (in minutes) used in calculating the rolling median that will be used to find periods of wake after sleeping. A waso_window of 1 is the same as using the raw data without a rolling median

percentile <- 0.51 ## this is the percentile threshold of the variable (within the noon-to-noon time period) used to classify activity vs. inactivity (VeDBA here)

waso_block <- 3 ## this is the number of consecutive minutes of inactivity needed to classify a period as sleep within the sleep period. A waso_block of 1 means that anytime the value is below the threshold, the kinkajou in considered sleeping and anytime the value is above the threshold the kinkajou is considered awake

frag_block <- 2 ## this is the number of minutes of waking that need to be consecutive to be considered a wake bout during the day (other epochs of wake that do not meet this criterion will still be considered wake for WASO and wake_bouts, but not frag_wake_bouts)

day_length <- 14*60  ## varaible for the minutes between 05.00 and 19.00

## shows the time (as well as one previous time and one later time) where a minute is skipped
sort( unique( d1$time ) ) [ which( diff( as_hms( sort( unique( d1$time ) ) ) ) != as_hms( '00:01:00' ) ) + -1:1 ]

## again confirms that every minute is represented in the data except for one (can tell this by comparing this number to the minutes_in_day variable above)
length( unique(d1$time) )

## create a vector containing the names of each kinkajou
tag_names <- unique( d1$tag )


## make a copy of d1. We will fill in this new dataframe with information about if the kinkajou was asleep in each epoch
full_dat <- d1
d1$dark = NA
full_dat$sleep_bouts <- NA ## binary indicating whether the row is considered sleep, based on the waso or nap requirements
full_dat$n_bursts <- NA ## the number of bursts collected in a given midnight-to-midnight period (to be compared to the total number of minutes in the day). This column will indicate whether the data for a given day is insufficient to calculate the sleep period 
full_dat$max_time_diff <- NA ## the maximum difference between consecutive fixes in a given midnight-to-midnight period. With the previous column, this column will indicate whether the data is insufficient to calculate the sleep period 
full_dat$dark <- NA ## a binary indicating whether a row belongs to the period of darkness (between the end of astrological evening twilight and the beginning of astrological morning twilight)
full_dat$poss_dark_bursts <- NA ## the number of potential bursts in the dark period on this night, i.e. the number of minutes between the start and end of the night
full_dat$n_dark_bursts <- NA ## the total number of bursts actually taken during the dark period on this night. This will be compared to the previous column to determine whether the data during the dark period is sufficient to calculate the dark_TST and ave_vedba
full_dat$max_dark_time_diff <- NA ## the maximum difference between consecutive fixes in a given dark period. With the previous column, this column will indicate whether the data is insufficient to calculate the dark_TST and ave_vedba
full_dat$poss_day_bursts <- NA ## the number of potential bursts in day light period, i.e. the number of minutes between sunrise and sunset
full_dat$n_day_bursts <- NA ## the total number of bursts actually taken during the day light period. This will be compared to the previous column to determine whether the data during the day period is sufficient 
full_dat$max_day_time_diff <- NA ## the maximum difference between consecutive fixes in a given day (light) period. With the previous column, this column will indicate whether the data is insufficient to calculate the prev_day_sleep and prev_day_ave_vedba
full_dat$roll_log_vedba <- NA ## the rolling median of log VeDBA that is used for sleep bout identification, so with the waso_window



### Threshold ###
thresh=c()
for( i in 1:length(tag_names) ){
  ## subset the data to just this individual's data
  id_dat <- d1[ which(d1$tag == tag_names[i]), ]
  
  ## create a vector the days for which this individual has data
  dayss <- unique( id_dat$day )
  med_vedba=c()
  for( j in 1:length(dayss) ){
    night_dat <- id_dat[ which(id_dat$day == dayss[j]), ]
    
    med_vedba[j] <- list(zoo::rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' ))
    
  }
  med_vedba=unlist(med_vedba)
  thresh[i]=list(quantile( med_vedba, percentile, na.rm = T))#* multiplier
}

### Getting sleep bouts ###

## for each individual...
for( i in 1:length(tag_names) ){
  
  ## subset the data to just this individual's data
  id_dat <- d1[ which(d1$tag == tag_names[i]), ]
  
  ## create a vector the days for which this individual has data
  dayss <- unique( id_dat$day )
  
  ## for each day on which this individual has data
  for( j in 1:length(dayss) ){
    
    ## subset this individual's data to just that day
    night_dat <- id_dat[ which(id_dat$day == dayss[j]), ]
    
    ## create empty columns for the sleep period and sleep bout binary variables
    night_dat$sleep_bouts <- NA
    
    ## save a column of the total number of bursts for that day. This will also make it easier to remove these days from the dataframe later
    night_dat$n_bursts <- nrow( night_dat )
    
    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( '00:00:00', sort( night_dat$time ), '23:59:00' )
    
    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( as_hms( sorted_times ) ), units = 'secs' )
    
    if( length( time_diffs ) > 0 ){ ### If there is one night for one kinkajou with only one single burst, this if statement is needed

      ## save a column of the maximum time difference between burst for that day (this will make it easier to pull out days with insufficient data later)
      night_dat$max_time_diff <- max( unique(as.numeric(time_diffs )))

    }else{

      night_dat$max_time_diff <- NA
    }
    
    ## take the rolling median of the log VeDBA
    night_dat$roll_log_vedba <- zoo::rollmedian( night_dat$log_vedba, waso_window, fill = NA, align = 'center' )
    
    ### find blocks of continuous inactivity
    
    ## find the run length encoding of periods above and below the threshold
    temp <- rle( as.numeric( night_dat$roll_log_vedba < thresh[[i]] ) )
    
    ## mark which rows are part of runs of activity vs. inactivity, as determined by waso_block
    runs <- as.numeric( rep( temp$lengths >= waso_block, times = temp$lengths ) )
    
    sleep_bouts <- as.numeric( night_dat$roll_log_vedba < thresh[[i]] & runs == 1 )
    
    ## make which rows are part of runs of inactivity. These are the periods of sleep within the sleep period
    night_dat$sleep_bouts <- sleep_bouts
    
    print( max( sleep_bouts ) )
    print( max( night_dat$sleep_bouts ) )
    
    ## take the rolling median of the log VeDBA and save it as a column
    roll_log_vedba <- zoo::rollmedian( night_dat$log_vedba, mov_window, fill = NA, align = 'center' )
    ## determine the threshold activity vs. inactivity threshold based on the percentile, and the rolling median just produced
    
    if (nrow(night_dat) > 10 ){ # some kinkajous have not a lot of data per day towards the end of the tagging period, which is why this statement is needed
      plot(x=night_dat$local_timestamp, roll_log_vedba)
      abline(h=thresh[[i]])
    } else { #collectinng these individuals and days in a df 
      print(paste(unique(night_dat$day), unique(night_dat$tag)))
    }
    
    
    dark_start <- sun_dat$night_start[ which(sun_dat$date == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) ]
    
    dark_end <- sun_dat$night_end[ which(sun_dat$date == (as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) + 1)) ]
    
    night_dat$dark <- as.numeric( night_dat$local_timestamp >= dark_start & night_dat$local_timestamp <= dark_end )
    
    night_dat$poss_dark_bursts <- floor( as.numeric( dark_end - dark_start, units = 'mins' ) )
    
    dark_dat <- id_dat[ which(id_dat$local_timestamp > dark_start & id_dat$local_timestamp < dark_end), ]
    
    night_dat$n_dark_bursts <- nrow(dark_dat)
    
    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( dark_start, sort( dark_dat[ which(dark_dat$dark == 1), 'local_timestamp'] ), dark_end )
    
    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( sorted_times ), units = 'secs' )
    
    if( length( time_diffs ) != 0 ){
      ## save the maximum time difference during the night
      night_dat$max_dark_time_diff <- max( time_diffs )  ## so if this variable is NA, that means that there are no bursts taken from the nighttime period on this day for this individual
    }else{
      
      night_dat$max_dark_time_diff <- NA
    }
    
    sunrise <- sun_dat$sunrise[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )  )) ]
    
    sunset <- sun_dat$sunset[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )  )) ]
    
    night_dat$poss_day_bursts <- floor( as.numeric( sunset - sunrise, units = 'mins' ) )
    
    day_dat <- id_dat[ which(id_dat$local_timestamp > sunrise & id_dat$local_timestamp < sunset), ]
    
    night_dat$n_day_bursts <- nrow( day_dat )
    
    ## sort the timestamps (they are probably already sorted)
    sorted_times <- c( sunrise, sort( day_dat$local_timestamp ), sunset )
    
    ## find the time difference in seconds between each burst
    time_diffs <- as.numeric( diff( sorted_times ), units = 'secs' )
    
    if( length( time_diffs ) != 0 ){
      ## save the maximum time difference during the day
      night_dat$max_day_time_diff <- max( time_diffs ) 
    }else{
      
      night_dat$max_day_time_diff <- NA
    }
    
    # night_dat <- night_dat[ ,  names( night_dat ) != 'roll_log_vedba' ]
    
    night_dat <- night_dat[ , names( full_dat ) ]
    
    ### put the night data (which is technically day here) back into full_dat
    full_dat[ which(full_dat$tag == tag_names[i] & full_dat$day == dayss[j]), ] <- night_dat
    
    # if( max( full_dat$sleep_bouts, na.rm = T ) > 1 ) stop( 'asdf' )
    
  }
  
}


# create the date for the "TST of 24 hours" measurement 
full_dat$eight_to_eight <- date(full_dat$local_timestamp + lubridate::hours(4))

## check where the night changes from one night to the next to see if it is at 8 am
head(full_dat[ ( diff( c( as.Date( full_dat$eight_to_eight ) ) ) == 1),])

# copy the df before cleaning
pre_clean_full <- full_dat

d1$day = as.Date(d1$day)

study_days <- as.Date( min( d1$day):max( d1$day ), origin = "1970-01-01", tz = "America/Panama" )

sleep_per <- data.frame( tag = rep( unique( d1$tag ), each = length( study_days ) ), 
                         day = rep( study_days, times = length( unique( d1$tag ))), 
                         sunset = NA, 
                         sunrise = NA, 
                         day_dur = NA, #length of the night between sunrise and sunset
                         dark_start = NA, 
                         dark_end = NA, 
                         light_period_dur = NA, # length of day between start of astronomical twilight in the morning till end of astronomical twilight in the evening 
                         TST_whole_day = NA, ## TST of a 24 hour period from 8 pm to 8 pm 
                         TST_sleep_per = NA, ## TST between 5 am and 7 pm 
                         WASO = NA, ## time spend awake during the period of the dominant sleep period
                         prev_active_per_sleep = NA, ## time slept during dominant waking period (here: 7pm - 5am) before the day 
                         follow_active_per_sleep = NA, ## time slept during dominant waking period (here: 7pm - 5am) in the following night
                         sleep_eff = NA, ## proportion of the time spend asleep during the dominant sleeping period 
                         wake_bouts = NA, ## number of awakenings during the dominant sleep period
                         frag_wake_bouts = NA, ## number of awakenings during the dominant sleep period that were longer than 2 minutes 
                         summed_VeDBA = NA, ## sum of log VeDBA during dominant sleeping period
                         day_VeDBA_corr = NA, 
                         ave_vedba = NA, ## mean VeDBA during the day period
                         prev_active_per_ave_vedba = NA, poss_dark_bursts = NA, n_dark_bursts = NA, poss_day_bursts = NA, n_day_bursts = NA, max_time_diff = NA, n_bursts= NA, max_dark_time_diff = NA, max_day_time_diff = NA)



## create empty vectors for the durations of sleep and wake bouts. We will fill these in to see if the distributions of the durations of these bouts later
sleep_durs <- c()
wake_durs <- c()

## night start and end time 
night_end_time <- "05:00:00"
night_start_time <- "19:00:00"

#### TST calculation ####
## for each individual...
for( i in 1:length(tag_names) ){
  print(tag_names[i])
  
  ## subset the data to just this individual's data
  id_dat <- full_dat[ full_dat$tag == tag_names[i], ]
  
  ## create a vector the days for which this individual has data
  dayss <- unique( id_dat$day )
  
  ## for each day on which this individual has data
  pb = txtProgressBar(min = 0, max = length(dayss),  style = 3, char="=") 
  for( j in 1:length(dayss) ){
    
    ## subset this individual's data to just that day
    night_dat <- id_dat[ id_dat$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ), ]
    
    ## should already be in order, but just in case
    night_dat <- night_dat[ order( night_dat$local_timestamp ), ]
    
    ## copy the values needed fot cleaning later 
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_bursts <- unique( night_dat$n_bursts, na.rm = TRUE )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_time_diff <- max(unique( night_dat$max_time_diff , na.rm = TRUE), na.rm = TRUE)
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$poss_dark_bursts <- unique( night_dat$poss_dark_bursts, na.rm = TRUE )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_dark_bursts <- unique( night_dat$n_dark_bursts, na.rm = TRUE )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_dark_time_diff <- max(unique( night_dat$max_dark_time_diff, na.rm = TRUE ), na.rm = TRUE)
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$poss_day_bursts <- unique( night_dat$poss_day_bursts, na.rm = TRUE )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$n_day_bursts <- unique( night_dat$n_day_bursts, na.rm = TRUE )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )) , ]$max_day_time_diff <- unique( night_dat$max_day_time_diff, na.rm = TRUE )
    
    ## sunset and sunrise 
    sunset <- sun_dat$sunset[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) )) ]
    
    sunrise <- sun_dat$sunrise[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) )) ]
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$sunset <- sunset
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$sunrise <- sunrise
    
    day_dur <- as.numeric( sunset - sunrise, units = 'mins' )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$day_dur <- day_dur
    
    ## darkness period 
    
    dark_start <- sun_dat$night_start[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) )) ]
    
    dark_end <- sun_dat$night_end[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) )) ]
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$dark_start <- dark_start
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$dark_end <- dark_end
    
    light_period_dur <- as.numeric( dark_start - dark_end, units = 'mins' )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$light_period_dur <- light_period_dur
    
    if( nrow( night_dat[ which(night_dat$sleep_bouts == 1), ] ) > 0 ){
      
      ## TST_sleep_per = TST 5.00 to 19.00
      day_end <- as.POSIXct( paste( str_split_fixed( sunset, " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
      
      day_start <- as.POSIXct( paste( str_split_fixed( sunrise, " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
      
      SPT_dat <- night_dat[which(night_dat$local_timestamp >= day_start & night_dat$local_timestamp < day_end),]
      
      TST_sleep_per <- sum( SPT_dat$sleep_bouts == 1 )
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$TST_sleep_per <- TST_sleep_per
      
      ## WASO = time spend awake during dominant sleep period window
      WASO <- sum( SPT_dat$sleep_bouts == 0 ) 
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$WASO <- WASO
      
      ## sleep_eff = proportion of the time spend asleep during the dominant sleeping period 
      sleep_eff <- TST_sleep_per/day_length
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$sleep_eff <- sleep_eff
      
      ## summed_VeDBA = sum of log VeDBA during dominant sleeping period
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$summed_VeDBA <- sum( SPT_dat$log_vedba, na.rm = TRUE)
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$day_VeDBA_corr <- sum( SPT_dat$log_vedba, na.rm = TRUE ) / day_length
      
      ## frag_wake_bouts = number of awakenings during the dominant sleep period that were longer than 2 minutes
      temp <- rle( SPT_dat$sleep_bouts )
      
      runs <- as.numeric( rep( temp$lengths >= frag_block, times = temp$lengths ) )
      
      frag_wake_bouts <- as.numeric( SPT_dat$sleep_bouts == 0 & runs == 1 )
      
      diffs <- diff( c( 1, frag_wake_bouts ) )
      
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$frag_wake_bouts <- sum( diffs == 1, na.rm = TRUE )
      
      ## wake_bouts = number of awakenings during the dominant sleep period
      ## find the distinct sleep bouts (i.e. epochs of sleep separated by waking)
      diffs <- diff( c( 0, SPT_dat$sleep_bouts ) )
      
      ## save the number of distinct wake bouts
      sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$wake_bouts <- sum( diffs == -1 , na.rm = TRUE)
      
      ## find durations of sleep and wake bouts
      temp <- rle( SPT_dat$sleep_bouts )
      
      ## add the duration of sleep bouts to the sleep bout duration vector
      sleep_durs <- c( sleep_durs, temp$lengths[ which(temp$values == 1) ] )
      ## add the duration of wake bouts to the wake bout duration vector
      wake_durs <- c( wake_durs, temp$lengths[ which(temp$values == 0 )] )
      
    }
    
    ## ave_vedba = mean VeDBA during the day: sunrise - sunset
    light_dat <- night_dat[ which(night_dat$local_timestamp > sunrise & night_dat$local_timestamp < sunset), ]
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$ave_vedba <- mean( na.omit(light_dat$log_vedba))
    
    ## prev_active_per_sleep = TST during the night between 19.00 and 07.00 the following day
    date_start <- sun_dat$sunset[ which(sun_dat$date == ( as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) -1 )) ]
    
    night_start <- as.POSIXct( paste( str_split_fixed( date_start, " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
    
    date_end <- sun_dat$sunrise[ which(sun_dat$date == (as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) ))]
    
    night_end <- as.POSIXct( paste( str_split_fixed( date_end, " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
    
    day_dat <- id_dat[ which(id_dat$local_timestamp > night_start & id_dat$local_timestamp < night_end), ]
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_active_per_sleep <- sum( day_dat$sleep_bouts , na.rm = TRUE)
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$prev_active_per_ave_vedba <- mean( day_dat$log_vedba , na.rm = TRUE)
    
    ## follow_active_per_sleep = time slept during dominant waking period (here: 7 am - 5.30 pm) in the following day
    start_date <- sun_dat$sunset[ which(sun_dat$date == (as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) )) ]
   
    follow_night_start <- as.POSIXct( paste( str_split_fixed( start_date, " ", 2 )[ , 1 ], night_start_time ), tz = 'America/Panama' )
    
    follow_date <- sun_dat$sunrise[ which(sun_dat$date == (as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ) + 1 )) ]
   
    follow_night_end <- as.POSIXct( paste( str_split_fixed( follow_date, " ", 2 )[ , 1 ], night_end_time ), tz = 'America/Panama' )
    
    follow_day_dat <- id_dat[ which(id_dat$local_timestamp > follow_night_start & id_dat$local_timestamp < follow_night_end), ]
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$follow_active_per_sleep <- sum( follow_day_dat$sleep_bouts , na.rm = TRUE)
    
    ## TST_whole_day = TST of a 24 hour period from 8 am to 8 am 
    ## subset this individual's data to just that 24 h period
    twenty_four <- id_dat[ id_dat$eight_to_eight == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" ), ]
    
    TST_whole_day <- sum( twenty_four$sleep_bouts == 1 )
    
    sleep_per[ which(sleep_per$tag == tag_names[i] & sleep_per$day == as.Date( dayss[j], origin = "1970-01-01", tz = "America/Panama" )), ]$TST_whole_day <- TST_whole_day
    
    setTxtProgressBar(pb,j)
  }
  
  close(pb)
}

sleep_per

#### Prepare and clean dataframe ####

# make a column for the number of nights since the beginning of the study, just for easy notation and subsetting later
sleep_per$day_num <- as.numeric( sleep_per$day - min( sleep_per$day ) + 1 )

## reformat timestamps
sleep_per$sunset <- as.POSIXct( sleep_per$sunset, origin = "1970-01-01 00:00:00", tz = "America/Panama" )
sleep_per$sunrise <- as.POSIXct( sleep_per$sunrise, origin = "1970-01-01 00:00:00", tz = "America/Panama" )
sleep_per$dark_start <- as.POSIXct( sleep_per$dark_start, origin = "1970-01-01 00:00:00", tz = "America/Panama" )
sleep_per$dark_end <- as.POSIXct( sleep_per$dark_end, origin = "1970-01-01 00:00:00", tz = "America/Panama" )

## subset to nights with data 
checkdates=data.frame(unique(cbind(d1$tag, as.character(d1$day))))
colnames(checkdates)=c("tag","day")
checkdates$day <- as.POSIXct( checkdates$day, origin = "1970-01-01", tz = "America/Panama" )

sleep_per_save <- sleep_per[which(paste(sleep_per$tag,sleep_per$day)%in%paste(checkdates$tag,checkdates$day)==TRUE),]
sleep_per <- sleep_per[which(paste(sleep_per$tag,sleep_per$day)%in%paste(checkdates$tag,checkdates$day)==TRUE),]

### Add in moon phase data 

sleep_per <- merge( x = sleep_per, y = moon_dat, by.x = 'day', by.y = 'date', all.x = T, all.y = F, sort = F )

### check number of nights for which sleep period was calculated and inspect those for which no sleep period was calculated ###

sum( !is.na( sleep_per$TST_whole_day ) )
sum( !is.na( sleep_per$TST_sleep_per ) )
sleep_per_nona <- sleep_per[ !is.na( sleep_per$TST_sleep_per ), ]

#### Cleaning the dataframes of data on nights with insufficient data 

## for days on the later side of a noon-to-noon period with a lot of missing data, we can't have a reliable threshold for what was considered sleep in the morning, and we may be missing a lot of epochs. 
## Therefore, remove: sleep time during the day (day defined as prev sunrise to sunset), sleep time during the night (defined as 1900 to 0500)

sleep_per[ paste( sleep_per$tag, sleep_per$day, sep = '_' ) %in% 
             paste( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), 'tag' ], ( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ) , 'day' ] - 1 ), sep = '_' ), 
           c('prev_active_per_sleep') ] <- NA

## for days on the later side of a noon-to-noon period with a lot of missing data, we can't have a reliable threshold for what was considered sleep in the afternoon, and we may be missing a lot of epochs. 
## Therefore, remove: sleep time during the following day 
sleep_per[ paste( sleep_per$tag, sleep_per$day, sep = '_' ) %in% 
             paste( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), 'tag' ], ( sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ) , 'day' ] + 1), sep = '_' ), 
           c( 'follow_active_per_sleep' ) ] <- NA

## remove all these variable from the night, and from the days on the early side of the noon-to-noon period if the noon-to-noon period is missing a lot of data (because then we might not be able to reliably calculate the sleep VeDBA threshold, and a lot of epochs might be missing, which would skew TST and such)
sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), 
           c( 'TST_whole_day', 'TST_sleep_per', 'WASO', 'prev_active_per_sleep', 'summed_VeDBA', 'day_VeDBA_corr', 'follow_active_per_sleep', 'sleep_eff', 'wake_bouts', 'frag_wake_bouts') ] <- NA


sleep_per_nona <- sleep_per[ !is.na( sleep_per$TST_sleep_per ), ]

nrow( sleep_per_nona )


## remove all these variable from the night, and from the days on the early side of the noon-to-noon period if the noon-to-noon period is missing a lot of data (because then we might not be able to reliably calculate the sleep VeDBA threshold, and a lot of epochs might be missing, which would skew TST and such)
sleep_per[ sleep_per$n_bursts < ( mins_in_day - missing_mins ) & !is.na( sleep_per$n_bursts ), 
           c( 'TST_whole_day', 'TST_sleep_per', 'WASO', 'prev_active_per_sleep', 'summed_VeDBA', 'day_VeDBA_corr', 'follow_active_per_sleep', 'sleep_eff', 'wake_bouts', 'frag_wake_bouts') ] <- NA

## remove all these variable from the night, and from the days on the early side of the noon-to-noon period if the noon-to-noon period has large gaps of missing data
sleep_per_nona[ sleep_per_nona$max_time_diff > time_gap & !is.na( sleep_per_nona$max_time_diff ), c( 'TST_whole_day', 'TST_sleep_per')] <- NA

sleep_per_nona <- sleep_per_nona[ !is.na( sleep_per_nona$TST_sleep_per ), ]
sleep_per_nona <- sleep_per_nona[ !is.na( sleep_per_nona$TST_whole_day ), ]

nrow( sleep_per_nona )

## remove data for sleep period and sleep bouts on days when there is a lot of missing data, because we cannot reliably calculate the sleep VeDBA threshold and there may be a lot of missing epochs
full_dat[ which(full_dat$n_bursts < ( mins_in_day - missing_mins )), c( 'sleep_bouts' ) ] <- NA

### Add in some data 

sleep_per_nona$ID <- str_split_fixed(sleep_per_nona$tag, '_', 2 )[ , 1]
sleep_per_nona$data_col <- NA
for (i in 1:length(tag_names)){
  sleep_per_nona[which(sleep_per_nona$tag == tag_names[i]),]$data_col <- unique(d1[which(d1$tag == tag_names[i]),]$data_col)
}


### save the created dataframes as csv
write.csv( sleep_per, '/Users/Josie/Documents/Masterarbeit/Output/Kinkajou_sleep_per51.csv', row.names = F )
write.csv( full_dat, '/Users/Josie/Documents/Masterarbeit/Output/Kinkajou_full_dat51.csv', row.names = F )
write.csv( sleep_per_nona, '/Users/Josie/Documents/Masterarbeit/Output/Kinkajou_sleep_per_nona505.csv', row.names = F )



#### TST comparison ####

sleep_per_nona$TST_whole_day - sleep_per_nona$TST_sleep_per
sleep_per_nona$prev_active_per_sleep

ave_sunrise <- as_hms( mean( as.numeric( as_hms( sleep_per_nona$sunrise ) ) ) )
ave_sunset <- as_hms( mean( as.numeric( as_hms( sleep_per_nona$sunset ) ) ) )

