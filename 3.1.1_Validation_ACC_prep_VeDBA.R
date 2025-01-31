#### Validation ACC data prep ####
library( stringr )
library( data.table )

### function to calculate VeDBA 
# uses a rolling window to calculate the dynamic component of a vector
# win_size is in samples, so a window size of 7 would be 7 acc samples, not units of time. It has to be an odd number
# For FFT data, data are 12.5hz, so a windowsize of 7 is ~0.5 seconds

dy_acc <- function(vect, win_size = 7){
  #vect is a vector with the acc data for one axis for one burst
  #win-size can be changed, but should be default 7 for our data, in our case 7 oberservations are ca. 7 min (10 with pads)
  pad_size <- win_size/2 - 0.5 #make sure length out == length in. Adds a number of NAs to each side of vector 
  # buffers the vector, creates an odd number
  
  padded <- unlist( c(rep(NA, pad_size), vect, rep(NA, pad_size)) ) #adds the pad to the input vector
  acc_vec <- rep(NA, length = length( vect ) ) ##creates empy output vector 
  
  ## sliding window
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))] ## subset the window, aka isolates win_size observations at a time from the input vector
    m_ave <- mean( win, na.rm = T ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp ##Store output 
  }
  
  return( unlist( acc_vec) )
}



#### Data input ####
# load ACC data 
accdata = read.csv("/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Sleep_on_BCI_new_ACC.csv")
names(accdata)
unique(accdata$individual.taxon.canonical.name)
species <- "Cebus capucinus"

# subset to only the species data
accdata = accdata[which(accdata$individual.taxon.canonical.name == species),]
unique(accdata$individual.taxon.canonical.name)
unique(accdata$individual.local.identifier)

#transform timestamp
head(accdata)
accdata$timestamp <- as.POSIXct(x= accdata$timestamp, format=c("%Y-%m-%d %H:%M:%OS"), tz='UTC' )
accdata$local_timestamp=lubridate::with_tz(accdata$timestamp, tzone = "America/Panama")

# trim dataset to needed columns 
acc_dat <- accdata[ , c( 'individual.local.identifier', 'tag.local.identifier' , 'timestamp', 'local_timestamp', 'eobs.start.timestamp',
                         'eobs.accelerations.raw', 'eobs.acceleration.sampling.frequency.per.axis') ]
names( acc_dat ) <- c('ID', 'tag', 'timestamp', 'local_timestamp', 'eobs_start_timestamp', 'eobs_accelerations_raw', 'sampling_freq_per_axis' )


#check for duplicates
length( unique( acc_dat$eobs_accelerations_raw ))
nrow( acc_dat )

# remove any rows with missing data (if there is any)
acc_dat=acc_dat[complete.cases(acc_dat$sampling_freq_per_axis),] #to remove missing information NA in specific colums 
sum(is.na(acc_dat$sampling_freq_per_axis)) # to make sure that worked, count how many NAs are in the column => should be 0

#check the dates of data collection
unique(as.Date(acc_dat$local_timestamp))

#### shorten the ACC burst ####

#check the length of the acc burst 
acc_sampling <- length( strsplit( acc_dat$eobs_accelerations_raw[ 1 ], " " )[[1]] ) ## 792 ACC samples collected per minute on all axes.
792/3 ## 264 samples per axis with a sampling frequency of 12.5
264/12.5 ## makes 21.12 s long bursts

## FFT Settings: to validate -> to which the data needs to be downsampled to 
FFT_acc_sampling <- 246 # 246 ACC samples collected per minute on all axes.
246/3 ## 82 samples per axis with a sampling frequency of 12.5
82/12.5 ## makes 6.56 s long bursts


#shorten the ACC burst to match the FFT sampling 
#since the sampling frequency and sampling interval are the same, we can just take the first 246 ACC samples recorded each time
acc_dat2 <- acc_dat
acc_dat2$acceleration_short <- NA

for (i in 1:nrow(acc_dat2)){
  samples <- strsplit( acc_dat$eobs_accelerations_raw[ i ], " " )
  downsample <- samples[[1]][1:FFT_acc_sampling]
  acc_dat2$acceleration_short[i] <- paste(downsample,collapse=" ") 
}

#check that it worked 
length( strsplit( acc_dat2$acceleration_short[ 1 ], " " )[[1]] )

#### Create VeDBA dataframe ####
# make a column for night during which the burst occurs (night is defined as the 24 hour period from noon to noon, with the date of the night being the date on which the night starts (i.e. the date of the first noon of the night) )
acc_dat2$night <- lubridate::date( acc_dat2$local_timestamp - lubridate::hours(12)) 



# split each string of ACC samples so that we can parse them into X, Y, and Z axes. Each row is a burst and will be split into separate measurements per axis
d2 <- as.data.frame( str_split( acc_dat2$acceleration_short, " ", simplify = T ) )

# make the columns class numeric instead of character strings
for(i in 1:ncol(d2)){
  
  d2[,i] <- as.numeric(as.character((d2[,i])))
  
}

# name the columns based on whether the sample belongs to the x, y, or z axis
names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

# add the timestamp and tag data to this df directly from acc_dat (because d2 is just a column from acc_dat that has been split into several columns (i.e. the rows still match those of acc_dat))
d2$local_timestamp <- acc_dat2$local_timestamp
d2$tag <- acc_dat2$tag
d2$ID <- acc_dat2$ID
d2$timestamp <- acc_dat2$timestamp
d2$night <- acc_dat2$night
d2$eobs_start_timestamp <- acc_dat2$eobs_start_timestamp

# show any rows for which we don't have a complete ACC burst. This would occur, for example, if there were times of the day when the sampling frequency lower or burst duration was shorter
d2[!complete.cases(d2),]

# remove any rows for which we don't have a complete ACC burst
inds <- complete.cases(d2)
acc_dat2 <- acc_dat2[ inds ,]
d2 <- d2[ inds ,]

## split the parsed axes into their own dataframes
x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

#### Calculate VeDBA ####

## calculate the average vedba per burst
# Applys the dy_acc function to all the bursts 
acc_dat2$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean )
# all x, y, z numbers and create an average mean 

## calculate log of the average vedba
acc_dat2$log_vedba <- log( acc_dat2$ave_vedba ) 

#### Save dataframes ####

write.csv(acc_dat2, "/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Validation_VeDBA_full_night_and_day_data.csv", row.names = F) #saves the vedba 
write.csv(acc_dat, "/Users/capuchin_monkey/Documents/Cap_Validation/DATA/Validation_acc_dat.csv", row.names = F) #saves the vedba 


## plot VeDBA 

## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00
ts_func <- function( time_vec ){
  
  num_time <- as.numeric( as_hms( time_vec ) )
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  return( corr_time )
}
## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}
library(hms)
library(plotrix)

#subset dataframe to only one night
sleep_one <- acc_dat2
unique(sleep_one$night)
sleep_one = sleep_one[which(sleep_one$night == "2023-10-06" ),]

## plot VeDBA
x_vals <- ts_func( sleep_one$local_timestamp ) # x values for Vedba 
plot(x=x_vals, y = sleep_one$log_vedba, type = 'l', xaxt = 'n', xlab = '', ylab = '', las = 1, lwd = 0.65 )
title( ylab = "log VeDBA", line = 2 )
# add x axis  
axis( 1, at = seq( 0, 60*24*60, 3*60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 3*60*60) ), as_hms( seq( 0, 60*12*60, 3*60*60) ) ) ) 
axis( 1, at = seq( 0, 60*24*60, 1*60*60), labels = F ) 


acc_dat2$local_time <- str_split_fixed(acc_dat2$local_timestamp, " ", 2)[,2] 
## plot average VeDBA of all nights
ave_ved <- aggregate( acc_dat2$log_vedba, by = list( acc_dat2$local_time ), FUN = mean, na.rm = T ) 
names( ave_ved ) <- c( 'local_time', 'ave_VeDBA')

ave_ved <- ave_ved[ order( ts_func( ave_ved$local_time ) ), ]

se_ved <- aggregate( acc_dat2$log_vedba, by = list( acc_dat2$local_time ), FUN = std.error, na.rm = T ) 
names( se_ved ) <- c( 'local_time', 'se_VeDBA')
se_ved <- se_ved[ order( ts_func( se_ved$local_time ) ), ]
se_ved$se_VeDBA[ is.na( se_ved$se_VeDBA) ] <- 0

x_vals <- ts_func( ave_ved$local_time )

plot( x_vals , ave_ved$ave_VeDBA, type = 'l', xaxt = 'n', xlab = 'Time', ylab = '', las = 1, lwd = 0.75  )
axis( 1, at = seq( 0, 60*24*60, 3*60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 3*60*60) ), as_hms( seq( 0, 60*12*60, 3*60*60) ) ) )

low_se <- ave_ved$ave_VeDBA - se_ved$se_VeDBA
high_se <- ave_ved$ave_VeDBA + se_ved$se_VeDBA
polygon( x = c( ( x_vals ) ,rev( x_vals ), x_vals[ 1 ] ), y = c( low_se, rev( high_se ), low_se[ 1 ] ), col=transp( "#2171B5" , 0.4 ), border=NA ) # fills area between the curves



