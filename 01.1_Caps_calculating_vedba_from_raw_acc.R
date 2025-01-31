#### Calculating VeDBA from raw acc data ####

#### In CAPUCHIN MONKEYS ####
library( stringr )
library( data.table )

## a function that uses a rolling window to calculate the dynamic component of a vector
##win_size is in samples, so a window size of 7 would be 7 acc samples, not units of time. Has to be an odd number
##For FFT data, data appear to be 12hz, so a windowsize of 7 is ~0.5 seconds

species="Cebus capucinus"  # Change species to the species you will be working on assuming multiple species are in the dataframe

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


## read in the complete data. This is the data downloaded from Movebank with "ACC" selected 
complete_data <- read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/DATA/FFT_ACC_all_raw.csv") 
# check some stuff 
class(complete_data) #complete_data <- as.data.frame( complete_data )
head(complete_data)
unique(complete_data$eobs.acceleration.sampling.frequency.per.axis)
unique(complete_data$eobs.acceleration.axes)
unique(complete_data$individual.taxon.canonical.name)
#create one tag column 
complete_data$individual_id=paste(complete_data$individual.local.identifier, complete_data$tag.local.identifier, sep="_")

## this makes the timestamp into local time by adding three hours
complete_data$timestamp <- as.POSIXct(x= complete_data$timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='UTC' ) ## turns the timestamp into a POSIX element
complete_data$local_timestamp=lubridate::with_tz(complete_data$timestamp, tzone = "America/Panama")

##subset data but check for weird browser vs r download differences in column names 
complete_data_trim <- try(complete_data[ , c( 'individual_id', 'local_timestamp' , 'eobs.accelerations.raw', 'eobs.acceleration.sampling.frequency.per.axis', 'individual.taxon.canonical.name' ), ]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.
if(class(complete_data_trim)=="try-error"){
  complete_data_trim <- complete_data[ , c( 'individual_id', 'local_timestamp' , 'eobs.accelerations.raw', 'eobs.acceleration.sampling.frequency.per.axis', 'individual.taxon.canonical.name') ] ## keep only the necessary columns
}


# rename the columns to simpler names
names( complete_data_trim ) <- c( 'tag', 'local_timestamp', 'eobs_accelerations_raw', 'sampling_freq_per_axis', 'species' )
head( complete_data_trim )

# trim the data to only the rows that contain spider monkey data and only those rows that have ACC data
acc_dat <- complete_data_trim[ complete_data_trim$eobs_accelerations_raw != "" & complete_data_trim$species == species, ] 

#checking point
head( acc_dat )
length( unique( acc_dat$eobs_accelerations_raw ))
nrow(acc_dat)
unique(acc_dat$sampling_freq_per_axis)
## each acc burst is unique, as we would expect. This confirms that none of the rows have NAs or otherwise missing data where the ACC burst data should be
sum(is.na(acc_dat$sampling_freq_per_axis)) # count how many NAs are in the column => should be 0
#acc_dat=acc_dat[complete.cases(acc_dat$sampling_freq_per_axis),] #to remove missing information NA in specific columns, if that is the case


# remove complete_data and complete_data_trim so that they don't take up so much ram
rm( complete_data, complete_data_trim ) #allows to delete things from the environment, that are not needed anymore
gc() #garbage collection to free up ram 

#### DATA exploration ####

## What data do we have for data?

plot(as.numeric(as.factor(acc_dat$tag))~acc_dat$local_timestamp,cex=0.3,pch=16,main = "Accelerometry bursts",xlab="",xaxt='n',yaxt='n',ylab="ID")
axis(2,at=1:length(unique(acc_dat$tag)),labels=sort(unique(acc_dat$tag)),las=1,cex=0.3)
axis.POSIXct(1,at=seq(min(acc_dat$local_timestamp),max(acc_dat$local_timestamp),by="1 month"),las=2)

# length of ACC burst  
length( strsplit( acc_dat$eobs_accelerations_raw[ 1 ], " " )[[1]] ) 
## 246 ACC samples collected per minute. This is 82 samples per axis. With a sampling frequency of 12.5 Hz per axis, this makes roughly 6.56 second bursts

#make a column for night during which the burst occurs 
# night is defined as the 24 hour period from noon to noon, with the date of the night being the date on which the night starts (i.e. the date of the first noon of the night)
acc_dat$night <- lubridate::date( acc_dat$local_timestamp - lubridate::hours(12)) 

# save the tag names
tag_names <- as.character( unique( acc_dat$tag ) ) #include animal name and identifier

# make a column for time of day
acc_dat$time <- str_split_fixed(acc_dat$local_timestamp, " ", 2)[,2] #extracting time from the date column

#### calculating VeDBA ####

# split each string of ACC samples so that we can parse them into X, Y, and Z axes. Each row is a burst, split into each axis and each separate measurement per axis
d2 <- as.data.frame( str_split( acc_dat$eobs_accelerations_raw, " ", simplify = T ) )

# make the columns class numeric instead of character strings
for(i in 1:ncol(d2)){
  
  d2[,i] <- as.numeric(as.character((d2[,i])))
  
}

# name the columns based on whether the sample belongs to the x, y, or z axis
names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

# add the timestamp and tag data to this dataframe. We can add it directly from acc_dat because d2 is just a column from acc_dat that has been split into several column (i.e. the rows still match those of acc_dat)
d2$local_timestamp <- acc_dat$local_timestamp
d2$tag <- acc_dat$tag

# show any rows for which we don't have a complete ACC burst. This would occur, for example, if there were times of the day when the sampling frequency lower or burst duration was shorter
d2[!complete.cases(d2),]

# remove any rows for which we don't have a complete ACC burst
inds <- complete.cases(d2)
acc_dat <- acc_dat[ inds ,]
d2 <- d2[ inds ,]

## split the parsed axes into their own dataframes
x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

# calculate the average vedba per burst
# Applys the dy_acc function to all the bursts -> apply a previous function to things in the list 
acc_dat$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean )
# all x, y, z numbers and create a average mean 
# make a column for the logs of the average vedba
acc_dat$log_vedba <- log( acc_dat$ave_vedba ) #log that mean 

# create folder on computer to save vedba data in 
write.csv(acc_dat, "/Users/capuchin_monkey/Documents/Sleep_on_BCI/Cap_sleep/DATA/Caps_full_night_and_day_data.csv", row.names = F) #saves the vedba 
head( acc_dat )

acc_dat$Year <- year(acc_dat$local_timestamp)
for (i in tag_names){
  print (i)
  print(unique(acc_dat[which(acc_dat$tag == i),]$Year))
}

hist(acc_dat$ave_vedba, breaks = 100)
hist(acc_dat$log_vedba, breaks = 100)

