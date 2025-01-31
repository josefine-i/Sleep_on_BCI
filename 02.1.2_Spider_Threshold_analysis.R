####### Sleep evaluation ########

library(plotrix)



### prep dataframe with only subset of individuals 
unique(sleep_per_nona$tag)
sleep_sub_indiv = sleep_per_nona[which(sleep_per_nona$tag == "Mimi_4660" | sleep_per_nona$tag == "Norah_4655" | sleep_per_nona$tag == "Bob_4661"),]
unique(sleep_sub_indiv$tag)
sleep_per_nona <- read.csv( "/Users/capuchin_monkey/Documents/Caps_sleep/DATA/sleep_per_nona425_allindiv.csv" )


min(str_split_fixed( sleep_per_nona$sunset, ' ', 2 )[ , 2 ])
max(str_split_fixed( sleep_per_nona$sunrise, ' ', 2 )[ , 2 ])

############# THRESHOLD ANALYSIS ############
### SPT ###

#mean
mean(sleep_per_nona$TST_night)
#median
median(sleep_per_nona$TST_night)
#std 
std.error(sleep_per_nona$TST_night)
#sd
sd(sleep_per_nona$TST_night)
#histogram to see distribution
hist(sleep_per_nona$TST_night, breaks = 100) 


### TST ###

#mean
mean(sleep_per_nona$TST_sunset_twelve)
#median
median(sleep_per_nona$TST_sunset_twelve)
#std 
std.error(sleep_per_nona$TST_sunset_twelve)
#sd
sd(sleep_per_nona$TST_sunset_twelve)
#histogram to see distribution
hist(sleep_per_nona$TST_sunset_twelve, breaks = 100) 


### wake bouts ###

#mean
mean(sleep_per_nona$TST_whole_day)
#median
median(sleep_per_nona$TST_whole_day)
#std 
std.error(sleep_per_nona$TST_whole_day)
#sd
sd(sleep_per_nona$TST_whole_day)
#histogram to see distribution
hist(sleep_per_nona$TST_whole_day, breaks = 100) 

################################################
##### Evaluation for individuals#####

unique(sleep_per_nona$tag)

### SPT ###
#mean
mean(sleep_per_nona$SPT[which(sleep_per_nona$tag == "Valoy_5766")])
#median
median(sleep_per_nona$SPT[which(sleep_per_nona$tag == "Valoy_5766")])
#std 
std.error(sleep_per_nona$SPT[which(sleep_per_nona$tag == "Valoy_5766")])
#histogram to see distribution
hist(sleep_per_nona$SPT[which(sleep_per_nona$tag == "Valoy_5766")], breaks = 100) 

### TST ###
#mean
mean(sleep_per_nona$TST[which(sleep_per_nona$tag == "Valoy_5766")])
#median
median(sleep_per_nona$TST[which(sleep_per_nona$tag == "Valoy_5766")])
#std 
std.error(sleep_per_nona$TST[which(sleep_per_nona$tag == "Valoy_5766")])
#histogram to see distribution
hist(sleep_per_nona$TST[which(sleep_per_nona$tag == "Valoy_5766")], breaks = 100) 

### wake bouts ###
#mean
mean(sleep_per_nona$wake_bouts[which(sleep_per_nona$tag == "Valoy_5766")])
#median
median(sleep_per_nona$wake_bouts[which(sleep_per_nona$tag == "Valoy_5766")])
#std 
std.error(sleep_per_nona$wake_bouts[which(sleep_per_nona$tag == "Valoy_5766")])
#histogram to see distribution
hist(sleep_per_nona$wake_bouts[which(sleep_per_nona$tag == "Valoy_5766")], breaks = 100) 


#######################################
##### EXPLORATIVE PLOTS #####

library(ggplot2)
library(ggpubr)
library(lubridate)
#### plot SPT vs. nights #### 
# for the first study period 
sleep_test <- sleep_per_nona
sleep_test$year <- lubridate::year(sleep_test$night)
sleep_test = sleep_test[which(sleep_test$year <= 2016),]

x_range <- c( min( as.numeric(sleep_test$night)), max(as.numeric(sleep_test$night )) )

ggplot(sleep_test, aes(x = as.numeric(night), y=SPT)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2015")
ggplot(sleep_test, aes(x = as.numeric(night), y=TST)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2015")
ggplot(sleep_test, aes(x = as.numeric(night), y=wake_bouts)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2015")

# for the first study period 
sleep_test <- sleep_per_nona
sleep_test$year <- lubridate::year(sleep_test$night)
sleep_test = sleep_test[which(sleep_test$year >= 2017),]

x_range <- c( min( as.numeric(sleep_test$night)), max(as.numeric(sleep_test$night )) )

ggplot(sleep_test, aes(x = as.numeric(night), y=SPT)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2017")
ggplot(sleep_test, aes(x = as.numeric(night), y=TST)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2017")
ggplot(sleep_test, aes(x = as.numeric(night), y=wake_bouts)) + geom_point()+theme_classic() + xlim(x_range) + labs(title = "2017")

###########################################
######### plot sleep of a night ########

sleep_trim <- sleep_per[ !is.na( sleep_per$SPT ), ]
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

### Fig 1 ###
library(hms)

sun_trim <- sun_dat[ sun_dat$date %in% unique( full_dat$night ), ]

ave_sunset <- as_hms( mean( as.numeric( as_hms( sun_trim$sunset ) ) ) )
ave_sunset <- ts_func( ave_sunset )

ave_sunrise <- as_hms( mean( as.numeric( as_hms( sun_trim$sunrise ) ) ) )
ave_sunrise <- ts_func( ave_sunrise )

ave_night_start <- as_hms( mean( as.numeric( as_hms( sun_trim$night_start ) ) ) )
ave_night_start <- ts_func( ave_night_start )

ave_night_end <- as_hms( mean( as.numeric( as_hms( sun_trim$night_end ) ) ) )
ave_night_end <- ts_func( ave_night_end )

par(mar=c(6,6,4,4))
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(2,2,2.5), respect = FALSE)


### Fig 1A
#one night one individual their sleep & wake bouts

#create subset dataframe of one night and one individual 
full_dat <- fread( "/Users/capuchin_monkey/Documents/Caps_sleep/DATA/full_dat425_all.csv" )

sleep_one <- full_dat
sleep_one = sleep_one[which(sleep_one$night == "2018-01-19" & sleep_one$tag == "Da Vinci_5764"),]


par(mar = c(0, 5, 0.5, 1 ) )

## plot VeDBA
x_vals <- ts_func( sleep_one$local_time )


plot(x=x_vals, y = sleep_one$log_vedba, type = 'l', xaxt = 'n', xlab = 'Time', ylab = '', las = 1, lwd = 0.5 )
title( ylab = "log VeDBA", line = 2.5 )

## add night shading 
polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('darkgrey', .25), border = NA )


## plot onset and waking 
one_onset <- ts_func(sleep_per_nona[which(sleep_per_nona$night == unique( sleep_one$night) & sleep_per_nona$tag == unique( sleep_one$tag)),]$onset)
one_waking <- ts_func(sleep_per_nona[which(sleep_per_nona$night == unique( sleep_one$night) & sleep_per_nona$tag == unique( sleep_one$tag)),]$waking)

abline( v = c( one_onset, one_waking ), col = '#08519C', lty = 3, lwd = 2 )

# plot sleep bouts blue
sleep_one$colors = NA
sleep_one$colors[which(sleep_one$sleep_bouts == 0)] = "white"
sleep_one$colors[which(sleep_one$sleep_bouts == 1)] = "#4292C6"
    
for (i in 1:nrow(sleep_one)){
    abline (v = x_vals[i], col = scales::alpha(sleep_one$colors[i], 0.1))
}
  


### Fig 1B
# average VeDBA of all and night shading 

par(mar = c(0, 5, 0, 1))

ave_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = mean, na.rm = T ) 

names( ave_ved ) <- c( 'local_time', 'ave_VeDBA')

ave_ved <- ave_ved[ order( ts_func( ave_ved$local_time ) ), ]


se_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = std.error, na.rm = T ) 
names( se_ved ) <- c( 'local_time', 'se_VeDBA')

se_ved <- se_ved[ order( ts_func( se_ved$local_time ) ), ]

se_ved$se_VeDBA[ is.na( se_ved$se_VeDBA) ] <- 0

x_vals <- ts_func( ave_ved$local_time )

plot( x_vals , ave_ved$ave_VeDBA, type = 'l', xaxt = 'n', xlab = 'Time', ylab = '', las = 1, lwd = 0.5  )

title( ylab = "log VeDBA", line = 2.5 )


low_se <- ave_ved$ave_VeDBA - se_ved$se_VeDBA
high_se <- ave_ved$ave_VeDBA + se_ved$se_VeDBA


polygon( x = c( ( x_vals ) ,rev( x_vals ), x_vals[ 1 ] ), y = c( low_se, rev( high_se ), low_se[ 1 ] ), col=transp( "#2171B5" , 0.5 ), border=NA ) # fills area between the curves

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('darkgrey', .25), border = NA )


### Fig 1C
# onset and waking density and night shading 

par(mar = c(4.1, 5, 0, 1))

## make a density plot of the onset time and waking time
onset_dens <- density( ts_func( sleep_trim$onset_time ) )

wake_dens <- density(  ts_func( sleep_trim$waking_time ) )

waso_times <- full_dat[ !is.na( full_dat$sleep_per ) & full_dat$sleep_per == 1 & full_dat$sleep_bouts == 0, 'local_time' ]

waso_dens <- density( ts_func( waso_times ) )

x_range <- c( 0, 24*60*60 )

y_range <- c( min( onset_dens$y, wake_dens$y ), max( onset_dens$y, wake_dens$y ) )

plot( onset_dens$x, onset_dens$y, type = 'l', ylab = '', xlab = 'Time', xaxt = 'n', yaxt = "n", xlim = x_range, ylim = y_range, lty = 2, las = 1, col = c( '#08519C') )

#axis( 2, at = seq( 0, 0.0004, by = 0.0002 ), las = 1 )

title( ylab = "Probability Density", line = 3.9 )

lines( wake_dens$x, wake_dens$y, type = 'l', ylab = 'Density', xlab = 'Waking Time', xaxt = 'n', lty = 3, col = c('#4292C6' ) )


legend( x = 'topright', cex = 0.9, col = c( '#08519C', '#4292C6' ), lty = c( 1, 1 ), legend = c( 'Sleep onset', 'Waking time' ), bty = 'n' )

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('darkgrey', .25), border = NA )

axis( 1, at = seq( 0, 60*24*60, 3*60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 3*60*60) ), as_hms( seq( 0, 60*12*60, 3*60*60) ) ) ) 

axis( 1, at = seq( 0, 60*24*60, 1*60*60), labels = F ) 

axis( 2, at = c( 0.0, 0.0002, 0.0005 ), labels = c( '0.0', '0.0002', '0.0004' ), las = 2 )

#### End of Fig

##########################################
###### SLEEP ARCITECTURE EVALUATION ######

### use ts-func for this or do as follows:

temptime=as.numeric( as_hms(sleep_per_nona$onset ) )
temptime[which(temptime<15)]=temptime[which(temptime<15)]+24

temptime = hour(sleep_per_nona$onset)+(minute(sleep_per_nona$onset)/60)

##########################################


#sunset
as_hms( mean( as.numeric( as_hms( sun_trim$sunset ) ) ) )
as_hms(std.error( as.numeric( as_hms( sun_trim$sunset ) ) ) )

#sunrise 
as_hms( mean( as.numeric( as_hms( sun_trim$sunrise ) ) ) )
as_hms(std.error( as.numeric( as_hms( sun_trim$sunrise ) ) ) )

#night start
as_hms(ave_night_start)
as_hms(std.error( as.numeric( as_hms( sun_trim$night_start ) ) ) )

#night end
as_hms( mean( as.numeric( as_hms( sun_trim$night_end ) ) ) )
as_hms(std.error( as.numeric( as_hms( sun_trim$night_end ) ) ) )

#sleep onset
as_hms( mean( as.numeric( as_hms(sleep_per_nona$onset ) ) ) )
as_hms( median( as.numeric( as_hms(sleep_per_nona$onset ) ) ) )
as_hms(std.error( as.numeric( as_hms(sleep_per_nona$onset ) ) ) )

#sleep offset
as_hms( mean( as.numeric( as_hms(sleep_per_nona$waking ) ) ) )
as_hms( median( as.numeric( as_hms(sleep_per_nona$waking ) ) ) )
as_hms(std.error( as.numeric( as_hms(sleep_per_nona$waking ) ) ) )

#sleep efficency 
mean(sleep_per_nona$sleep_eff)
median(sleep_per_nona$sleep_eff)
std.error(sleep_per_nona$sleep_eff)

#sleep onset and night start
start = as_hms( mean( as.numeric( as_hms(sleep_per_nona$onset ) ) ) )-as_hms( mean( as.numeric( as_hms( sun_trim$night_start ) ) ) )
print(start)

#sleep offset and night end
end = as_hms( mean( as.numeric( as_hms(sleep_per_nona$waking ) ) ) )-as_hms( mean( as.numeric( as_hms( sun_trim$night_end ) ) ) )
print(end)

#sleep onset and sunset
start2 = as_hms( mean( as.numeric( as_hms(sleep_per_nona$onset ) ) ) )-as_hms( mean( as.numeric( as_hms( sun_trim$sunset ) ) ) )
print(start2)

#sleep offset and sunrise
end2 = as_hms( mean( as.numeric( as_hms(sleep_per_nona$waking ) ) ) )-as_hms( mean( as.numeric( as_hms( sun_trim$sunrise ) ) ) )
print(end2)



library(ggplot2)
ggplot(data = full_dat) + geom_density(aes(x = log_vedba, color = tag)) 


