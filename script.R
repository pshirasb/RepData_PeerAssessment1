
library("plyr")

# Extract and load raw data
if(!file.exists("./activity.csv")){
    unzip(zipfile="./activity.zip")
}
activity_raw = read.csv("./activity.csv")
str(activity_raw)

# Find the total number of steps per day 
steps_per_day = ddply( activity_raw, "date", summarise
                     , total = sum(steps, na.rm=T)
                     , avg   = mean(steps, na.rm=T)
                     , med   = median(steps, na.rm=T)
                     )

# Draw the steps per day histogram
hist( x      = steps_per_day$total
    , col    = "lightblue" 
    , border = "pink"
    , main   = "Steps Per Day Histogram"
    , xlab   = "steps per day"
    )

# Calculate mean total number of steps per day
spd_mean = mean(steps_per_day$total, na.rm=T)

# Calculate median total number of steps per day
spd_median = median(steps_per_day$total, na.rm=T)

print(spd_mean)
print(spd_median)

# Time series plot of average daily activity pattern
daily_activity = ddply( activity_raw
                      , "interval"
                      , summarise
                      , avg = mean(steps, na.rm=T)
                      )

plot( x      = daily_activity
    , col    = "darkgreen"
    , type   = "l"
    , main   = "Average Daily Activity Pattern"
    , ylab   = "Average steps across all days"
    )

# Number of incomplete records
num_missing = nrow(activity_raw) - sum(complete.cases(activity_raw))
print(num_missing)

# Fill the missing data with the mean of that interval
index_complete   = complete.cases(activity_raw)
replace_na = function(x) {
                return(daily_activity[daily_activity$interval == x,"avg"])
             }
activity_filled = activity_raw
activity_filled[!index_complete,] = ddply( activity_raw[!index_complete,]
                                        , .(date, interval)
                                        , transform
                                        , steps = replace_na(interval)
                                        )

# Find the total number of steps per day, 
steps_per_day = ddply( activity_filled, "date", summarise
                     , total = sum(steps, na.rm=T)
                     , avg   = mean(steps, na.rm=T)
                     , med   = median(steps, na.rm=T)
                     )

# Draw the steps per day histogram
hist( x      = steps_per_day$total
    , col    = "lightblue" 
    , border = "pink"
    , main   = "Steps Per Day Histogram"
    , xlab   = "steps per day"
    )

# Calculate mean total number of steps per day
spd_mean = mean(steps_per_day$total, na.rm=T)

# Calculate median total number of steps per day
spd_median = median(steps_per_day$total, na.rm=T)

print(spd_mean)
print(spd_median)

# Weekdays vs Weekends

isWeekday = function(x) {
                day = weekdays(as.Date(x),T)
                return(ifelse(day %in% c("Sun","Sat"),"weekend","weekday")) 
            }

activity_filled_ext = transform( activity_filled
                               , day = isWeekday(date)
                               )

activity_filled_ext$day = as.factor(activity_filled_ext$day)


# Time series plot of average Weekend/Weekday activity pattern
weekend_activity = ddply( activity_filled_ext[activity_filled_ext$day == "weekend",]
                        , "interval"
                        , summarise
                        , avg = mean(steps, na.rm=T)
                        )
weekday_activity = ddply( activity_filled_ext[activity_filled_ext$day == "weekday",]
                        , "interval"
                        , summarise
                        , avg = mean(steps, na.rm=T)
                        )
par(mfrow=c(2,1))
plot( x      = weekend_activity
    , col    = "darkgreen"
    , type   = "l"
    , main   = "Average Weekend Activity Pattern"
    , ylab   = "Average steps across all weekends"
    )
plot( x      = weekday_activity
    , col    = "darkblue"
    , type   = "l"
    , main   = "Average Weekday Activity Pattern"
    , ylab   = "Average steps across all weekdays"
    )

