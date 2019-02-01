#title: "Predicitive Maintenance of of bicycles - Oslo"
#author: "G. Jan"
#date: "Sept 2018"

## This is rough coding - unfortunately I don t have time to make it nicer

library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(dplyr)
library(data.table)
library(sqldf)
library(caret)


####################################################################################################################################
# Loading maintenance log
library(readxl)
maint <- read_excel("/home/greg/Documents/Candidature/CaseBike/CASE_repairs_sample.xlsx",col_types = c("numeric","text","date"))
maint <- as.data.frame(maint)
library(lubridate)
Sys.setlocale("LC_TIME", "C")
maint$ndate <- as.numeric(maint$repaired_at)
maint$year <- year(maint$repaired_at)

# Aggregate per bike id and date
agg <- aggregate(year ~ bike_id + ndate, maint,max)

# Preing date and time
agg$repaired_at <- as.POSIXct(agg$ndate, origin="1970-01-01",tz="UTC")
agg$year <- year(agg$repaired_at)
agg$month <- month(agg$repaired_at)
agg$month <- factor(agg$month, labels=c("Mar",
                                        "Apr","May","Jun",
                                        "Jul","Aug","Sep",
                                        "Oct","Nov","Dec"),levels= c("3", "4", 
                                       "5", "6", "7", "8", "9","10","11","12"))
agg$day <- day(agg$repaired_at)
agg$hour <- hour(agg$repaired_at)
agg$wday <- weekdays(agg$repaired_at)
agg$wday <- factor(agg$wday, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
agg$ymd <- ymd(paste(agg$year,agg$month,agg$day))
agg2017 <- agg[agg$year==2017,] # keep 2017 data only

##################################################################################
# Loading bike trips
trips <- read.csv("/home/greg/Documents/Candidature/CaseBike/CASE_trips_sample",sep=",")

# Preping date/time
trips$trip_started_at <- gsub(pattern = "UTC",replacement = "",trips$trip_started_at)
trips$trip_ended_at <- gsub(pattern = "UTC",replacement = "",trips$trip_ended_at)
trips$trip_started_at <- as.POSIXct(trips$trip_started_at,tz=" UTC")
trips$trip_ended_at <- as.POSIXct(trips$trip_ended_at,tz=" UTC")

trips$numstart <- as.numeric(trips$trip_started_at)
trips$numend <- as.numeric(trips$trip_ended_at)

trips$year <- year(trips$trip_ended_at)
trips$month <- month(trips$trip_ended_at)
trips$month <- factor(trips$month, labels=c("Mar",
                                        "Apr","May","Jun",
                                        "Jul","Aug","Sep",
                                        "Oct","Nov","Dec"),levels= c("3", "4", 
                                                                     "5", "6", "7", "8", "9","10","11","12"))
trips$day <- day(trips$trip_ended_at)
trips$hour <- hour(trips$trip_ended_at)
trips$wday <- weekdays(trips$trip_ended_at)
trips$wday <- factor(trips$wday, levels= c("Sunday", "Monday", 
                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
trips$ymd <- ymd(paste(trips$year,trips$month,trips$day))


tripsdt <- as.data.table(trips[order(trips$bike_id,trips$trip_ended_at),])

# get time of following trip
tripsdt[ , next_trip := shift(trip_started_at,type="lead")] 
tripsdt <- tripsdt[, c(1:6,28,7:27)]
tripsdt <- tripsdt %>% 
  group_by(bike_id) %>% 
  mutate(next_trip= ifelse(row_number()==n(), NA, next_trip))
tripsdt$numnext <-tripsdt$next_trip
tripsdt$next_trip <- as.POSIXct(tripsdt$next_trip, origin="1970-01-01",tz="UTC")

####################################################################################################################################
# Merging maintenance log and bike trips
# Assumtion = bike broke on the last trip before maintenance

mge <- sqldf("
  SELECT *
  FROM tripsdt t LEFT JOIN agg a
  ON t.bike_id = a.bike_id
  AND a.ndate BETWEEN t.numend AND t.numnext
")
# Preping date and time
mge$repaired_at <- as.POSIXct(mge$ndate,origin="1970-01-01",tz="UTC")
mge$trip_started_at <- as.POSIXct(mge$numstart,origin="1970-01-01",tz="UTC")
mge$trip_ended_at <- as.POSIXct(mge$numend,origin="1970-01-01",tz="UTC")
mge$next_trip <- mge$next_trip-3600*2

# Set broke flag for trips that borke the bikes according to assumption above
mge$broke <- ifelse(is.na(mge$ndate),0,1)

# Remove NAs and and outliers
mge$member_gender[mge$member_gender == ""] <- NA
mgena <- mge[!is.na(mge$member_birth_year),]
mgena <- mgena[between(mgena$member_birth_year,1947,2001),]
mgena <- mgena[!is.na(mgena$member_gender),]

######
# Create new features
######

mgena$flag <- c(FALSE,diff(mgena$broke) < 0 | diff(mgena$bike_id) != 0)

# Cumulate time each bike is in used until repair
mgena$cumduration <- with(mgena, ave(trip_duration, cumsum(flag == TRUE), FUN = cumsum))
mgena$brokemax <- with(mgena, ave(broke, cumsum(flag == TRUE), FUN = max))
mgena$groupindex <- with(mgena, ave(bike_id, cumsum(flag == TRUE), FUN = seq_along))
mgena$groupmax <- with(mgena, ave(groupindex, cumsum(flag == TRUE), FUN = max))
# Create linear percentage until bike breaks = 100%
mgena$percm <- mgena$groupindex/mgena$groupmax


####################################################################################################################################
#### Machine learning: Random Forest

set.seed(333)

# Remove bike that did not break
mgenar <- mgena[mgena$brokemax==1,]
# Split dataset
trainpartr <- createDataPartition(y=mgenar$percm, p=0.7, list=FALSE)
trainingr <- mgenar[trainpartr, ]
testingr <- mgenar[-trainpartr, ]


trainingr <- trainingr[,c(2,4,8,9,10,24,26,27,41,44)]

# train model
fitrfm<- train(percm ~ .,data=trainingr,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                     #tuneGrid = tgrid,
                     num.trees = 100,
                     importance = "permutation")

# Prediction
predrfm <- predict(fitrfm,newdata = testingr)

# Metrics
SSE = sum((testingr[,44] -predrfm)^2)    # sum of squared errors
SST = sum((testingr[,44] - mean(trainingr[,10]))^2) # total sum of squares, remember to use training data here
R_square = 1 - SSE/SST
message('R_squared on the test data:')
round(R_square, 2)

SSE = sum((testingr[,44] - predrfm)^2)
RMSE = sqrt(SSE/length(predrfm))
message("Root mean square error on the test data: ")
round(RMSE, 2)
varImp(fitrfm)

# Graph predictions vs observations
testingr$predrfm <- predrfm
ggplot(testingr,aes(testingr$percm,testingr$predrfm))+ geom_point(alpha = 1/10) + 
  geom_smooth(method='lm',aes(fill="blue")) + theme_classic()  + xlab("Observed") + ylab("Predicted") + ggtitle("Observed vs Predicted percentage broken")