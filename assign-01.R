##initialize to use library sqldf
initialize <-function ()
{
  library(sqldf)
  library(ggplot2)
}

##This function return the full path+ file name to be read
getFullPathFileName <-function(pFileName)
{
  return (paste(paste(getwd(),paste("activity", pFileName, sep="/"), sep="/"), ".csv" , sep=""))
}

## Gets the Steps Dt
#This function reads the csv file into a datatable
getDtSteps <- function()
{ 
  if(!exists("dtSteps"))
  {
    dtSteps <<- read.csv(getFullPathFileName('activity'), header=T, sep=",")
  }
  dtSteps$steps <- as.numeric (dtSteps$steps)
  dtSteps$date <- as.Date(dtSteps$date, "%Y-%m-%d")    
  return (dtSteps);  
}



##Gets a datatable without NA values
getDtStepsWithoutNa <- function()
{
  dtSteps <- getDtSteps()  
  return (
sqldf("SELECT 
        *
        FROM dtSteps 
        WHERE steps <>'NA'
        ")
)
}

## Gets the plotting dt
# groupb by day
getPlotDtStepsWithoutNa <-function()
{
  dtSteps<- getDtStepsWithoutNa()
  return( sqldf("SELECT 
        date
        ,sum(steps) as totalSteps
        ,avg(steps) as avgSteps
        ,median(steps) as medianSteps
         
        FROM
        dtSteps
        GROUP BY date
        ")
  )
}

###plot 2
##What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

getPlotDtStepsWithoutNaGroupInterval <-function()
{
   dtSteps<- getDtStepsWithoutNa()
   return( sqldf("SELECT 
        interval
        ,sum(steps) as totalSteps
        ,avg(steps) as avgSteps
        ,median(steps) as medianSteps
         
        FROM
        dtSteps
        GROUP BY interval
        ")
   )
}

## @knitr plotHistogramWithoutNa
plotHistogramWithoutNa <-function()
{
  dtPlot <- getPlotDtStepsWithoutNa()
  #dtPlot <- sqldf("select date, totalSteps from dtPlot")
  #qplot(dtPlot$date, dtPlot$sumSteps) +geom_histogram()
  ggplot(data=dtPlot, aes(x=date, y=totalSteps)) + 
     geom_bar(stat="identity", colour="black") +
     xlab("date") +
     ylab("Total Steps") +
     ggtitle("Total steps per day")
  
  #_ggplot(data=dtPlot, aes(date)) + geom_bar(aes(y=totalSteps), stat="identity") + geom_line(aes(y=avgSteps, color="blue"))
  
  ##avg steps:
  #ggplot(data=dtPlot, aes(x=date, y=avgSteps)) + geom_bar(stat="identity", colour="blue")
}

#average steps without NA
getAverageStepsPerDay <-function()
{
  return(mean(getPlotDtStepsWithoutNa()$totalSteps))
}


#median steps without NA
getMedianStepsPerDay <-function()
{
  return (median(getPlotDtStepsWithoutNa()$totalSteps)  )
}

#This is used to fill missing values
getDtMedianStepsPerInterval <-function()
{
   dtSteps <- getDtSteps()
   return (sqldf("select interval, median(steps) as steps from dtSteps WHERE steps <>'NA'  group by interval"))
}

#insert missing values
getDtFilledMissingValues <- function()
{
   dtMedianMissingSteps <- getDtMedianStepsPerInterval()
   dtMissingSteps<- sqldf("select step1.*, 
                           step2.steps as missingStep 
                           FROM dtSteps step1 
                           left join dtMedianMissingSteps step2 on step1.interval = step2.interval")
   
   return(sqldf("select 
                dtMissingSteps.*
                , case  
                  when steps is null then missingStep 
                  else steps end as adjStep 
                from dtMissingSteps"))
}

# x <- as.data.frame(c(1,2,3,4,5))
# colnames(x) <- c("x")
# sqldf("select median(x) from x")
# lapply(x, class)

##how many unique dates not NA?
#length (unique(subset(dtSteps, !is.na(steps))$date ))

## how many unique dates NA?
#length (unique(subset(dtSteps, is.na(steps))$date ))

##total number of missing values
# nrow (subset(dtSteps, is.na(steps) ))

#dtStepsWithoutNa <- getPlotDtStepsWithoutNaGroupInterval()