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