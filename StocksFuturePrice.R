#' @title Probability of Stock's future price
#'
#' @description The package help to predict stock's future price.
#'
#' @param 'GOOG'
#'
#' @return NULL
#'
#' @examples stocks_future_price('GOOG')
#'
#' @export

stocks_future_price<-function(GOOG)
{


  # recall Google quotes
  GOOG <- getSymbols(Symbols = 'GOOG', src = 'yahoo', auto.assign =FALSE)
  summary(GOOG)

  # importing price data
  GOOG<-data.frame(xts::as.xts(GOOG))

  # attributed the column names
  names(GOOG) <- c("data.Open"   ,  "data.High"   ,  "data.Low"   ,   "data.Close"  ,  "data.Volume",  "data.Adjusted")
  names(GOOG)

  # creating lag and lead features of price column
  GOOG <- xts::xts(GOOG,order.by=as.Date(rownames(GOOG)))
  GOOG <- as.data.frame(merge(GOOG, lm1=stats::lag(GOOG[,'data.Adjusted'],c(-1,1,3,5,10))))

  # features
  GOOG$Date<-as.Date(rownames(GOOG))
  GOOG$Day_of_month<-as.integer(format(as.Date(GOOG$Date),"%d"))
  GOOG$Month_of_year<-as.integer(format(as.Date(GOOG$Date),"%m"))
  GOOG$Year<-as.integer(format(as.Date(GOOG$Date),"%y"))
  GOOG$Day_of_week<-as.factor(weekdays(GOOG$Date))

  head(GOOG)


  # plot data using data.Open
  plot(GOOG[, "data.Open"], main = "data")

  # plot data using day of week and low data
  ggplot(GOOG, aes(Day_of_week, data.Low)) +
    geom_point(na.rm=TRUE, color="blue", size=3, pch=18)
  #plot data using Year and High data
  ggplot(GOOG, aes(data.High, Year)) +
    geom_point(na.rm=TRUE, color="red", size=3, pch=18)


  # naming variables for reference
  today <- 'data.Adjusted'
  tommorow <- 'data.Adjusted.5'

  # building outcome
  GOOG$up_down <- as.factor(ifelse(GOOG[,tommorow] > GOOG[,today], 1, 0))

  # building train and test sets
  train<-GOOG[stats::complete.cases(GOOG),]
  test<-GOOG[nrow(GOOG),]


  # training model
  model<-stats::glm(up_down~data.Open+data.High+data.Low+data.Close+
                      data.Volume+data.Adjusted+data.Adjusted.1+
                      data.Adjusted.2+data.Adjusted.3+data.Adjusted.4+
                      Day_of_month+Month_of_year+Year+Day_of_week,
                    family=binomial(link='logit'),data=train)


  # making Predictions
  pred<-as.numeric(stats::predict(model,test[,c('data.Open','data.High','data.Low','data.Close','data.Volume','data.Adjusted','data.Adjusted.1','data.Adjusted.2','data.Adjusted.3','data.Adjusted.4','Day_of_month','Month_of_year','Year','Day_of_week')],type = 'response'))

  # printing results
  print("Probability of Stock price going up tommorow:")
  print(pred)

}
