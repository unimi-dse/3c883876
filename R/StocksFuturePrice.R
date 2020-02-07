#' @title Probability of Stock's future price
#'
#' @description The package helps to predict stock's future price.
#'
#' @param 'GOOG'
#'
#' @return NULL
#'
#' @examples stocks_future_price('GOOG')
#'
#' @export

stocks_future_price<-function(id="GOOG")
{


  # recall Google quotes
  GOOG <- quantmod::getSymbols(Symbols = id, src = 'yahoo', auto.assign =FALSE)


  # importing price data
  id<-data.frame(xts::as.xts(id))

  # attributed the column names
  names(id) <- c("data.Open"   ,  "data.High"   ,  "data.Low"   ,   "data.Close"  ,  "data.Volume",  "data.Adjusted")


  # creating lag and lead features of price column
  id <- xts::xts(id,order.by=as.Date(rownames(id)))
  id <- as.data.frame(merge(GOOG, lm1=stats::lag(id[,'data.Adjusted'],c(-1,1,3,5,10))))

  # features
  id$Date<-as.Date(rownames(id))
  id$Day_of_month<-as.integer(format(as.Date(id$Date),"%d"))
  id$Month_of_year<-as.integer(format(as.Date(id$Date),"%m"))
  id$Year<-as.integer(format(as.Date(id$Date),"%y"))
  id$Day_of_week<-as.factor(weekdays(id$Date))


  # plot data using data.Open
  plot(id[, "data.Open"], main = "data")

  # plot data using day of week and low data
  ggplot2::ggplot(id, ggplot2::aes(Day_of_week, data.Low)) +
    ggplot2::geom_point(na.rm=TRUE, color="blue", size=3, pch=18)
  #plot data using Year and High data
  ggplot2::ggplot(id, ggplot2::aes(data.High, Year)) +
    ggplot2::geom_point(na.rm=TRUE, color="red", size=3, pch=18)


  # naming variables for reference
  today <- 'data.Adjusted'
  tommorow <- 'data.Adjusted.5'

  # building outcome
  id$up_down <- as.factor(ifelse(id[,tommorow] > id[,today], 1, 0))

  # building train
  train<-id[stats::complete.cases(id),]

  #building test
  test<-id[nrow(id),]


  # training model
  model<-stats::glm(up_down~data.Open+data.High+data.Low+data.Close+
                      data.Volume+data.Adjusted+data.Adjusted.1+
                      data.Adjusted.2+data.Adjusted.3+data.Adjusted.4+
                      Day_of_month+Month_of_year+Year+Day_of_week,
                    family=binomial(link='logit'),data=train)


  # making Predictions
  pred<-as.numeric(stats::predict(model,test[,c('data.Open','data.High','data.Low','data.Close','data.Volume','data.Adjusted','data.Adjusted.1','data.Adjusted.2','data.Adjusted.3','data.Adjusted.4','Day_of_month','Month_of_year','Year','Day_of_week')],type = 'response'))

  # printing results
  print("Probability of Stock's price going up tommorow:")
  print(pred)

}
