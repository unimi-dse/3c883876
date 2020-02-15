#' @title Probability of Stock's future price
#'
#'
#' @description The package predicts stock's future price and its output of a linear regression model.
#'
#'
#' @param id is a character vector specifying the names of each symbol to be loaded.
#'
#'
#' @return newlist - a list of two components that contains the prediction and model.
#'
#
#' @examples stocks_future_price ('GOOG')
#'
#'
#' @export
#'

stocks_future_price<- function(id="GOOG")
{
  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  #recall quotes
  id <- quantmod::getSymbols(Symbols = id, src = 'yahoo', auto.assign =FALSE)


  # importing price data
  id<-data.frame(id)

  # attributed the column names
  names(id) <- c("data.Open"   ,  "data.High"   ,  "data.Low"   ,   "data.Close"  ,  "data.Volume",  "data.Adjusted")

  # creating lag and lead features of price column
  id <- xts::xts(id,order.by=as.Date(rownames(id)))
  id <- as.data.frame(merge(id, lm1=stats::lag(id[,'data.Adjusted'],c(-1,1,3,5,10))))

  # features
  id$Date<-as.Date(rownames(id))
  id$Day_of_month<-as.integer(format(as.Date(id$Date),"%d"))
  id$Month_of_year<-as.integer(format(as.Date(id$Date),"%m"))
  id$Year<-as.integer(format(as.Date(id$Date),"%y"))
  id$Day_of_week<-as.factor(weekdays(id$Date))


  # plot data using day of week and data.Low
  p1<-ggplot2::ggplot(id, ggplot2::aes(Day_of_week, data.Low)) +
    ggplot2::geom_point(na.rm=TRUE, color="blue", size=3, pch=18)
  ggplot2::ggtitle("Low data") +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.7, face="bold"))

  #plot data using Date and data.Close
  p2<-ggplot2::ggplot(id, ggplot2::aes(Date, data.Close)) +
    ggplot2::geom_point(na.rm=TRUE, color="red", size=3, pch=18)
  ggplot2::ggtitle("Closing Stock Prices") +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.7, face="bold"))

  # plot data using day of week and data.High
  p3<-ggplot2::ggplot(id, ggplot2::aes(Day_of_week, data.High)) +
    ggplot2::geom_point(na.rm=TRUE, color="yellow", size=3, pch=18)
  ggplot2::ggtitle("High data") +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.7, face="bold"))

  #plot data using Date and data.Close
  p4<-ggplot2::ggplot(id, ggplot2::aes(Date, data.Open)) +
    ggplot2::geom_point(na.rm=TRUE, color="green", size=3, pch=18)
  ggplot2::ggtitle("Opening Stock Prices") +
    ggplot2::theme(plot.title = ggplot2::element_text(lineheight=.7, face="bold"))

  gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)

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
  pred<-as.numeric(stats::predict(model,test[,c('data.Open','data.High','data.Low','data.Close','data.Volume','data.Adjusted','data.Adjusted.1','data.Adjusted.2','data.Adjusted.3','data.Adjusted.4','Day_of_month','Month_of_year','Year','Day_of_week')], scale=1, type = 'response'))

  newlist <- list(pred,model)

  return(newlist)

}
