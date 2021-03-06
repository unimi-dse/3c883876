% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StocksFuturePriceR.R
\name{stocks_future_price}
\alias{stocks_future_price}
\title{Probability of Stock's future price}
\usage{
stocks_future_price(id = "GOOG")
}
\arguments{
\item{id}{is a character vector specifying the names of each symbol to be loaded.}
}
\value{
newlist - a list of two components that contains the prediction and model.
}
\description{
The package predicts stock's future price and its output of a linear regression model.
}
\examples{
stocks_future_price ('GOOG')


}
