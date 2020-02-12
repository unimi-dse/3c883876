
# Probability of Stock's Future Price

### 3c883876
The package predicts the probability that stock's future price at tomorrow’s close will be greater than today’s closing price and its output of a linear regression model.

## Installation

 `# first install the R package "devtools" if not installed
 devtools::install_github('unimi-dse/3c883876')`


## Usage

 `# load the package
 require(StocksFuturePriceR)`


## Analyze
The main function of the package is `stocks_future_price('id')`, in which 'id' is a character vector specifying the names of each symbol to be loaded. It is useful to predict stock's future price and its output of a linear regression model.


## Packages Imported

ggplot2, gridExtra, quantmod, stats, xts



**Author**: Veronica Astorino

**Date**: February 2020

