# Stock Analysis package
Author: Bartol Freškura
  
### Description
Package for extracting various information about stocks (historical prices, dividends, various ratios),
creating optimal portfolios and plotting graphs that relate to stocks.
  
The main purpose of this package is to have all information and tools for analyzing stock in one place,
so it can shorten the time used for gathering data and making calculations.
  
  
###Required packages
Before running the code it is neccessary to install and load several packages for R.

1. **ggplot2** - for plotting
2. **quantmod** - for downloading stocks data from multiple web sources
3. **foreach** and **doParallel** - multi core processing related libraries because some computations are
really complex and by default R does not use multiple cores
4. **R.matlab** - package used for reading .mat files

```{r}
library(R.matlab)
library(quantmod)
library(foreach)
library(doParallel)
library(ggplot2)
```

### Example of usage

####Example 1: Retrieve basic stock data
Lets first create the string containing tickers for companies we are interested in. We can also
load tickers for several stock indexes. webSource variable is used by quantmod package and it 
specifies the web source for downloading data. For more information read the quantmod documentation.

```{r}
webSource = "yahoo"
tickers = "WMT;AAPL;TSLA;"

#Indexes tickers: S&P 500, Russell 3000, Russell 2000, Dow Jones Industrial Average
indexTickerYahoo = c('^GSPC', '^RUA', '^RUT', '^DJI')
```
  
First step is to use the quantmod package to download financial data into list variables that are stored 
in R as **ticker.f**. This will create seperate .f files for every company we have in our tickers
string.
getFinancials is a function from quantmod package so if you want to know more about it read the docs.

```{r}
getFinancials(Symbol = tickers, src = webSource , auto.assign = TRUE)
```

Now that we have the required data, we are going to extract some useful information from it.
For example, lets see the cashflow statement, balance sheet and income statement for Apple.

```{r}
getBalanceSheet(AAPL.f, periodFreq = "A")
getIncomeStatement(AAPL.f, periodFreq = "A")
getCashFlow(AAPL.f, periodFreq = "A")
```
**NOTE:** The quantmod package is loading data from yahoo finance and it will only retrieve data for the last
4 periods. The same applies for google finance data.

  
Historical prices are one of the most frequent type of information used for analysing stocks. 
It's time to get that data:

```{r}
getHistoricPrices("MMM",srce = 'yahoo', timeSpan=4)
```
This will get us the historic prices for 3M from last 4 years on a daily basis. It will also provide
a date and the volume on the particular day.


