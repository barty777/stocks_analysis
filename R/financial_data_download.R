#load quantmod package
library(quantmod)

##Setup Constants
#sources available: google, yahoo, FRED
webSource = "google"
#Pick the tickers for analysis
tickers = "WMT;AAPL;TSLA"

## Get Stocks Financials
# Symbol or more valid google symbol, as a character vector or semi-colon
# Variables are returned in format of TickerName.f
getFinancials(Symbol = tickers, src = webSource , auto.assign = TRUE)

#' Return data from balance sheet
#' @param company: data from getFinancials functions in the form of 'CompanyName
#' .f'. Only ONE company can be in the argument.
#' @param periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return Returns dataFrame type with balance sheet data
getBalanceSheet <- function(company, periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "BS")
      data.frame(x)
}

#' Return data from Income statement
#' @param company: data from getFinancials functions in the form of
#'  'CompanyName.f'. Only ONE company can be in the argument.
#' @param  periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type with income statement data
getIncomeStatement <- function(company, periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "IS")
      data.frame(x)
}

#' Return data from Cash Flow Statement
#' @param company: data from getFinancials functions in the form of
#' 'CompanyName.f'. Only ONE company can be in the argument.
#' @param periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type with cash flow data
getCashFlow <- function(company,periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "CF")
      data.frame(x)
}

#'Returns dataframe with this columns: Open Price, High Price, Low Price, Close
#' Price, Volume, Adjusted
#'@param company: Array of company tickers
#'@param srce: website source
#'@return dataframe with historic prices
getHistoricPrices <- function(company,srce = 'google') {
      x <- getSymbols(Symbols = company,src = srce, symbol.lookup = TRUE, env = NULL)
      data.frame(x)
}

#' Function returns dividends for the specified company.
#' @param company ticker for the company.
#' @param timeSpan Time interval in years. timeSpan of 2 means that dividends
#' from last 2 years will be retrieved. Default is 1 year.
#' @return Data frame consisting of 2 columns: date of dividend and the ammount in US Dollars.
getHistoricDividends <- function(company, timeSpan=1, srce="google") {
      #Date calculation
      yearFrom <- toString(as.numeric(c(format(Sys.Date(), "%Y"))) - timeSpan)

      divTemp <- getDividends(Symbol = company, from = paste(yearFrom,"-01-01", sep = ''), src = srce, env=NULL)
      ammount <- double()

      #Extract dividends from .xts table
      for(i in 1:nrow(divTemp)){
            ammount <- c(ammount, coredata(divTemp[i]))
      }

      colNames <- c('Date of dividend','Amount in USD')
      data <- data.frame(index(divTemp),ammount)
      colnames(data) <- colNames
      data
}
