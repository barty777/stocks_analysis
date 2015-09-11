#load quantmod package
library(quantmod)

#setup Constants
webSource = "google"
tickers="WMT;AAPL;TSLA"

# Get Stocks Financials
# Symbolo ne or more valid google symbol, as a character vector or semi-colon delimited string
# Variables are returned in format of TickerName.f
getFinancials(Symbol = tickers, src = webSource , auto.assign = TRUE)

#' Return data from balance sheet
#' @param company data from getFinancials funcitons; period can be 'Q' for quarterly data and 'A' for annual data.
#' @return Returns dataFrame type
getBalanceSheet <- function(company, periodFreq){
  x<-viewFinancials(company,period=periodFreq,type = "BS")
  data.frame(x)
}

#' Return data from Income statement
#' @param company data from getFinancials funcitons; period can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type
getIncomeStatement <- function(company, periodFreq){
  x<-viewFinancials(company,period=periodFreq,type = "IS")
  data.frame(x)
}

#' Return data from Cash Flow Statement
#' @param company data from getFinancials funcitons; period can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type with cash flow data
getCashFlow <- function(company,periodFreq){
  x<-viewFinancials(company,period=periodFreq,type = "CF")
  data.frame(x)
}


