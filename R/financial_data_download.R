#load quantmod package
library(quantmod)

##Setup Constants
#sources available: google, yahoo, FRED (Federal Reserve)
webSource = "google"

#Pick the tickers for analysis
tickers = "WMT;AAPL;TSLA;"

#Indexes tickers: S&P 500, Russell 3000, Russell 2000, Dow Jones Industrial Average
indexTickerYahoo = c('^GSPC', '^RUA', '^RUT', '^DJI')

## Get Stocks Financials
# Symbol or more valid google symbol, as a character vector or semi-colon
# Variables are returned in format of 'TickerName.f'
getFinancials(Symbol = tickers, src = webSource , auto.assign = TRUE)


#' @title  Download and view balance sheet
#' @author Bartol Freškura
#' @description
#' Return data from balance sheet for the last 4 periods (quarterly or annually)
#' @param company: data from getFinancials functions in the form of
#'  'CompanyName.f'. Only ONE company can be in the argument.
#' @param periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return Returns dataFrame type with balance sheet data
#' @usage getBalanceSheet('AAPL',periodFreq='Q'), getBalanceSheet('TSLA',periodFreq='A')
getBalanceSheet <- function(company, periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "BS")
      data.frame(x)
}

#'@title Download and view income statement
#'@author Bartol Freškura
#' @description
#' Return data from Income statement for the last 4 periods (quarterly or annually)
#' @param company: data from getFinancials functions in the form of
#'  'CompanyName.f'. Only ONE company can be in the argument.
#' @param  periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type with income statement data
getIncomeStatement <- function(company, periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "IS")
      data.frame(x)
}



#' @title Download and view cash flow statement
#' @author Bartol Freškura
#' @description
#' Return data from Cash Flow Statement for the last 4 periods (quarterly or annually)
#' @param company: data from getFinancials functions in the form of
#' 'CompanyName.f'. Only ONE company can be in the argument.
#' @param periodFreq: can be 'Q' for quarterly data and 'A' for annual data.
#' @return  Returns dataFrame type with cash flow data
getCashFlow <- function(company,periodFreq = 'A') {
      x <- viewFinancials(company,period = periodFreq,type = "CF")
      data.frame(x)
}


#'@title Download and view prices from history
#'@author Bartol Freškura
#'@description
#'Returns dataframe with this columns: Open Price, High Price, Low Price, Close
#' Price, Volume, Adjusted
#'@param company: Company ticker
#'@param srce: website source
#'@param timeSpan Time interval in years. timeSpan of 2 means that data. Rounds
#' to integer numbers.
#' from last 2 years will be retrieved. Default is 1 year.
#'@return dataframe with historic prices
getHistoricPrices <- function(company,srce = 'yahoo', timeSpan=1) {
      x <- getSymbols(Symbols = company,src = srce, env = NULL)
      yearFrom <- toString(as.numeric(c(format(Sys.Date(), "%Y"))) - round(timeSpan))
      rawConverted <- coredata(x)

      openPrice <- double()
      highPrice <- double()
      lowPrice <- double()
      closePrice <- double()
      volume <- double()
      adjusted <- double()

      #Extract data from .xts table
      for(i in 1:nrow(rawConverted)){
            openPrice <- c(openPrice, rawConverted[i,1])
            highPrice <- c(highPrice, rawConverted[i,2])
            lowPrice <- c(lowPrice, rawConverted[i,3])
            closePrice <- c(closePrice, rawConverted[i,4])
            volume <- c(volume, rawConverted[i,5])
           # adjusted <- c(adjusted, rawConverted[i,6])
      }
      colNames <- c('Date', 'Open','High', 'Low','Close', 'Volume')
      data <- data.frame(index(x),openPrice,highPrice,lowPrice,closePrice,volume)
      colnames(data) <- colNames

      boolVectorDate <- data[,1]>=paste(yearFrom,"-01-01",sep = '')
      data[boolVectorDate,]

}


#' @title Download and view dividends
#' @author Bartol Freškura
#' @description
#' Function returns dividends for the specified company.
#' @param company ticker for the company.
#' @param timeSpan Time interval in years. timeSpan of 2 means that dividends
#' from last 2 years will be retrieved. Default is 1 year. Rounds
#' to integer numbers.
#' @param srce: website source
#' @return Data frame consisting of 2 columns: date of dividend and the ammount
#'  in US Dollars.
#' @usage getHistoricDividends(company='CBL', timeSpan=4, srce='yahoo')
getHistoricDividends <- function(company, timeSpan=1, srce="yahoo") {
      #Date calculation
      yearFrom <- toString(as.numeric(c(format(Sys.Date(), "%Y"))) - round(timeSpan))

      divTemp <- getDividends(Symbol = company,
                              from = paste(yearFrom,"-01-01", sep = ''),
                              src = srce, env=NULL)
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

#' @title Download and view some basic information
#' @author Bartol Freškura
#' @description
#' Returns summary information about the company (e.g. Various ratios,  volume...)
#' Returns only from yahoo.com
#' Information that is returned: Symbol, Earnings/Share, EPS Estimate Current Year,
#' EPS Estimate Next Quarter, 52-week Low,
#' 200-day Moving Average, Price/Book, P/E Ratio,
#' Dividend Yield, Dividend/Share, Price/Sales,
#' Shares Owned, Volume, Stock Exchange,
#' EPS Estimate Next Year, 52-Week-High
#'
#' @param company ticker for the company.
#' @return Data frame with specified information
#' @usage getSummary('AAPL'), getSummary('WMT')
getSummary <- function(company) {
      informationVector <- c('Symbol','Earnings/Share','EPS Estimate Current Year',
                             'EPS Estimate Next Quarter','52-week Low',
                             '200-day Moving Average','Price/Book','P/E Ratio',
                             'Dividend Yield','Dividend/Share','Price/Sales',
                             'Shares Owned','Volume','Stock Exchange',
                             'EPS Estimate Next Year',
                             '52-Week-High')
      raw <- getQuote(company, what = yahooQF(informationVector))
      data.frame(raw)
}
