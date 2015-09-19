##This file is used for testing

#Load companies
companies <- c('AAPL','WMT','TSLA','GE')

#Indexes tickers: S&P 500, Russell 3000, Russell 2000, Dow Jones Industrial Average
indexTickerYahoo = c('^GSPC', '^RUA', '^RUT', '^DJI')


companyPrices <- getHistoricPrices(company = companies[2],timeSpan = 10)
indexPrices <- getHistoricPrices(company = indexTickerYahoo[1],timeSpan = 10)

companyReturns <- getReturns(companyPrices)
indexReturns <- getReturns(indexPrices)

beta <- getBetaMeanVar(companyReturns,indexReturns)
