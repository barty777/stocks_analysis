
#' @name Calculate Portoflio Return
#' @author Bartol Freškura
#' @description Calculates mean return in given portfolio of stocks
#' @param companiesTicker Array of tickers
#' @param weightsArray Array of weights
#' @param timespan Time interval in years. timeSpan of 2 means that data. Rounds
#' to integer numbers.
#' @return Mean of the returnes for given portfolio and weights
#' @usage portfolioAvgReturn(c("AAPL","WMT","GE"), c(0.2,0.5,0.3),4))
portfolioAvgReturn <- function(companiesTicker, weightsArray, timespan){
      stdev <- numeric()
      meanRet <- numeric()
      var <- numeric()

      for(i in 1:ncol(companiesTicker)){

#             companyPrices <- getHistoricPrices(company = companiesTicker[i],timeSpan = timespan)
#             companyReturns <- getReturns(companyPrices)
            temp <- getReturnsMeanVar(companiesTicker[,i])

            stdev <- c(stdev, temp$Stdev)
            meanRet <- c(meanRet, temp$Mean)
            var <- c(var,temp$Variance)
      }
      frame <- data.frame(meanRet,stdev,var)
      # rownames(frame) <- companiesTicker
      colnames(frame) <- c("Mean", "Stdev", "Variance")


      ##calcuating portfolio returns
      returnsMat <- (frame$Mean)
      portRet <- sum((weightsArray) * returnsMat)
      portRet
}



#' @name Calculate Portoflio Standard Deviation
#' @author Bartol Freškura
#' @description Calculates Standard deviation in given portfolio of stocks
#' @param companiesTicker Array of tickers
#' @param portfolioReturns Average return for the given portfolio
#' @param timespan Time interval in years. timeSpan of 2 means that data. Rounds
#' to integer numbers.
#' @return Mean of the returnes for given portfolio and weights
#' @usage portfolioStdev(c("AAPL","WMT","GE"), 0.0063, 4))
portfolioStdev <- function(companiesTicker,portfolioReturns,timespan,weightsMatrixArray){

      for(i in 1:ncol(companiesTicker)){

#
#             companyPrices <- getHistoricPrices(company = companiesTicker[i],timeSpan = timespan)
#             companyReturns <- getReturns(companyPrices)

            vecDiff <- companyReturn[,1]-portfolioReturns

            if(i == 1){
                  diffsFrame <- data.frame(vecDiff)
            }
            else{
                  diffsFrame <- data.frame(vecDiff,diffsFrame)
            }

      }
      colnames(diffsFrame) <- companiesTicker


      #get covariances-variances matrix
      diffMatrix <- as.matrix(diffsFrame)
      covMatrix <- (t(diffMatrix) %*% diffMatrix) / nrow(diffMatrix)

      stdev <- sqrt( (weightsMatrixArray %*% covMatrix) %*% t(t(weightsMatrixArray)))
      stdev

}
