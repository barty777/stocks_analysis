
#' @name Calculate Portoflio Weighted Average Return
#' @author Bartol Freškura
#' @description Calculates weighted average returns in given portfolio of stocks for every stock
#' @param companiesReturns Data Frame of returns for every stock
#' @param weightsArray Array of weights
#' @return Mean of the returnes for given portfolio and weights
#' @usage portfolioAvgReturn(frameReturns, c(0.2,0.5,0.3)))
#'
portfolioAvgReturn <- function(companiesReturns, weightsArray){
      meanRet <- numeric()

      for(i in 1:ncol(companiesReturns)){
            temp <- mean(companiesReturns[,i])
            meanRet <- c(meanRet, temp)
      }

      ##calcuating portfolio avg returns
      portRet <- sum((weightsArray) * meanRet)
      portRet
}



#' @name Calculate Portoflio Average Return Array
#' @author Bartol Freškura
#' @description Calculates average return for every stock in portfolio
#' @param companiesReturns Data Frame of returns for every stock. Every columns contains returns
#' for one company.
#' @return Array of means for every stock in portfolio
#' @usage portfolioAvgReturn(frameReturns, c(0.2,0.5,0.3)))
#'
portfolioAvgReturnArray <- function(companiesReturns){

      for(i in 1:ncol(companiesReturns)){
            temp <- mean(companiesReturns[,i])
            meanRet <- c(meanRet, temp)
      }
      meanRet
}


#' @name Calculate Portoflio Standard Deviation
#' @author Bartol Freškura
#' @description Calculates Standard deviation for a given portfolio of stocks
#' For more information how the function works: https://www.youtube.com/watch?v=ZfJW3ol2FbA
#' @param companiesReturns Data Frame of returns for every stock
#' @param portfolioReturns array of average returns of stocks for the given portfolio
#' @param weightsArray Array of weights for every stock
#' @return Mean of the returnes for given portfolio and weights
#' @usage portfolioStdev(frameReturns, c(0.03,0.04,-0.04), c(0.2,0.5,0.3)))
portfolioStdev <- function(companiesReturns, portfolioReturns, weightsArray){
      for(i in 1:ncol(companiesReturns)){

            vecDiff <- companiesReturns[,i]-portfolioReturns[i]
            if(i == 1){

                  diffsFrame <- data.frame(vecDiff)
            }
            else{
                  diffsFrame <- data.frame(diffsFrame,vecDiff)
            }
      }

      #get covariances-variances matrix
      diffMatrix <- as.matrix(diffsFrame)
      covMatrix <- (t(diffMatrix) %*% diffMatrix) / nrow(diffMatrix)
      stdev <- sqrt( (weightsArray%*%covMatrix)%*%t(t(weightsArray)))
      stdev
}
