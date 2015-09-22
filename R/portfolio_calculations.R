
#' @name Calculate Portoflio Return
#' @author Bartol Freškura
#' @description Calculates mean return in given portfolio of stocks
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



#' @name Calculate  Portoflio Return Array
#' @author Bartol Freškura
#' @description Calculates array of mean returns for every stock in portfolio
#' @param companiesReturns Data Frame of returns for every stock
#' @param weightsArray Array of weights
#' @return Array of means for every stock in portfolio
#' @usage portfolioAvgReturn(frameReturns, c(0.2,0.5,0.3)))
#' 
portfolioAvgReturnArray <- function(companiesReturns, weightsArray){
      meanRet <- numeric()
      
      for(i in 1:ncol(companiesReturns)){
            temp <- mean(companiesReturns[,i])
            meanRet <- c(meanRet, temp)
      }

      meanRet
}


#' @name Calculate Portoflio Standard Deviation
#' @author Bartol Freškura
#' @description Calculates Standard deviation in given portfolio of stocks
#' @param companiesReturns Data Frame of returns for every stock
#' @param portfolioReturns array of average returns of stocks for the given portfolio
#' @param weightsArray Array of weights
#' @return Mean of the returnes for given portfolio and weights
#' @usage portfolioStdev(frameReturns, c(0.03,0.04,-0.04), c(0.2,0.5,0.3)))
portfolioStdev <- function(companiesReturns,portfolioReturns, weightsArray){

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
