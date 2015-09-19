#' @title View returns on daily basis
#' @author Bartol Freškura
#' @description Function returns data frame containing returns from past
#' on the daily basis. Returns are NOT in PERCENTAGES!!!
#' @param pricesHistoric Data frame with historic prices. Data should be in
#' form of Date, Open High, Low, Close, Volume
#' @return data frame containing date and return.
#'
getReturns <- function(pricesHistoric) {
      returns <- numeric()
      #Start from second row because return for first price can't be
      #calculated

      for(i in 2:(nrow(pricesHistoric)-1)) {
            change <- as.numeric(as.character((pricesHistoric$Close[i+1]/pricesHistoric$Close[i]) - 1))
            returns <- c(returns, change)
      }
      #Remove first and last dates
      dates <- pricesHistoric$Date
      dates <- dates[2:length(dates)]
      dates <- dates[1:length(dates)-1]

      colNames = c('Date', 'Return')
      frame <- data.frame(dates,as.numeric(returns))
      colnames(frame) <- colNames

      return (frame)
}


#' @title Calculate beta, mean and variance
#' @author Bartol Freškura
#' @description Calculates beta coefficient for the given company when
#' compared with given index. Also calculates mean and the variance of the returns
#'  Argument data frames MUST have equal number of entries (rows).
#' @param companyReturns Data frame containing returns for the company.
#' @param indexReturns Data frame containing returns for the index.
#' @return data frame with mean, variance and beta
#' @usage getBetaMeanVar(TeslaReturns, S&P500_Returns)

getBetaMeanVar <- function(companyReturns, indexReturns) {
      covariance <- cov(companyReturns$Return,indexReturns$Return)
      varianceInd <- var(indexReturns$Return)
      
      beta <- covariance/varianceInd
      var <- var(companyReturns$Return)
      mean <- mean(companyReturns$Return)
      
      frame <- data.frame(mean, var, beta)
      colnames(frame) <- c('Mean', 'Variance', 'Beta')
      frame
}


