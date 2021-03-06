#' @title View returns on daily basis
#' @author Bartol Freškura
#' @description Function returns data frame containing returns from past
#' on the daily, weekly or monthly basis. Returns are NOT in PERCENTAGES!!!
#' @param pricesHistoric Data frame with historic prices. Data should be in
#' form of Date, Open High, Low, Close, Volume, Adjusted.Close
#' @return data frame containing date and return.

getReturns <- function(pricesHistoric, frequency="M") {
      returns <- numeric()


      if(frequency=='D'){

            for(i in 2:(nrow(pricesHistoric)-1)) {
                  change <- as.numeric(as.character((pricesHistoric$Adjusted.Close[i+1]/pricesHistoric$Adjusted.Close[i]) - 1))
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
      else if(frequency=='W'){
            for(i in seq(from=1, to=nrow(pricesHistoric), by=5)) {
                  #check if there are sufficient number of entries
                  if((i+5) <= nrow(pricesHistoric)){
                        change <- as.numeric(as.character((pricesHistoric$Adjusted.Close[i+5]/pricesHistoric$Adjusted.Close[i]) - 1))
                        returns <- c(returns, change)
                  }
            }
            colNames = c('Return')
            frame <- data.frame(as.numeric(returns))
            colnames(frame) <- colNames

            return (frame)
      }
      else if(frequency=='M'){
            for(i in seq(from=1, to=nrow(pricesHistoric), by=22)) {
                  #check if there are sufficient number of entries
                  if((i+22) <= nrow(pricesHistoric)){
                        change <- as.numeric(as.character((pricesHistoric$Adjusted.Close[i+22]/pricesHistoric$Adjusted.Close[i]) - 1))
                        returns <- c(returns, change)
                  }
            }
            colNames = c('Return')
            frame <- data.frame(as.numeric(returns))
            colnames(frame) <- colNames

            return (frame)
      }

}


#' @title Calculate beta
#' @author Bartol Freškura
#' @description Calculates beta coefficient for the given company when
#' compared with given index.
#'  Argument data frames MUST have equal number of entries (rows).
#' @param companyReturns Data frame containing returns for the company.
#' @param indexReturns Data frame containing returns for the index.
#' @return data frame with beta index
#' @usage getBeta(TeslaReturns, S&P500_Returns)

getBeta <- function(companyReturns, indexReturns) {
      covariance <- cov(companyReturns$Return,indexReturns$Return)
      varianceInd <- var(indexReturns$Return)

      beta <- covariance/varianceInd

      frame <- data.frame(beta)
      colnames(frame) <- c('Beta')
      frame
}


#' @title Calculate mean and variance
#' @author Bartol Freškura
#' @description Calculates mean and the variance of the returns
#' @param companyReturns Vector containing returns for the company.
#' @return data frame with mean, standard deviation and variance
#' @usage getReturnsMeanVar(AppleReturns)
getReturnsMeanVar <- function(companyReturns){
      sd <- sd(companyReturns)
      mean <- mean(companyReturns)
      var <- var(companyReturns)

      frame <- data.frame(mean,sd,var)
      colnames(frame) <- c('Mean', 'Stdev', "Variance")
      frame
}
