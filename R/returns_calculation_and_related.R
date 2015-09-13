#' @title View returns on daily basis
#' @description Function returns data frame containing returns from past
#' on the daily basis. Returns are NOT in PERCENTAGES!!!
#' @param pricesHistoric Data frame with historic prices. Data should be in
#' form of Date, Open High, Low, Close, Volume
#'
getReturns <- function(pricesHistoric, frequency="monthly") {
      returns <- numeric()


      if(frequency=='daily'){
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
      else{

      }

}
