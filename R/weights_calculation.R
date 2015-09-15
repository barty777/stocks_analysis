
#' @name Calculate weights for portfolio
#' @author Bartol Freškura
#' @description Returns data frame containing all combinations of weights where
#' sum of the weigths in each row equals between 0.97 and 1.
#' WARNING: This function has a high RAM memory usage so use with caution
#' because it can easily take all your CPU and RAM resources and therfore
#' freeze your computer.
#' @param stockNumber number of stock in portfolio
#' @param increment Increment when determining weights
#' @return data frame containing all possible combinations of weights which
#' when summed equal between 0.97 and 1.
getWeights <- function(increment=0.1, stockNumber=3) {


      #Precaution because of the high complexity
      if(increment<0.02 | stockNumber>6){
            break;
      }

      ##Tested with inc=0.02 and s#= 4.
      ##Tested with inc=0.03 and s#= 5.
      else {
            increments <- seq(from = 0, to= 1, by=increment)
            frame <- expand.grid(rep(list(increments),each=stockNumber))
            frameNew <- data.frame()
            subset(frame, subset = (rowSums(frame)<=1) & (rowSums(frame)>=0.97))
      }
}


