
#' @name Calculate weights for portfolio
#' @author Bartol Freškura
#' @description Returns data frame containing all combinations of weights where
#' sum of the weigths in each row equals one.
#' @param stockNumber number of stock in portfolio
#' @param increment Increment when determining weights
#' @return 
getWeights <- function(increment=0.1, stockNumber=3) {
      
      
      #Precaution
      if(increment<0.05 || stockNumber>7){
            break;
      }     
      else {
            increments <- seq(from = 0, to= 1, by=increment)
            frame <- expand.grid(rep(list(increments),each=stockNumber))
            frameNew <- data.frame()
            subset(frame, subset = (rowSums(frame)<=1) & (rowSums(frame)>=0.97))
      }
}

 
