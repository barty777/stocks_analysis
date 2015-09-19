
## THIS FUNCTION IS OBSOLETE!!!!
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
            frame <- expand.grid(rep(list(increments),each=stockNumber), Var1+Var2+Var3 <=1)
            frameNew <- data.frame()
            subset(frame, subset = (rowSums(frame)<=1) & (rowSums(frame)>=0.97))
      }
}





#' @name Load weights
#' @author Bartol Freškura
#' @description Loads weights from .mat file and converts to data frame type.
#' .mat files are in format of "Xxxx_y" where X represents number before the
#' decimal point, and x-es represent the numbers after the decimal point.
#' y represents lenght of the each row. E.g: 0020_6 means that function
#' will return a data frame with row lenght of 6 and the increment between
#' ratios in row of 0.020.
#' @param name Name of the file
#' @param path Path to the folder where .mat files reside. Default folder
#' is your working directory
#' @return data frame with all the weights
#' @usage loadMatlabWeights("0020_6","/home/username/Documents/MatFilesFolder/"), loadMatlabWeights("/0040_8")
loadMatlabWeights <- function(name, folderPath=getwd()){
      pathname <- file.path(paste(folderPath, name,".mat", sep = ''))
      data <- readMat(pathname)
      data <- do.call(rbind.data.frame, data)
      data
}


