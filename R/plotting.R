#' @name Plot Average Return and Standard Deviation
#' @author Bartol Freškura
#' @description Plots a graph with the standard deviation on the x axis and the average return on
#' the y axis.
#' @param returnsFrame Data frame containing returns for stocks in every column
#' @example
plotReturnStdev <- function(returnsFrame, companies) {
      returnsVec <- numeric()
      stdevVec <- numeric()

      for(i in 1:ncol(returnsFrame)){
            tempFrame <- getReturnsMeanVar(returnsFrame[,i])
            returnsVec <- c(returnsVec, tempFrame$Mean*100)
            stdevVec <- c(stdevVec, tempFrame$Stdev*100)
      }


      plotFrame <- data.frame(returnsVec,stdevVec)
      colnames(plotFrame) <- c("Return", "Stdev")
      rownames(plotFrame) <-  companies
      #Plotting
      plot <- ggplot(data = plotFrame, aes(x = Stdev, y =Return, label=companies)) +
            coord_cartesian(xlim = c(min(plotFrame$Stdev - plotFrame$Stdev/5), max(plotFrame$Stdev + plotFrame$Stdev/5)),
                            ylim= c(min(plotFrame$Return - plotFrame$Return/5), max(plotFrame$Return+ plotFrame$Return/5)))
      plot <- plot + geom_point() + geom_text(aes(label=companies),hjust=-0.3, vjust=-0.5)
      plot <- plot + labs(x="Standard Deviation [%]", y="Mean Return [%]")
      plot
}
