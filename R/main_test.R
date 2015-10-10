##This file is used for testing

##Path to .mat folder
path <- paste(getwd(),"/Matlab",sep = "")

##3 stocks
weights <- loadMatlabWeights("/0020_5",path)
weightsMatrix <- as.matrix(weights)

#Load companies
#companies <- c('JPM','MMM','PG')
#companies <- c('AAPL','IBM')
companies <- c("ICUI","REGN","MMSI","ABMD","IT")
#companies <- c("ICUI","REGN","MMSI")

#Indexes tickers: S&P 500, Russell 3000, Russell 2000, Dow Jones Industrial Average
# indexTickerYahoo = c('^GSPC', '^RUA', '^RUT', '^DJI')
# indexPrices <- getHistoricPrices(company = indexTickerYahoo[1],timeSpan = 4)
# indexReturns <- getReturns(indexPrices)



##Load company returns
for(i in 1:length(companies)){

      companyPrices <- getHistoricPrices(company = companies[i],timeSpan = 7)
      companyReturns <- getReturns(companyPrices, frequency = 'M')
      if(i==1){
            returnsDataFram <- data.frame(companyReturns$Return)
      }
      else{
            returnsDataFram <-  data.frame(returnsDataFram,companyReturns$Return)
      }

}
#set column names for data frame
colnames(returnsDataFram) <- companies



##calculate single thread
# means <- numeric()
# stdevs <- numeric()
# we <- numeric()
# size <- nrow(weights)
# for(i in 1:1){
#
#       ##Print progress
#       if(i %% 50 == 0){
#             cat(paste("Progress:", i, "/", size, sep = " "))
#             cat("\n")
#       }
#
#       returnsMean <- portfolioAvgReturn(companiesReturns = returnsDataFram, weightsArray = weights[i,])
#       returnsMeanArray <- portfolioAvgReturnArray(companiesReturns = returnsDataFram)
#       returnsStdev <- portfolioStdev(companiesReturns = returnsDataFram, portfolioReturns = returnsMeanArray, weightsArray =  weightsMatrix[i,])
#
#
#       ratio <- returnsMean/returnsStdev
#       browser()
#       means <- c(means,returnsMean)
#       stdevs <- c(stdevs,returnsStdev)
#
#
# }
# frameMean <- data.frame(means, stdevs)





##calculate with multiple CPU Cores
cl <- makeCluster(8)
registerDoParallel(cl)

returnsMean <- numeric()
returnsStdev <- numeric()
weightsArray <- numeric()
size <- nrow(weights)
strt<-Sys.time()
cat("Calculation started. This make take a while...")

returnsMeanArray <- portfolioAvgReturnArray(companiesReturns = returnsDataFram)
finalResult <- foreach(i=1:size) %dopar% {

      returnsMean <- portfolioAvgReturn(companiesReturns  = returnsDataFram,weightsArray = weights[i,])
      returnsStdev <- portfolioStdev(companiesReturns = returnsDataFram, portfolioReturns = returnsMeanArray, weightsArray =  weightsMatrix[i,])
      ratio <- returnsMean/returnsStdev
      temp <- c(returnsMean,returnsStdev,ratio, weightsMatrix[i,])

}
stopCluster(cl)
print(Sys.time()-strt)


cat("Filtering Results...")
max <- finalResult[[1]][3]
id <- 1
for(i in 1:size){
      if(finalResult[[i]][3]>max){
            max <- finalResult[[i]][3]
            id <- i;
      }
}

finalFrame <- finalResult[[id]]
format <- formatReturnResult(companies = companies, finalFrame)
format

#Confidence interval
error <- qnorm(0.975)*format$Standard.Deviation/sqrt(size)
left <- format$Average.Return-error
right <- format$Average.Return+error
cat("95% conidence interval[", left," - ", right,"]")
