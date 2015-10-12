# Stock Analysis package
Author: Bartol Freškura
  
### Description
Package for extracting various information about stocks (historical prices, dividends, various ratios),
creating optimal portfolios and plotting graphs that relate to stocks.
  
The main purpose of this package is to have all information and tools for analyzing stock in one place,
so it can shorten the time used for gathering data and making calculations.
  
  
### Required packages
Before running the code it is neccessary to install and load several packages for R.

1. **ggplot2** - for plotting
2. **quantmod** - for downloading stocks data from multiple web sources
3. **foreach** and **doParallel** - multi core processing related libraries because some computations are
really complex and by default R does not use multiple cores
4. **R.matlab** - package used for reading .mat files

```{r}
library(R.matlab)
library(quantmod)
library(foreach)
library(doParallel)
library(ggplot2)
```

### Example of usage

#### Example 1: Retrieve basic stock data
Lets first create the string containing tickers for companies we are interested in. We can also
load tickers for several stock indexes. webSource variable is used by quantmod package and it 
specifies the web source for downloading data. For more information read the quantmod documentation.

```{r}
webSource = "yahoo"
tickers = "WMT;AAPL;TSLA;"

#Indexes tickers: S&P 500, Russell 3000, Russell 2000, Dow Jones Industrial Average
indexTickerYahoo = c('^GSPC', '^RUA', '^RUT', '^DJI')
```
  
First step is to use the quantmod package to download financial data into list variables that are stored 
in R as **ticker.f**. This will create seperate .f files for every company we have in our tickers
string.
getFinancials is a function from quantmod package so if you want to know more about it read the docs.

```{r}
getFinancials(Symbol = tickers, src = webSource , auto.assign = TRUE)
```

Now that we have the required data, we are going to extract some useful information from it.
For example, lets see the cashflow statement, balance sheet and income statement for Apple.

```{r}
balanceSheet <- getBalanceSheet(AAPL.f, periodFreq = "A")
incomeState <- getIncomeStatement(AAPL.f, periodFreq = "A")
cashFlow <- getCashFlow(AAPL.f, periodFreq = "A")
```
**NOTE:** The quantmod package is loading data from yahoo finance and it will only retrieve data for the last
4 periods. The same applies for google finance data.

  
  
Historical prices are one of the most frequent type of information used for analysing stocks. 
It's time to get that data:

```{r}
prices3M <- getHistoricPrices("MMM",srce = 'yahoo', timeSpan=4)
```
This will get us the historic prices for 3M from last 4 years on a daily basis. It will also provide
a date and the volume on the particular day.
  
  
We can also download dividends with the dates when they were issued

```{r}
dividends3M <- getHistoricDividends("MMM", srce = 'yahoo', timeSpan = 4)
```
  
If we are only interested in the summary of the stock we can use the function getSummary() which
will get us some basic information.
```{r}
summary3M <- getSummary("MMM")
```
  
  
#### Example 2: Creating optimized portfolio
Suppose we have picked our stock and now we have 10,000$ to invest in them. How do we know how much
money should we invest in each of these stocks?
This example is made for solving this dilemma.
  
First we are going to load some data to work with:

```{r}
##Path to .mat folder
path <- paste(getwd(),"/Matlab",sep = "")

##3 stocks
weights <- loadMatlabWeights("/0020_3",path)
weightsMatrix <- as.matrix(weights)

#Load companies
companies <- c('JPM','MMM','PG')


##Load company returns
for(i in 1:length(companies)){

      companyPrices <- getHistoricPrices(company = companies[i],timeSpan = 3)
      companyReturns <- getReturns(companyPrices, frequency = 'M')
      if(i==1){
            returnsDataFrame <- data.frame(companyReturns$Return)
      }
      else{
            returnsDataFrame <-  data.frame(returnsDataFram,companyReturns$Return)
      }

}
#set column names for data frame
colnames(returnsDataFram) <- companies
```
What I have done here is:

1. Set path to he directory where my .mat files reside
2. Load the .mat into the R data type and then convert that to matrix type *(Please read the
documentation for loadMatlabWeights)*
3. Pick companies for analysis
4. For every company download historic prices and then using the function getReturns calculate
5. Set column names for the new data frame so we have a nicer formating
  
```{r}
##calculate with multiple CPU Cores
cl <- makeCluster(8)
registerDoParallel(cl)

returnsMean <- numeric()
returnsStdev <- numeric()

size <- nrow(weights)
strt<-Sys.time()
cat("Calculation started. This make take a while...")

returnsMeanArray <- portfolioAvgReturnArray(companiesReturns = returnsDataFram)

#Calculate for every possible weight combination
finalResult <- foreach(i=1:size) %dopar% {

      returnsMean <- portfolioAvgReturn(companiesReturns  = returnsDataFram,weightsArray = weights[i,])
      returnsStdev <- portfolioStdev(companiesReturns = returnsDataFram, portfolioReturns = returnsMeanArray, weightsArray =  weightsMatrix[i,])
      ratio <- returnsMean/returnsStdev
      temp <- c(returnsMean,returnsStdev,ratio, weightsMatrix[i,])

}
stopCluster(cl)
print(Sys.time()-strt)


#Find combination with     the maximum avgReturn/Stdev ratio
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

```
Okay, that's alot of code. Time to break it down.
First 2 lines are used for preparing the *doParallel* package paramaters. Number 8 in the numberCluster
argument represents number of cores in your PC. Set it to the appropriate number. For more info take 
a look at the doParallel documentation.
  
Now comes the vector initialization. *returnsMean* will contain average returns for every company,
*returnsStdev* standard deviations for every company. Lines with *Sys.time* serve only to print the 
running time of the algorithm, they are not necessary but it is nice to see time elapsed :)
We store the mean return for every company in the *returnsMeanArray* variable because we are goin to 
need it later
  
This is the core part of the code. Using a *foreach* loop (because it supports multi core processing),
we iterate through every possible weight combination and calulate the corresponding *average return*
for the portfolio and *standard deviation* for the portfolio. Then we take the ratio between the two and store
it into the temp variable along with avgReturn,stdev and weight vector. On the end of the every iteration,
temp vector is stored into the *finalResult* variable which is of type list. 
  
After we have stored the results in a variable, now we must find for which weight we get the maximum
avgReturn/Stdev ratio. *formatReturnResult* is used for prettier formating of the final result.

**WARNING: This algorithm can take minutes or even hours to execute. Worst case scenario is that your 
PC freezes, so PLEASE SAVE ANY SENSITIVE WORK BEFORE RUNNING THIS. For number of stocks lower or equal 
to 4 it probably won't take long to execute (few minutes max) but be extra careful when using it with 
greater number than 4.**
  
  

We can also plot the Standard Deviation / Average Returns graph  so we can se how our stocks perform
in terms of profit and risk.

```{r}
plotReturnStdev(returnsDataFrame, companies)
```
