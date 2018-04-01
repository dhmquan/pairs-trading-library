#A collection of functions useful for pairs trading algorithm
#Quan Do - 2018

#@pre: stock name, start year, number of years
#@post: return stock data of a particular time range
downloadPriceDF <- function(stock, start = 2010, nyears = 1) {
  require(BatchGetSymbols)
  startdate <- paste(start, '01', '01', sep = '-')
  enddate <- paste(start+nyears, '01', '01', sep = '-')
  out <- suppressMessages(BatchGetSymbols(tickers = stock, first.date = startdate, last.date = enddate))
  cat('\n')
  if(out$df.control$download.status != 'OK') stop(paste0('something went wrong downloading ', stock, 'prices'))
  stockDF <- data.frame(date = as.character(out$df.tickers$ref.date),
                        price = out$df.tickers$price.adjusted)
  return(stockDF)
}

#@pre: 2 stock names, start year, time range
##post: a data.frame of 2 stock values and their ratio, ordered by date
downloadStockPairDF <- function(stock1,stock2,start = 2010,nyears=1) {
  library(dplyr)
  firstStock <- downloadPriceDF(stock1,start=start,nyears=nyears)
  secondStock <- downloadPriceDF(stock2,start=start,nyears=nyears)
  
  if (!identical(firstStock$date,secondStock$date)) stop("Dates are not identical")
  
  pair <- inner_join(firstStock,secondStock,by="date")
  names(pair) <- c("date","stock1","stock2")
  pair %>% mutate(ratio = stock1/stock2) -> pair
  return(pair)
}

#@pre: a data.frame of 2 stock prices
#@post: return a plot of prices over time
plotStocks <- function(stocksDF) {
  library(ggplot2)
  i_range = 1:nrow(stocksDF)
  plt <- ggplot() +
    geom_line(data = stocksDF,aes(x=i_range,y=stock1, color = "red")) +
    geom_line(data = stocksDF,aes(x=i_range,y=stock2, color = "blue")) +
    ggtitle("Stock Prices over Time") +
    xlab("Time") + ylab("Stock Price in USD") +
    scale_color_discrete(name = "Stocks", labels = c("stock 2", "stock 1"))
  plt
}

#@pre: data.frame of stock pair
#@post: return a plot of stock ratio over time, as well as its mean/sd
plotRatio <- function(stocksDF,k=1) {
  library(ggplot2)
  i_range = 1:nrow(stocksDF)
  ratio <- stocksDF$ratio
  m <- mean(ratio)
  s <- sd(ratio)
  ggplot() +
    geom_line(data = stocksDF,aes(x=i_range,y=ratio, color = "blue")) +
    geom_line(data = stocksDF,aes(x=i_range,y=m, color = "red")) +
    geom_line(data = stocksDF,aes(x=i_range,y=m+k*s)) +
    geom_line(data = stocksDF,aes(x=i_range,y=m-k*s)) +
    ggtitle("Stock Ratio over Time") +
    xlab("Time") + ylab("Stock Price in USD") +
    scale_color_discrete(name = "Legend", labels = c("Stock Ratio", "Ratio Mean"))
}

#@pre: k, ratio of test set, mean and sd of training set
#@post: list of triplets - opening/closing days for each trading position,
# and whether we're buying stock 1 / selling stock 2, or the opposite
findPositions <- function(k=1,ratio,m,s) {
  positions <- list(c())
  l <- length(ratio)
  n <- 1
  
  high <- m + s*k
  low <- m - s*k
  
  if (any(ratio > high | ratio < low)) {
    opens <- which(ratio > high | ratio < low)
    
    while (length(opens) != 0) {
      i = opens[1]
      
      if (ratio[i] > high) {
        if (any(ratio[i:l] < m)) {
          close <- i + which(ratio[i:l] < m)[1] - 1
        } else {
          close <- l
        }
        dir <- 1
        
      } else if (ratio[i] < low) {
        if (any(ratio[i:l] > m)) {
          close <- i + which(ratio[i:l] > m)[1] - 1
        } else {
          close <- l
        }
        dir <- -1
      }
      positions[[n]] <- c(i,close,dir)
      n <- n + 1
      opens <- opens[which(opens > close)]
    }
  }
  return(positions)
}

#@pre: a vector of stock ratio, and a list of open/close positions
#@post: return a plot of stock ratio and all points of open/close positions
addPositions <- function(ratio, positions, m, s, k=1) {
  i_range <- 1:length(ratio)
  
  openPos <- double(length(positions))
  openRatios <- double(length(positions))
  closePos <- double(length(positions))
  closeRatios <- double(length(positions))
  
  for (i in 1:length(positions)) {
    openPos[i] <- positions[[i]][1]
    openRatios[i] <- ratio[positions[[i]][1]]
    closePos[i] <- positions[[i]][2]
    closeRatios[i] <- ratio[positions[[i]][2]]
  }
  
  plt <- ggplot() +
    geom_line(aes(x=i_range,y=ratio)) +
    geom_line(aes(x=i_range,y=m)) +
    geom_line(aes(x=i_range,y=m+k*s)) +
    geom_line(aes(x=i_range,y=m-k*s)) +
    ggtitle("Stock Ratio over Time") +
    xlab("Time") + ylab("Stock Price in USD") +
    geom_point(aes(x=openPos,y=openRatios,color="Open Positions")) +
    geom_point(aes(x=closePos,y=closeRatios,color="Close Positions")) +
    scale_color_discrete(name = "Positions")
  
  plt
}

#@pre: 1 trading position, stock pair data.frame
#@post: return gain/loss from one 1 trading position
findShare <- function(position,stocksDF) {
  shares1 <- 1/stocksDF$stock1[position[1]]
  shares2 <- 1/stocksDF$stock2[position[1]]
  
  if (position[3] == 1) {
    profit1 <- -1 * shares1 * stocksDF$stock1[position[2]]
    profit2 <- shares2 * stocksDF$stock2[position[2]]
  } else {
    profit1 <- shares1 * stocksDF$stock1[position[2]]
    profit2 <- -1 * shares2 * stocksDF$stock2[position[2]]
  }
  
  fees <- 0.003 * (1 + 1 + abs(profit1) + abs(profit2))
  return(profit1 + profit2 - fees)
}

#@pre: list of positions, stock pair data.frame, boolean var for net
#@post: if net = TRUE, return percentage profit from all positions,
# if net = FALSE, return list of profits for each position pair
positionProfit <- function(positions, stocksDF, net = TRUE) {
  if (is.null(positions[[1]])) {
    return(0)
  }
  
  shares <- sapply(positions,findShare,stocksDF = stocksDF)
  if (net) {
    return(sum(shares))
  } else
    return(shares)
}

#@pre: stock pair data.frame, plot boolean
#@post: return k value for optimal pairs trading profit.
# if plot = TRUE, return also a plot of net profit over k.
findOptimalK <- function(stocksDF,plot = FALSE) {
  ratio <- stocksDF$ratio
  m <- mean(ratio)
  s <- sd(ratio)
  
  kmax = max(abs(ratio - m))/s
  kvalues <- seq(0, kmax, length = 100)
  positionList <- lapply(kvalues,findPositions,ratio=ratio,m=m,s=s)
  
  profits <- sapply(positionList, positionProfit,stocksDF=stocksDF)
  k <- kvalues[which(profits == max(profits))[1]]
  
  if (plot) {
    plt <- ggplot() +
      geom_line(aes(x=kvalues,y=profits)) +
      ggtitle("Net Profit over k Value") +
      xlab("k") + ylab("Net Profit Percentage") +
      geom_point(aes(x=k,y=max(profits),color="Optimal k")) +
      scale_color_discrete(name = "")
    print(plt)
    
  }
  
  return(k)
}

#@pre: stock pair data.frame, training fraction, plot boolean.
#@post: split data.frame into a training set and test set.
# Train pairs trading on training set, then test for profit on test set.
evaluatePairsTrading <- function(stocksDF,trainingFrac = 0.5, plot = FALSE) {
  trainingCount <- nrow(stocksDF) * trainingFrac
  trainingSet <- head(stocksDF,trainingCount)
  testSet <- tail(stocksDF, nrow(stocksDF) - trainingCount)
  
  k <- findOptimalK(trainingSet)
  
  ratio <- testSet$ratio
  m <- mean(trainingSet$ratio)
  s <- sd(trainingSet$ratio)
  
  pos <- findPositions(k,ratio,m,s)
  #browser()
  if (plot) {
    print(addPositions(testSet$ratio,pos,m,s,k))
  }
  
  profits <- positionProfit(pos,testSet)
  profits
}

#@post: Simulate stock pairs using a first-order VAR(1) model
#x_1,x_2 for t = 2...n
#y_1,y_2 for t = 1...n
#y_1,t = a_1 + b_1 * t + x_1,t
#y_2,t = a_2 + b_2 * t + x_2,t
simulateStockPair <- function(n=1000,sigma1=1,sigma2=1,rho=1,psi=0,b1=0,b2=0,plot=FALSE) {
  data <- matrix(ncol=3,nrow=n)
  
  x1 <- double(n)
  x2 <- double(n)
  x1[1] <- 20
  x2[1] <- 30
  data[1,1] <- 20 + b1 + x1[1]
  data[1,2] <- 30 + b2 + x2[1]
  data[1,3] <- data[1,1] / data[1,2]
  
  for (i in 2:n){
    x1[i] <- rho * x1[i-1] + (1 - rho) * psi * x2[i-1] + rnorm(1,0,sigma1)
    x2[i] <- rho * x2[i-1] + (1 - rho) * psi * x1[i-1] + rnorm(1,0,sigma2)
    
    data[i,1] <- 20 + b1 * i + x1[i]
    data[i,2] <- 30 + b2 * i + x2[i]
    data[i,3] <- data[i,1] / data[i,2]
  }
  
  df <- data.frame(data)
  colnames(df) <- c("stock1", "stock2", "ratio")
  
  if (plot) {
    print(plotStocks(df))
  }
  
  df
}

#@post: return profit and plot of applying pairs trading on the given stock pair
testStockPair <- function(stock1, stock2, start = 2010, nyears = 5, plot = FALSE) {
  stocksDF <- downloadStockPairDF(stock1, stock2, start = start, nyears = nyears)
  print(cor(stocksDF$stock1,stocksDF$stock2))
  profit <- evaluatePairsTrading(stocksDF, plot = plot)
  profit
}

#@post: return a distribution of profits made from a simulated stock pair.
# if returnCorrelation == TRUE, return a distribution of correlations instead
simulateDistribution <- function(nrep = 100, returnCorrelation = FALSE, ...) {
  reps <- double(nrep)
  
  if (!returnCorrelation) {
    for (i in 1:nrep) {
      stocks <- simulateStockPair(...)
      reps[i] <- evaluatePairsTrading(stocks)
    }
  } else {
    for (i in 1:nrep) {
      stocks <- simulateStockPair(...)
      reps[i] <- cor(stocks$stock1,stocks$stock2)
    }
  }
  
  return(reps)
}