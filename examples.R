install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('pairs_trading_lib.R')

#Extract stock data for Verizon and AT%T from 2008-2018
sodas <- downloadStockPairDF("KO", "PEP", 2015)
stocks <- downloadStockPairDF("VZ", "T", 2008)
head(stocks)

#correlation of 2 stocks
cor(stocks$stock1, stocks$stock2)

#Plot of stock prices over time
plotStocks(sodas)
plotStocks(stocks)

#Plot of stock ratio and mean/sd
plotRatio(sodas)
plotRatio(stocks)

ratio <- stocks$ratio
m <- mean(ratio)
s <- sd(ratio)

#find all opening/closing positions
pos <- findPositions(1,ratio,m,s)
head(pos)

addPositions(ratio,pos,m,s)

#find profits from pairs trading
profits <- positionProfit(pos,stocks)
profits

#find optimal k for pairs trading on VZ and T stocks
optK <- findOptimalK(stocks,plot=TRUE)
optK
pos <- findPositions(optK,ratio,m,s)
pos
addPositions(ratio,pos,m,s,optK)
positionProfit(pos,stocks)

#Evaluate pairs trading strategy on different stock pairs
evaluatePairsTrading(stocks, plot = TRUE)

testStockPair("KO", "PEP", 2000, 10, TRUE)

testStockPair("XOM", "CVX", 2002, 10, TRUE)

testStockPair("GOOGL", "AMZN", 2010, 5, TRUE)

testStockPair("GM", "BBBY", 2012, 5, TRUE)

testStockPair("CVX", "UAL", 2010, 5, TRUE)

stocks <- simulateStockPair(rho = 0.9, psi = 0.5)
dim(stocks)
head(stocks)
plotStocks(stocks)
plotRatio(stocks)

## correlation between stock prices
cor(stocks$stock1, stocks$stock2)

## now, each stock with a temporal trend
stocks <- simulateStockPair(rho = 0.9, psi = 0.5, sigma2 = 2, b1 = 0.01, b2 = 0.02, plot = TRUE)
plotRatio(stocks)
evaluatePairsTrading(stocks, plot = TRUE)

## correlation between stock prices
cor(stocks$stock1, stocks$stock2)

#mean profit of a simulated stock pair
dist <- simulateDistribution(rho = 0.9, psi = 0.9)
meanProfit <- mean(dist)
profitSE <- sd(dist) / sqrt(length(dist))
profitUpper <- meanProfit + 1.96 * profitSE
profitLower <- meanProfit - 1.96 * profitSE

#mean correalation of a simulated stock pair
corDist <- simulateDistribution(returnCorrelation = TRUE, rho = 0.9, psi = 0.9)
meanCor <- mean(corDist)
corSE <- sd(corDist) / sqrt(length(corDist))
corUpper <- meanCor + 1.96 * corSE
corLower <- meanCor - 1.96 * corSE

## look at the distribution of profits
summary(dist)
hist(dist)

#initialize combinations of rho and psi
rhos <- seq(0,1.00, by = 0.2)
psis <- seq(-1.00,1.00, by = 0.2)
rhopsi <- expand.grid(x = rhos, y = psis)
dim(rhopsi)

#find mean correlation distribution wrt to rho and psi
rhopsi$meanCor <- double(dim(rhopsi)[1])

for (i in 1:(dim(rhopsi)[1])) {
  rhopsi$meanCor[i] <- mean(simulateDistribution(returnCorrelation = TRUE, rho = rhopsi$x[i], psi = rhopsi$y[i]))
}

ggplot(rhopsi, aes(x = x, y = y, fill = meanCor)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")

#combinations of rho and psi
rhopsi2 <- expand.grid(x = rhos, y = psis)
colnames(rhopsi2) <- c("rho2","psi2")
dim(rhopsi2)

#find median profit distribution wrt to rho and psi
rhopsi2$medianProfit <- double(dim(rhopsi2)[1])

for (i in 1:(dim(rhopsi2)[1])) {
  rhopsi2$medianProfit[i] <- median(simulateDistribution(nrep = 10, rho = rhopsi2$rho2[i], psi = rhopsi2$psi2[i]))
}

ggplot(rhopsi2, aes(x = rho2, y = psi2, fill = medianProfit)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")

#combinations of b2 and b2
b1 <- seq(-2.00,2.00, by = 0.5)
b2 <- seq(-2.00,2.00, by = 0.5)
b1b2 <- expand.grid(x = b1, y = b2)
colnames(b1b2) <- c("b1","b2")
dim(b1b2)

#find mean correlation distribution wrt to b1 and b2
b1b2$medianProfit <- double(dim(b1b2)[1])

for (i in 1:(dim(b1b2)[1])) {
  b1b2$medianProfit[i] <- median(simulateDistribution(nrep = 10, b1 = b1b2$b1[i], b2 = b1b2$b2[i]))
}

ggplot(b1b2, aes(x = b1, y = b2, fill = medianProfit)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")