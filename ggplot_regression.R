library(UsingR);
data(placed.df);
library(reshape2);
long <- melt(placed.df,  na.rm=TRUE);
g <- ggplot(long, aes(x=value, fill=variable));
g <- g + geom_histogram(colour ="cyan", binwidth = 1)
g <- g + facet_grid(.~variable)

myHist <- function(){
  mse <- mean((placed.df$Salary - mu)^2)
  g <- ggplot(placed.df, aes(x=Salary)) + geom_histogram(fill="salmon", 
                                                         colour="black", 
                                                         binwidth = 1)
  g <- g + geom_vline(xintercept = mu, size=3)
  g <- g + ggtitle(paste("mu =",mu,",MSE =", round(mse, 2), sep = ""))
  
}

g <- ggplot(placed.df, aes(x=Salary)) + geom_histogram(fill="salmon", 
                                                       colour="black", 
                                                       binwidth = 1)
g <- g + geom_vline(xintercept = mean(placed.df$Salary), size = 3)
g