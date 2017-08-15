---
#title: "Coursera's Statistical Inference Final Project Part - I, Simulation Exercise"
#author: "Jorge Mendoza"
#date: "August 11, 2017"
#output: html_document
---

##### - Overview

###### On this report we will investigate the exponential distribution using R and make theorical comparasions with the Central Limit Theorem or CLT.

###### In order to simulate the exponential distribution using R we will run the function `rexp(n, lambda)`

set.seed(1234)

library(ggplot2)

lambda <- 0.2
n <- 40
sim <- 1000
expSampleMeans <- NULL

for (i in 1:sim){
  expSampleMeans <- c(expSampleMeans, mean(rexp(n, lambda)))
}

sampleMean <- mean(expSampleMeans)
sampleStandar_deviation <- sd(expSampleMeans)
sampleVariance <- var(expSampleMeans)

hist(expSampleMeans, col = "blue")

var <- (1 / lambda)^2 / (n) 
mu <- 1/lambda

data <- as.data.frame(expSampleMeans)
plot <- ggplot(data, aes(x = expSampleMeans));
plot <- plot + geom_histogram(binwidth = 0.4, color = 'blue', fill = 'white', aes(y = ..density..)) 
plot <- plot + stat_function(fun = dnorm, color = 'black', args = list(mean = 5, sd = sqrt(0.625))) 
plot <- plot + geom_vline(xintercept = mean(expSampleMeans), color="blue");
plot <- plot + geom_vline(xintercept = mu, color="red");
plot <- plot + labs(colour = "Blue")
plot <- plot + labs(x = "Sample Means")
plot <- plot + labs(title = "Theorical Distribution Vs. Sample Distribution")
plot
