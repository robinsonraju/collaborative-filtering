## Util functions used by the Recommender system
## Coding style : https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml 

## Utility to read file ##
## Ensure that the file is in the working directory
# Input : filename
# Output : data frame
ReadCSVFile <- function(file.name) {
    file.data <- read.csv(file=file.path(dataDir, file.name))
    
    # We know that the first column is userid. Make that as the rowname
    rownames(file.data) <- file.data[,1]
    
    # Remove first column from the data since that is not part of the data
    file.data <- file.data[,-1]
    
    return(file.data)
}

## Helper function to calculate the cosine similarity between two vectors ##
# Input : 2 vectors 'x', 'y'
# Output : Cosine similarity  
# Example: 
# > x <- 1:10
# > y <- 11:20
# > GetCosineSim(x,y)
# [1] 0.9559123
GetCosineSim <- function(x,y) {
    sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
}

## Helper function to calculate the cosine similarity between two vectors ##
# Input : 2 vectors 'x', 'y', centered = boolean to indicate centering
# Output : Cosine similarity  
# Examples: 
# > x <- 1:10
# > y <- 11:20
# > GetCosineSimWithCentering(x, y, FALSE)
# [1] 0.9559123
# > GetCosineSimWithCentering(x, y, TRUE)
# [1] 1
GetCosineSimWithCentering <- function(x, y, centered=FALSE) {
    if(centered) {
        x.m <- mean(x, na.rm=TRUE)
        x <- as.vector(apply(as.matrix(x), 2, function(z) {(z - x.m)}))
        
        y.m <- mean(y, na.rm=TRUE)
        y <- as.vector(apply(as.matrix(y), 2, function(z) {(z - y.m)}))
    } 
    GetCosineSim(x, y)
}

# Helper to compute cosine similartiy b/w one record and a list of records
# This also sorts the similarity and returns k top values
GetCosineDistVecTopK <- function(input.rec, trng.recs, k, centered=FALSE) {
    # Create a vector of same length as number of records
    dist.vec <- vector(length = nrow(trng.recs))
    names(dist.vec) <- rownames(trng.recs)
    
    # Loop through the records and compute cosine similarity
    for(i in 1:nrow(trng.recs)) {
        input.rec <- as.vector(input.rec,mode='numeric')
        curr.rec <- as.vector(trng.recs[i,],mode='numeric')
        dist.vec[i] <- GetCosineSimWithCentering(input.rec, curr.rec, centered) 
    }
    
    # Sort the similarity scores
    dist.vec <- sort(dist.vec, decreasing = TRUE)
    
    # Filter top k values
    if(length(dist.vec) >= nrow(trng.recs)) {
        dist.vec <- dist.vec[1:k] 
    }
    
    return(dist.vec)
}

## Helper function to calculate RMSE ## 
# Input : 2 vectors 'actual', 'predicted'
# Output : RMSE 
# Example: 
# > actual <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
# > predicted <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)
# > rmse (actual, predicted)
# [1] 1.183216
ComputeRMSE <- function(actual, predicted) {
    error <- actual - predicted    
    sqrt(mean(error^2, na.rm=TRUE))
}

## Helper function to compute average ## 
# Calls weighted average function if weighted.avg boolean value is true
# Otherwise, finds mean. 
# Example:
# > sim.vec <- seq(1, 0.1, by = -0.1)
# > records <- data.frame(matrix(1:40, nrow=10))
# > colnames(records) <- c("c1", "c2", "c3", "c4")
# > Average(sim.vec, records, "c1", FALSE)
# [1] 5.5
# > Average(sim.vec, records, "c1", TRUE)
# [1] 0.4
Average <- function(sim.vec, records, avg.col, weighted.avg=FALSE) {
    avg <- 0
    if (weighted.avg) {
        avg <- WeightedAverage(sim.vec, records, avg.col)
    } else {
        avg <- mean(records[,avg.col], na.rm=TRUE)
    }
    avg
}

## Helper function to compute weighted average
# Input : sim.vec - vector with similarity scores
#           records - data frame with values
#           avg.col - column for which weighted average needs to be found
# Example:
# > sim.vec <- seq(1, 0.1, by = -0.1)
# > records <- data.frame(matrix(1:40, nrow=10))
# > colnames(records) <- c("c1", "c2", "c3", "c4")
# > WeightedAverage(sim.vec, records, "c1")
# [1] 0.4
WeightedAverage <- function(sim.vec, records, avg.col) {
    s <- sum(records[,avg.col], na.rm=TRUE)
    p <- 0

    for(i in 1:nrow(records)) {
        if(!is.na(sim.vec[i]) && !is.na(records[i, avg.col])) {
            p <- p + (sim.vec[i] * records[i, avg.col])
        }
    }
    p * 10 / s
}

# Comparison with Recommenderlab package
Compare.with.recommenderlab <- function() {
    r <- Recommender(Jester5k[1:1000], method = "UBCF")
    recom <- predict(r, Jester5k[1001], n=10)
    print(as(recom, "list"))
}
