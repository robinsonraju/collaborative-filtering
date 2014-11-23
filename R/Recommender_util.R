## Util functions used by the Recommender system
## Coding style : https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml 

if (VERBOSE)
    print("Loading Utility functions")

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
# > x <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
# > y <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)
# > GetCosineDist(x,y)
# [1] 0.989567
GetCosineDist <- function(x,y) {
    sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
}

# Helper to compute cosine similartiy b/w one record and a list of records
# This also sorts the similarity and returns k top values
GetCosineDistVecTopK <- function(input.rec, trng.recs, k) {
    # Create a vector of same length as number of records
    dist.vec <- vector(length = nrow(trng.recs))
    names(dist.vec) <- rownames(trng.recs)
    
    # Loop through the records and compute cosine similarity
    for(i in 1:nrow(trng.recs)) {
        dist.vec[i] <- GetCosineDist(input.rec, trng.recs[i,]) 
    }
    
    # Sort the similarity scores
    dist.vec <- sort(dist.vec, decreasing = TRUE)
    
    # Filter top k values
    if(length(dist.vec) > nrow(trng.recs)) {
        dist.vec <- dist.vec[1:k] 
    }
    
    return(dist.vec)
}

## Helper function to calculate RMSE ## 
# Input : 2 vectors 'actual', 'predicted'
# Output : RMSE 
# > actual <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
# > predicted <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)
# > rmse (actual, predicted)
# [1] 1.183216
ComputeRMSE <- function(actual, predicted) {
    error <- actual - predicted    
    sqrt(mean(error^2))
}

# Comparison with Recommenderlab package
Compare.with.recommenderlab <- function() {
    r <- Recommender(Jester5k[1:1000], method = "UBCF")
    recom <- predict(r, Jester5k[1001], n=10)
    print(as(recom, "list"))
}
