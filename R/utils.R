## Util functions used by the Recommender system
## Coding style : https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml 

if (VERBOSE)
    print("Loading Utility functions")

# Utility to read file
ReadCSVFile <- function(file.name) {
    file.data <- read.csv(file=file.path(dataDir, file.name))
    
    # We know that the first column is userid. Make that as the rowname
    rownames(file.data) <- file.data[,1]
    
    # Remove first column from the data since that is not part of the data
    file.data <- file.data[,-1]
    
    return(file.data)
}

# Helper function to calculate the cosine similarity between two vectors
GetCosineDist <- function(x,y) {
    cosine.dist <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(cosine.dist)
}

# Helper to compute cosine similartiy b/w one record and a list of records
# This also sorts the similarity and returns k top values
GetCosineDistVecTopK <- function(input.rec, trng.rec, k) {
    # Create a vector of same length as number of records
    dist.vec <- vector(length = nrow(trng.rec))
    names(dist.vec) <- rownames(trng.rec)
    
    # Loop through the records and compute cosine similarity
    for(i in 1:nrow(trng.rec)) {
        dist.vec[i] <- GetCosineDist(input.rec, trng.with.ratings[i,]) 
    }
    
    # Sort the similarity scores
    dist.vec <- sort(dist.vec, decreasing = TRUE)
    
    # Filter top k values
    if(length(dist.vec) > nrow(trng.rec)) {
        dist.vec <- dist.vec[1:k] 
    }
    
    return(dist.vec)
}

# Helper function to calculate the ratings
GetRating <- function(history, similarities) {
    x <- sum(history*similarities) / sum(similarities)
    return(x)
}
