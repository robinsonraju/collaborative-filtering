## Function to determine values for NA in the input ##
# Input : input.data (vector), trng.data (data frame), 
#           k (number of similar records to be compared with), 
# Output : vector with NA columns populated with values. 
# Approach : 
#   non.na.cols = Find which columns in the input have ratings
#   similar.recs =  All records in trng.data that have valid values for non.na.cols
#   knn.recs = Calculate distance from input.data to all the recs in similar.recs, return top k
#   For each na.col in na.cols
#       value of na.col = average of the value of the knn_records for the same column.
GetRecommendedRatings <- function(input.data, trng.data, k=10) {
    output.with.ratings <- input.data 
    
    # Find which columns have ratings and which don't.
    if (VERBOSE) print("Finding out which columns in input data are NAs")
    input.data.na = input.data[,is.na(input.data)]
    input.with.ratings = input.data[,!is.na(input.data)]
    
    # Get all records that have valid values for columns in input that have ratings
    if (VERBOSE) print("Getting all records in trng that have valid values in the same columns as input")
    trng.with.ratings <- na.omit(trng.data[,colnames(input.with.ratings)])
    
    # Calculate distance from input to all the recs with ratings
    # Return top 10 records
    if (VERBOSE) print("Calculating similarity and getting the top 10")
    dist.vec <- GetCosineDistVecTopK(input.with.ratings, trng.with.ratings, 10)
    closest.trng.recs <- trng.data[names(dist.vec),]
    
    # Loop through the na columns to populate them
    if (VERBOSE) print("Computing rating for each NA column in the input")
    for (i in 1:ncol(input.data.na)) {
        col.na <- colnames(input.data.na)[i]
        output.with.ratings[col.na] <- mean(closest.trng.recs[,col.na], na.rm=TRUE)
    }
    
    return (output.with.ratings)
}

## Function to get top n recommendations ## 
# Input : input.data (vector), trng.data (data frame), 
#           k (number of similar records to be compared with), 
#           n (top n recommendations to be returned)
# Output : vector with column Ids of recommended jokes. 
# Approach : 
#   output.with.ratings = vector with all NA values filled by using CF algorithm
#   Sort recommendations based on rating values in decreasing order
#   Return top n. 
Recommend <- function (input.data, trng.data, k=10, n=10) {
    input.data.na = input.data[,is.na(input.data)]
    output.with.ratings <- GetRecommendedRatings(input.data, trng.data, k)
    
    # Recommendation
    output.with.recomm <- output.with.ratings[,colnames(input.data.na)]
    if (VERBOSE){
        print("NA values in input data filled with predicted ratings")
        print(output.with.recomm)
    }
    
    top.10.recomm <- names(sort(output.with.recomm, decreasing=TRUE))[1:n]
    top.10.recomm
}