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
GetRecommendedRatings <- function(input.data, 
                                  trng.data, 
                                  k=10, 
                                  na.col=NA, 
                                  weighted.avg=FALSE,
                                  centered=FALSE) {
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
    dist.vec <- GetCosineDistVecTopK(input.with.ratings, trng.with.ratings, 10, centered)
    closest.trng.recs <- trng.data[names(dist.vec),]
    
    # Populate recommended value for NA attribute(s)
    if (is.na(na.col)) {
        if (VERBOSE) print("Computing rating for each NA column in the input")
        for (i in 1:ncol(input.data.na)) {
            col.na <- colnames(input.data.na)[i]
            output.with.ratings[col.na] <- Average(dist.vec, closest.trng.recs, col.na, weighted.avg)
        }
    } else {
        if (VERBOSE) print(paste("Computing rating one NA col ", na.col))
        output.with.ratings[na.col] <- Average(dist.vec, closest.trng.recs, na.col, weighted.avg)
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
Recommend <- function (input.data, trng.data, 
                       k=10, weighted.avg=FALSE, centered=FALSE, n=10) {
    input.data.na = input.data[,is.na(input.data)]
    output.with.ratings <- GetRecommendedRatings(input.data, trng.data, k, NA, weighted.avg, centered)
    
    # Recommendation
    output.with.recomm <- output.with.ratings[,colnames(input.data.na)]
    if (VERBOSE){
        print("NA values in input data filled with predicted ratings")
        print(output.with.recomm)
    }
    
    names(sort(output.with.recomm, decreasing=TRUE))[1:n]
}

trng.RMSE.long <- function (full.data, k=10, weighted.avg=FALSE, centered=FALSE) {
    start <- Sys.time()
    num.rows <- nrow(full.data) 
    
    # 90% is training, 10% is testing data
    ninety_perct <- round(0.9 * num.rows , 0)
    tr.data <- full.data[1:ninety_perct,]
    ts.data <- full.data[(ninety_perct+1):num.rows,]
    RMSE.ts.data <- apply(ts.data, 1, function(x) x<- NA)
    
    # Iterate through training records
    u.names <- rownames(ts.data)
    
    for (i in 1:nrow(ts.data)) {  
        curr.rec <- ts.data[i,]
        row.name <- u.names[i]

        curr.rec.orig.ratings <- curr.rec[,!is.na(curr.rec)]
        curr.rec.recom.ratings <- apply(curr.rec.orig.ratings, 2, function(x) x<- NA)
        
        # For each non-NA value, mark it as NA and determine recommended value
        j.names <- names(curr.rec.recom.ratings)
        for (j in 1:ncol(curr.rec.orig.ratings)) { 
            col.name <- j.names[j]
            temp <- curr.rec
            temp[col.name] <- NA
        
            rec.ratings <- GetRecommendedRatings(temp, tr.data, k, col.name, weighted.avg, centered)         
            curr.rec.recom.ratings[col.name] <- rec.ratings[col.name]
            
            if (VERBOSE) print(paste("col ",col.name,
                                     "orig value ", curr.rec.orig.ratings[col.name],
                                     " curr recommendation", curr.rec.recom.ratings[col.name]))
        }
        
        RMSE.ts.data[row.name] <- ComputeRMSE(curr.rec.orig.ratings, curr.rec.recom.ratings)
        if (VERBOSE) print(paste("row ", row.name , " rmse ", RMSE.ts.data[row.name] ))
    }
    
    end <- Sys.time()
    if (VERBOSE) print(end - start)
    
    RMSE.ts.data
}

trng.RMSE.short <- function (full.data, k=10, weighted.avg=FALSE, centered=FALSE) {
    start <- Sys.time()
    num.rows <- nrow(full.data) 
    
    # 90% is training, 10% is testing data
    ninety_perct <- round(0.9 * num.rows , 0)
    tr.data <- full.data[1:ninety_perct,]
    ts.data <- full.data[(ninety_perct+1):num.rows,]
   
    col.name <- "j32" #Call function that will give column with max num of ratings
    recom.data <- ts.data
    recom.data[,col.name] <- NA
    
    for (i in 1:nrow(ts.data)) {  
        curr.rec <- ts.data[i,]
        
        if (!is.na(curr.rec[col.name])){
            rec.ratings <- GetRecommendedRatings(recom.data[i,], tr.data, k, col.name, weighted.avg, centered)
            recom.data[i,col.name] <- rec.ratings[col.name]
        }
        if (VERBOSE) print(paste("row ",i,
                                 "orig value ", ts.data[i,col.name],
                                 " curr recommendation", recom.data[i,col.name]))
    }
    
    RMSE.ts.data <- ComputeRMSE(ts.data[,col.name], recom.data[,col.name])
    end <- Sys.time()
    if (VERBOSE) print(end - start)
    
    RMSE.ts.data
}