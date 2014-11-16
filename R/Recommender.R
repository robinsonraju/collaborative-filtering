## Approach
"
input.data = Read input from csv
non.na.cols = Find which columns in the input have ratings
trng.data = Read training data from csv (Jester5k dataset downloaded)
similar.recs =  All records in trng.data that have valid values for non.na.cols
knn.recs = Calculate distance from input.data to all the recs in similar.recs, return top 10 (k=10)
For each na.col in na.cols
    value of na.col = average of the value of the knn_records for the same column.
"

## Load Input data & initialize output ##
if (VERBOSE)
    print("Loading Input")
input.data <- ReadCSVFile('input_data.csv')
output.with.ratings <- input.data 
if (VERBOSE)
    print(input.data)

# Find which columns have ratings and which don't. 
input.data.na = input.data[,is.na(input.data)]
input.with.ratings = input.data[,!is.na(input.data)]

## Load Training(historical) data ## 
if (VERBOSE)
    print("Loading Training data - Jester5k")
trng.data <- ReadCSVFile('jester5k-1000.csv')

# Get all records that have valid values for columns in input that have ratings
if (VERBOSE)
    print("Get all records in trng that have valid values in the same columns as input")
trng.with.ratings <- na.omit(trng.data[,colnames(input.with.ratings)])

# Calculate distance from input to all the recs with ratings
# Return top 10 records
if (VERBOSE)
    print("Calculating distance and getting the top 10")
dist.vec <- GetCosineDistVecTopK(input.with.ratings, trng.with.ratings, 10)
closest.trng.recs <- trng.data[names(dist.vec),]

# Loop through the na columns to populate them
if (VERBOSE)
    print("Computing rating for each column in the input that is missing the rating")
for (i in 1:ncol(input.data.na)) {
    col.na <- colnames(input.data.na)[i]
    output.with.ratings[col.na] <- mean(closest.trng.recs[,col.na], na.rm=TRUE)
}

# Recommendation
output.with.recomm <- output.with.ratings[,colnames(input.data.na)]
if (VERBOSE){
    print("NA values in input data filled with predicted ratings")
    print(output.with.recomm)
}

top.10.recomm <- names(sort(output.with.recomm, decreasing=TRUE))[1:10]
if (VERBOSE) {
    print("Recommending top 10 jokes that user has not rated")
    print(top.10.recomm)
}

# Comparison with Recommenderlab package
#r <- Recommender(Jester5k[1:1000], method = "UBCF")
#recom <- predict(r, Jester5k[1001], n=10)
#print(as(recom, "list"))
