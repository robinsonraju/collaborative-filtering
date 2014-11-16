## Approach
"
For each record
    find columns (attributes) that have values
    query_res = run a query to find all records that have values for these attributes
    for each_record in query_res
        compute distance between current_rec and each_record
    for each missing_attribute 
        filter out records from query_res that dont have value for missing_attribute
        value of missing_attribute = weighted average of the value from other records
"

## Load Input data ##
input.data <- ReadCSVFile('input_data.csv')

# Find which columns have ratings and which dont. 
input.data.na = input.data[,is.na(input.data)]
input.with.ratings = input.data[,!is.na(input.data)]

## Load Training(historical) data ## 
trng.data <- ReadCSVFile('jester5k-10.csv')

# Get all records that have valid values for columns in input that have ratings
trng.with.ratings <- na.omit(trng.data[,colnames(input.with.ratings)])

# Calculate distance from input to all the recs with ratings
# Return top 10 records
dv <- GetCosineDistVec(input.with.ratings, trng.with.ratings, 10)

for (i in 1:ncol(input.data.na)) {
    #print(colnames(input.data.na)[i])
    
    
}

