## Load libraries, functions ## 

# Global variables used throughout
projectDir = getwd()
VERBOSE = TRUE
codeDir = file.path(projectDir, 'R')
dataDir = file.path(projectDir, 'data')
outputDir = file.path(projectDir, 'output')

## Load libraries
if (VERBOSE) print("Loading libraries and functions for project")
library(recommenderlab) #  recommenderlab for comparison
data(Jester5k) # load Jester dataset

## Load functions
source(file.path(codeDir, 'Recommender_util.R'))
source(file.path(codeDir, 'Recommender.R'))

if (VERBOSE) print("Completed loading libraries & functions")

#Settings for this run
K = 10 
WEIGHTED_AVG = FALSE
CENTERED_DIST = TRUE 
NUM_RECOMMS = 10 

############ Executing the functions ################
# Load Input data & initialize output #
if (VERBOSE) print("Loading Input")
input.data <- ReadCSVFile('input_data.csv')
if (VERBOSE) print(input.data)

# Load Training(historical) data #
if (VERBOSE) print("Loading Training data - Jester5k")
trng.data <- ReadCSVFile('jester5k-1000.csv')
if (VERBOSE) print(paste(nrow(trng.data), " records loaded"))

# Calling Recommend function
if (VERBOSE) print(paste("Recommending top", NUM_RECOMMS, "jokes that user has not rated"))
top.n.recomm <- Recommend(input.data, trng.data, K, WEIGHTED_AVG, CENTERED_DIST, NUM_RECOMMS)

# Calling RMSE function 
if (VERBOSE) print("Computing RMSE by taking 10% of values")
RMSE.values <- trng.RMSE.short(trng.data,K, WEIGHTED_AVG, CENTERED_DIST)

# Print output
if (VERBOSE) {
    print("Settings for this run")
    print(paste("K:", K, 
                "WEIGHTED_AVG:", WEIGHTED_AVG, 
                "CENTERED_DIST", CENTERED_DIST, 
                "NUM_RECOMMS", NUM_RECOMMS))
}

print("top n recommendations")
print(top.n.recomm)

print("RMSE")
print(RMSE.values)
