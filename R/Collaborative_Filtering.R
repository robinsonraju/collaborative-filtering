## Load libraries, functions ## 

# Global variables used throughout
projectDir = getwd()
VERBOSE=TRUE
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

############ Executing the functions ################
# Load Input data & initialize output #
if (VERBOSE) print("Loading Input")
input.data <- ReadCSVFile('input_data.csv')
if (VERBOSE) print(input.data)

# Load Training(historical) data #
if (VERBOSE) print("Loading Training data - Jester5k")
trng.data <- ReadCSVFile('jester5k.csv')
if (VERBOSE) print(paste(nrow(trng.data), " records loaded"))

# Calling Recommend function
top.10.recomm <- Recommend(input.data, trng.data, 10, 10)
if (VERBOSE) print("Recommending top 10 jokes that user has not rated")
print(top.10.recomm)

