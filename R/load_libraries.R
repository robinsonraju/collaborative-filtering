# 
# Load libraries, functions
#

# Global variables used throughout
projectDir = getwd()
VERBOSE=TRUE
codeDir = file.path(projectDir, 'R')
dataDir = file.path(projectDir, 'data')
outputDir = file.path(projectDir, 'output')

if (VERBOSE)
    print("Loading libraries and functions for project")

## Load libraries
# None for now. 

## Load utilities
# File that contains utilities
source(file.path(codeDir, 'utils.R'))

if (VERBOSE)
    print("Completed loading libraries & functions")