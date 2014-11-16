## Util functions used by Recommender system

if (VERBOSE)
    print("Loading fuction - get_cosine_dist")
# Helper function to calculate the cosine similarity between two vectors
get_cosine_dist <- function(x,y) {
    cosine.dist <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(cosine.dist)
}