> install.packages("recommenderlab")
> library(recommenderlab)
> data(Jester5k)
> Jester5k
5000 x 100 rating matrix of class ‘realRatingMatrix’ with 362106 ratings.
> ratings_matrix <- as(Jester5k, "matrix") 
> write.csv(ratings_matrix, "jester5k.csv")