> install.packages("recommenderlab")
> library(recommenderlab)
> data(Jester5k)
> Jester5k
5000 x 100 rating matrix of class ‘realRatingMatrix’ with 362106 ratings.
> ratings_matrix <- as(Jester5k, "matrix") 
> write.csv(ratings_matrix, "jester5k.csv")

Cold Start Problem
What happens with new users where we have no ratings yet?
Recommend popular items
Have some start-up questions (e.g., "tell me 10 movies you love")
What do we do with new items?
Content-based ltering techniques.
Pay a focus group to rate them