## Helper function to compute average ##
# Calls weighted average function if weighted.avg boolean value is true
# Otherwise, finds mean.
# Example:
# > sim.vec <- seq(1, 0.1, by = -0.1)
# > records <- data.frame(matrix(1:40, nrow=10))
# > colnames(records) <- c("c1", "c2", "c3", "c4")
# > Average(sim.vec, records, "c1", FALSE)
# [1] 5.5
# > Average(sim.vec, records, "c1", TRUE)
# [1] 0.4
Average <- function(sim.vec, records, avg.col, weighted.avg=FALSE) {
    if (weighted.avg) {
        WeightedAverage(sim.vec, records, avg.col)
    } else {
        mean(records[,avg.col], na.rm=TRUE)
    }
}

## Helper function to compute weighted average
# Input : sim.vec - vector with similarity scores
#           records - data frame with values
#           avg.col - column for which weighted average needs to be found
# Example:
# > sim.vec <- seq(1, 0.1, by = -0.1)
# > records <- data.frame(matrix(1:40, nrow=10))
# > colnames(records) <- c("c1", "c2", "c3", "c4")
# > WeightedAverage(sim.vec, records, "c1")
# [1] 0.4
WeightedAverage <- function(sim.vec, records, avg.col) {
    s <- sum(records[,avg.col], na.rm=TRUE)
    p <- 0
    for(i in 1:nrow(records)) {
        p <- p + (sim.vec[i] * records[i, avg.col])
    }
    p / s
}