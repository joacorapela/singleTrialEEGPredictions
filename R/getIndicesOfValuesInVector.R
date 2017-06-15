# For each value in values answer the index of the first element in vector
# closer to the value. Produces a warning if the absolute difference between
# value and the closer element in vector is greater than tol.

getIndicesOfValuesInVector <- function(values, vector, tol=1e-4) {
    indices <- rep(NaN, length(values))
    for(i in 1:length(values)) {
        value <- values[i]
        index <- which.min(abs(vector-value))
        if(abs(vector[index]-value)>tol) {
            warning(sprintf("Could not find an element in vector closer to %f that %f", value, tol))
            browser()
        } else {
            indices[i] <- index
        }
    }
    return(indices)
}
