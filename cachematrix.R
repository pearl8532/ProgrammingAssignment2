## The following functions work together to solve, cache, and retrieve the inverse of a square matrix

#makeCacheMatrix creates a special "matrix", which contains functions to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    functions <- list(set = set, get = get, setinv = setinv, getinv = getinv)
    df <- data.frame(matrix(unlist(functions), nrow = 2, byrow = T))
}


#The following function solves the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been solved. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it solves the inverse of the given matrix and sets the value of the inverse in the cache via the serinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    set.mat <- x[[1]][[1]]
    set.inv <- x[[1]][[2]]
    get.mat <- x[[2]][[1]]
    get.inv <- x[[2]][[2]]
    i <- get.inv()
    if(!is.null(i)){
        message("Getting cached data")
        return(i)
    }
    message("Solving inverse and caching")
    data <- get.mat()
    i <- solve(data)
    set.inv(i)
    i
}
