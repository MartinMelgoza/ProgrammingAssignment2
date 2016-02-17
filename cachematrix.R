## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL     #inverse variable
        set <- function(y) {
                x <<- y
                i <<- NULL    #inverse variable
        }
        get <- function() x
        setmatrix <- function(solve) i <<- solve
        getmatrix <- function() i
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setmatrix(i)
        i
}
