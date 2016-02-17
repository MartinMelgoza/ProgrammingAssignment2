#This function will create an object saved to the cache
# 1. we can set the value of a matrix 
# 2. get the value of the matrix
# 3. set the inverse of the saved matrix
# 4. retrieve the inverse of the saved matrix

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


# This function will take the above function and will return a inverse matrix if the inverse was cached
# if it was not cached, it will calculate the inverse of the matrix

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
