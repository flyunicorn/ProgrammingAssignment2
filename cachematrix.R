## My function will Cache the Inverse of a Matrix

## The first function, makeCacheMatrix, creates a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
            x <<- y
            m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix created with the first function. 
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)

