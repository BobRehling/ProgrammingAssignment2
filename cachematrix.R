## Programming Assignment 2
## CReate a set of functions to invert a matrix

## Create a special matrix that can cahe it's inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y)  {
           x <<- y
           i <<- NULL
       }
     get <- function() x
     setinv <- function(solve) i <<- solve
     getinv <- function() i
     list( set=set, get=get, 
          setinv=setinv, getinv=getinv )
}


## Returns the inverse of a matrix. If the inverse has already been calculated, it is 
## retrieved from the cache, otherwise the inverse is computed and stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if( !is.null(i))  {
        message("getting cached data")
        return(i)
    }
    data <-  x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

# testing
a <- matrix(c(1, -1, 1, 1), nrow=2)
makeCacheMatrix(a)
cacheSolve(a)
