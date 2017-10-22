## A pair of functions to set/get compute the inverse of A matrix and cache it   
## Two functions are needed: makeCacheMatrix do define getters and setter, and 
## solve to actually solve fot the inverse. It is assumed the matrix is 
## indeed invertible

## A function to define setters and getters for the matrix
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inverseMatrix <- NULL
    set <- function(f)
    {
        x <<- f
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function () inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) 
{
    inv <- x$getInverse()
    if(!is.null(inv)) 
    {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv<<-solve(data)
    x$setinverse(inv)
    inv
}

