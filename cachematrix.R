## makeCacheMatrix maintains the cache of a matrix's
## inverse. cacheSolve computes and caches the inverse
## if it is not already cached.


## Keeps track of x's inverse using get
## and set methods.  If x is changed,
## set the cached inverse to NULL

makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks if x has a cached inverse
## if so, return it
## if not, compute the inverse, cache it, and return it

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}