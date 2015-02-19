## ------------------------------------------------------------------------------------------------------
## Assignment 2: Caching the Inverse of a Matrix
## ------------------------------------------------------------------------------------------------------
## Testing Script:
## size <- 5
## mt <- matrix(rnorm(size^2), nrow=size, ncol=size)
## 
## cache_mt <- makeCacheMatrix(mt)
## mt.1 <- cacheSolve(cache_mt)
## mt.2 <- cacheSolve(cache_mt)
## mt.3 <- cacheSolve(cache_mt)
## ------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------
## Function creates a special "matrix" object that can cache its inverse
## ------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix())
{
   m <- NULL
   
   ## Constructors
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   
   ## Public functions
   setinverse <- function(solve) m <<- solve(x)
   getinverse <- function() m
   
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## ------------------------------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should 
##    retrieve the inverse from the cache.
## ------------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...)
{
   ## Try cached data first
   m <- x$getinverse()
   if(!is.null(m))
   {
      message("getting cached data")
      return(m)
   }

   ## If not there, construct, solve and store (set) it
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   
   ## Return a matrix that is the inverse of 'x'
   m
}
