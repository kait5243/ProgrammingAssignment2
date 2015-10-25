## The following functions will calculate the inverse of a given
## matrix. For efficiency, the computed matrix inverse will be
## cached so that if the inverse for the same matrix is requested
## again, the function can check for a cached value and return
## it instead of recalculating the inverse

# Establish functions for sending and retrieving matrix inverse
# values from the cache. The cached matrix inverse value will be
# null if no inverse has been stored

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    # Set the matrix and set the cached inverse value to null
    set <- function(y)
    {
         x <<- y
         inv <<- NULL
    }
    
    # Return the original matrix
    get <- function() x
    
    # Set the matrix inverse in the cache
    setinv <- function(invs) inv <<- invs
    
    # Return the matrix inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Return the inverse of a given matrix "x". If this inverse has already
# been computed and cached, the cached value will be returned. Otherwise,
# the inverse will be calculated

cacheSolve <- function(x, ...) 
{
     # Get the stored matrix inverse from the cache
     inv <- x$getinv()
     
     # If the inverse value in the cache is not null, print a
     # message indicating it's retrieved from the cache and
     # return the stored inverse value
     if(!is.null(inv))
     {
          message("getting cached data")
          return(inv)
     }
     
     # If the cached inverse was null, calculate the inverse,
     # save that inverse to the cache, and return it
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
