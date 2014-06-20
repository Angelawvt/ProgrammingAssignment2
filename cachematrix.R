## Set of functions that will allow the calculation of an inverse matrix
## to be cached for future use without redoing the actual calculation
## Use: Initialize the matrix cache, m in the example, using makeCacheMatrix(). 
##      Set original matrix m: m$set(<matrix>)
##      Calculate inverse of m: cacheSolve(m)
##      Get the inverse of m: m$getinv()
## Note: Function assumes all matrices used for input are invertible

## Funtion makeCacheMatrix() 
##      Create a named list of functions that will operate on the cache.
##        set: Use to cache the original matrix data
##        get: Use to return the original matrix data, will return NA if not set
##        setinv: Use to cache the inverse of the matrix
##        getinv: Use to return the inverse of the matrix from cache, will 
##              return NULL if not cached
makeCacheMatrix <- function(x = matrix()) {
     # initialize solved (inverted) matrix to NULL
     i <- NULL
     
     # set function will define original matrix, x, in another environment
     # and will initialize inv to NULL. If x is not defined elsewhere, it 
     # will be defined in the global environment
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # get function will return the original matrix another environment. 
     get <- function() x
     
     # setinv will set the inverted matrix to the value of the passed "inv"
     # variable. Note the inversion should be performed using cacheSolve function.
     # i is found in another environment (if undefined, will be set globally)
     setinv <- function(inv) i <<- inv
     
     # getinv will return the matrix inversion from another environment
     getinv <- function() i
     
     # List of functions
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Function cacheSolve
##   To be called using a list, x, created using the makeCacheMatrix function above.
##   Will check the cache to see if the inverse has already been stored and
##   will return the previously solved matrix if found. Otherwise, will 
##   solve the matrix and store in the cache.
cacheSolve <- function(x, ...) {
     ## Get anything stored already as an inverse.
     i <- x$getinv()
     
     # If something is cached, print a message and return that matrix
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     
     # If we get here, nothing is in the cache, so get the original 
     # matrix data and solve (invert), setting the cached inverted matrix
     # to the result for future use
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}
