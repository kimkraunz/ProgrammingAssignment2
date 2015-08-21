## The makeCacheMatrix and cacheSolve functions return the index of a matrix, first by checking to see if there is a cached value and either returning the cached value or taking the inverse of the matrix and returning it.


makeCacheMatrix <- function(x = matrix()) {
    ## Returns a vector that contains the functions to change the matrix (if        needed), return the matrix, store the value 'm', and return 'm' 
    m <- NULL
    set <- function(y) {                        
        # changes the matrix (if needed) that is stored in the function by              defining 'x' and 'm' to the parent environment.  
        x <<- y                              
        m <<- NULL                              
    }
    
    get <- function() x                         # returns the matrix 'x'
    setinverse <- function(solve) m <<- solve   # stores the value of 'm'
    getinverse <- function() m                  # returns the value of 'm'
    list(set = set, get = get,                  # returns the functions as list
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function returns the inverse of 'x'.  It checks to see whether there is ## a cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()                 # sets 'm'
    if(!is.null(m)) {                   # checks to see whether 'm' is null
        message("getting cached data")
        return(m)                       # if 'm' is not null, returns cached 'm'
    }
    data <- x$get()                     # else returns matrix 'x'
    m <- solve(data, ...)               # returns the inverse of 'x'
    x$setinverse(m)                     # stores the inverse
    m                                   # returns the inverse of 'x' as 'm'
}
