## The functions aim at reducing calculation time of a matrix inverse 
## by getting cached data. If the inverse is calculated before, the function will
## get the cached data directly without calculating it repeatedly. 

## makeCacheMatrix function stores a list of function to store 
## cached matrix inverse. set() gives x and m the original value.
## get() is a function to retrieve x. setsolve() store the value of its variable 
## to m. getsolve() get the value of m. These matrices are used in the next fuction.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(nrow=0,ncol=0)
    set <- function(y) {
        x <<- y
        m <<- matrix(nrow=0,ncol=0)
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## if the matrix inverse is calculated and stored, the function will return
## the cache inverse matrix and a message"getting cached data". If it is not
## calculated or stored, the function will obtain the matrix, calculate the 
## inverse, store it in the cache and return the inverse. 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(nrow(m)!=0 && ncol(m)!=0) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m             ## Return a matrix that is the inverse of 'x'
}
