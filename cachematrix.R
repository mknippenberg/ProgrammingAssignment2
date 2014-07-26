## The two functions below will create a special "matrix" object which 
## can cache it's inverse. This can be used to save on computation if the 
## same matrix needs to have it's inverse calculated frequently.

## This first function makeCacheMatrix contains 4 methods to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## m is the variable that contains the computed inverse matrix
    ## when it is instantiated we set the value to NULL
    m <- NULL
    ## the set function lets us define a new matrix after makeCacheMatrix
    ## has already been instantiated as an object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## the get function simply returns the value of x
    get <- function() x
    ## the setInverse function stores a calculated inverse object into m
    setInverse <- function(inverse) m <<- inverse
    ## the getInverse function simply returns the value of m
    getInverse <- function() m
    ## This is the list/mappings of function names to references
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function performs the inverse matrix calculation 
## The function only computes the inverse matrix if the cached value m
## is NULL

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## m is set to the getInverse value from the makeCacheMatrix object
    m <- x$getInverse()
    ## Check to see if m is NULL
    if(!is.null(m)) {
        ## If m is NOT NULL that implies the result is cached and m is returned
        message("getting cached data")
        return(m)
    }
    ## Otherwise, get the matrix from the makeCacheMatrix object
    data <- x$get()
    ## Pass the matrix into the solve function
    m <- solve(data, ...)
    ## Call the setInverse function to populate the cache
    x$setInverse(m)
    ## Finally return m
    m
}

## A few test matricies
c<-matrix(c(1,-5,0,0,1,0,0,0,1),3,3)
c <- matrix(c(4,3,3,2),2,2)
c
b <- makeCacheMatrix(c)
cacheSolve(b)
