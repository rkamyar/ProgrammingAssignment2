## This program calculates the inverse of a matrix. If the inverse already exists 
## in the cache, then it retrieves the inverse from the cache.

## makeCacheMatrix gets a matrix as input and returns an object with four methods:
## $set(): sets the values of the matrix
## $get(): returns the value of the matrix
## $setInvers(): sets the inverse of the input matrix
## $getInvers(): returns the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvers <- function(solve) m <<- solve
    getInvers <- function() m
    list(set=set, get=get, setInvers=setInvers, getInvers=getInvers)
}


## cacheSolve returns the inverse of its input "matrix" embedded in a list. If the 
## inverse has already been cached, then cacheSolve retrieves it from the cache.  

cacheSolve <- function(x, ...) {
    m<-x$getInvers()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setInvers(m)
    m
}
