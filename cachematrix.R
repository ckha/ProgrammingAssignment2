## Put comments here that give an overall description of what your
## functions do
## Since matrix inversion is usally a costly computation, there are some benifit to caching the inverse of a matrix rather than computing it repeatedly. The below pair of below functions cache the inverse of a matrix
## Note that the input matrix should be square and inversible.


## Write a short comment describing this function
## This function create a special "mtrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseM) m <<- inverseM
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix above. 
##If the inverse has already been calculated(and hte matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
