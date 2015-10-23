##creating matrix inverse function based on mean vector function example

##first step: create special "matrix" object that can cache its inverse
##using solve function to get inverse

makeCacheMatrix <- function(x = matrix()) {   ##setting empty matrix as x default
    CacheInv <- NULL
    set <- function(y) {  ##creating set function
        x <<- y
        CacheInv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) CacheInv <<- solve  ##sets the inverse (CacheInv) to the inverse value
    getinverse <- function() CacheInv   ##returns the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##cahce function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    ##creating cache function
    CacheInv <- x$getinverse()
    if(!is.null(CacheInv)) {    ##if CacheInv (has stored inverse) is not null then it gets cached data
        message("getting cached data")
        return(CacheInv)
    }
    data <- x$get()
    CacheInv <- solve(data, ...)   ##otherwise calculates inverse
    x$setinverse(CacheInv)         ##then sets the inverse
    CacheInv
}


## NOTES TO SELF ##
## suggestion when use makeCacheMatrix: assign it to an object / v<-makeCacheMatrix(matrixhere)
## so when use cacheSolve we can do / cacheSolve(v)
## m <- matrix(1:16, 4, 4) DOES NOT HAVE INVERSE, because determinant of the matrix is 0
## in order to test we need a square matrix like m <- matrix(rnorm(16), 4, 4)
