## Two functions for caching the inverse of the matrix

## makeCacheMatrix() take a matrix as argument and returns 
## a cached matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinv <- function(new_inv) {
        inv <<- new_inv
    }
    
    getinv <- function() {
        inv
    }
    
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() function takes a cached matrix and returns the 
## inverse of the matrix if the matrix is nonsingular.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cur_inv <- x$getinv()
    if(is.null(cur_inv)) {
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        return(inv)
    } else {
        message('cacheSolve: retrieving cache')
        return(cur_inv)
    }
}

m <- matrix(rnorm(9), nrow = 3)
solve(m)
m.cache <- makeCacheMatrix(m)
cacheSolve(m.cache)
cacheSolve(m.cache)

