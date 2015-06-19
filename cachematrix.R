## The functions in this file provide a handy way to cache the inverse of a
## matrix so that it only needs to be calculated once, even if both the
## original matrix and the inverse are used many times.
##
## The cacheSolve function is intended to use a "matrix" created by the 
## makeCacheMatrix function in order to determine the inverse of that matrix.
## The inverse will be cached so that it only need be solved once.
## 
## The makeCacheMatrix function simply wraps a matrix in accessor functions
## that should be used to access the matrix as well as its inverse (but which
## will not solve the inverse matrix automatically).
##
## 
## Examples: 
## > m <- cbind(c(2,0),c(0,2))
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)
## > for (i in 1:10) print(cacheSolve(cm)^i)


## This function creates a special "matrix" that is really a list of wrapper 
## functions that can:
## 'set'        - store the value of a matrix
## 'get'        - retrieve the value of the matrix
## 'setInverse' - cache the inverse of the matrix
## 'getInverse' - retrieve the inverse of the matrix (if it has been cached)
## 
## Args:
##   x: (optional) matrix to be stored initially
##
## Returns:
##   list of wrapper functions, as described above.
##
makeCacheMatrix <- function(x = matrix()) {
        x.inverse <- NULL
        set <- function(new.x) {
                x <<- new.x
                x.inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(new.inverse) x.inverse <<- new.inverse
        getInverse <- function() x.inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function solves the inverse of a special "matrix" created by the
## makeCacheMatrix function, caching the result so that it need only be
## calculated once.
## 
## Args:
##   x:   a "matrix" to be inverted
##   ...: additional arguments to be passed to the "solve" function
##        
## Note: extra args will only be used the first time the inverse is solved.
##
## Returns:
##   the inverse of the "matrix" x
##
cacheSolve <- function(x, ...) {
        # Solve the inverse if we haven't done so already
        x.inverse <- x$getInverse()
        if(is.null(x.inverse)) {
                # message("Need to solve the inverse of x")
                x.inverse <- solve(x$get(), ...)
                x$setInverse(x.inverse)
        }
        ## Return a matrix that is the inverse of 'x'
        x.inverse
}
