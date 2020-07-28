
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
         ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("No inverse")
                return(i)
        }
        matriz <- x$get()
        i <- solve(matriz, ...)
        x$setInverse(i)
        i
}
## Example
cacheSolve(makeCacheMatrix(matrix(c(5,6,7,8),2,2)))
           