## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a list of functions that
        ## can cache the inverse of a matrix.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<-inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if ( ! is.null(m)) {
                print("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m
}
        
## Test cases

a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b = a$getInverse()
a$get() %*% b
