## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMinverse <- function(solve) m <<- solve
        getMinverse <- function() m
        list(set = set, get = get,
             setMinverse = setMinverse,
             getMinverse = getMinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMinverse(m)
        m
}


############Below some additional code in order to test that the program is working
############defining an invertible matrix
##a<-matrix(c(3,0,0,0,2,0,1,0,1), nrow = 3, ncol = 3)
##a
##solve(a)

############testing if makeCacheMatrix works
##aMatrix<-makeCacheMatrix(a)
##aMatrix$get()

############testing if cacheSolve works
##cacheSolve (aMatrix)

