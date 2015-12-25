## The functions makeCacheMatrix and cacheSolve creates a special matrix,
## calculates its inverse matrix and caches the inverse matrix to
## avoid potentially time-consuming computations on large matrices

## makeCacheMatrix {matrix}
## Create a special matrix that can be used to cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve {matrix}
## Calculate the inverse of the special input matrix. 
## If the cached inverse value is available it will be used directly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        if(!is.null(m)) {
                ## getting cached data
                return(m)
        }
  
        # As cached data is not availble it will be calculated and cached
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
