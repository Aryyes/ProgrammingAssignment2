## The following two functions are used to create a special object that 
## stores a matrix and cache's its mean.

## This function, makeCacheMatrix creates a special "matrix" object, which is 
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and some associated sub-functions/methods
        
        ## define the cache m
        m <- NULL
        set <- function(y) {
                x <<- y # assign the input matrix y to the variable x in the
                # parent environment
                m <<- NULL # re-initialize m in the parent environment to null
        }
        get <- function() x # return the matrix x
        setinverse <- function(inverse) m <<- inverse # set the cache m equal
        # to the inverse of the matrix x
        getinverse <- function() m # return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function, cacheSolve calculates the inverse of the special 
## "matrix" created with the above function. However, it first checks to see 
## if the inverse has already been caclulated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## matrix inverse and sets the value of the inverse in the cache via the 
## 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## Test

## solve(matrix(1:4, 2, 2))
