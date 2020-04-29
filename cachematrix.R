
# This function creates a special matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$getinverse()
    if (!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
}

# Below we call the function with a matrix call NEW. Compute the inverse, retrive from cache list, change to the inverse
NEW <- matrix(c(1:4),2,2)
NEW1 <- makeCacheMatrix(NEW)
cacheSolve(NEW1) #inverse returned after run cacheSolve on matrix NEW1

