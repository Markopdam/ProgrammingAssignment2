## Function makes a matrix and sets m to zero
## Matrix can me retrieved with get, and modified with set
## if inverse (solve) is made m is not longer zero 
## See cacheSolve.R for information about inversing the matrix

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

## Function returns a matrix that is the inverse (=solve) of x
## But only once stored in m, if m is not null than return m with message

cacheSolve <- function(x, ...) {
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
