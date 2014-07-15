## Creates an object incapsulating a matrix and its cached inverse value
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL;
    set <- function(y) {
        x <<- y;
        im <<- NULL;
    }
    get <- function() x
    setinvert <- function(inv) im <<- inv
    getinvert <- function() im
    
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Calculates the Inverse of a matrix stored in the object created by makeCacheMatrix()
## Uses cached value if exist
cacheSolve <- function(x, ...) {
    i <- x$getinvert()
    if (is.null(i)) {
        message("Calculating...")
        i = solve(x$get(), ...)
        x$setinvert(i)
    }
    i;
}
