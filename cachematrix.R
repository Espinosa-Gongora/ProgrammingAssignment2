## We use lexical scoping to avoid repeated computations. We use <<- which allows a function to return a value based on the outcome of another function.

## makeCacheMatrix returns a list of objects that are defined in an environment (parent environment) where their value will be accessible for the next function (cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse matrix. First gives a value to the local variable "inv". It looks for "inv" in the parent enclosing environment. If inv differs from NULL, it returns the value (second set of curly brakets). If it is NULL, it creates "inv" and calls the solve function again to compute the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
