## The functions below create a special object that essentially creates
## a vector pointing to a list of functions, and then cache's its mean.

## The function makeCacheMatrix creates a special "vector" that points
## to a list containing the following functions:
##   set = sets the value of the matrix if it is not yet defined
##   get = obtains the value of the matrix
##   set_inv = computes the value of the matrix inverse
##   get_inv = obtains the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## THe function cacheSolve computes the inverse of the special "vector" 
## created by the above function, using the 'solve' function in R. If the
## inverse has already been computed and its value exists in the cache,
## the function fetches its value from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inv(inv) 							
        inv
}
