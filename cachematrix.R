## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# The set function store x as the value 'set' 
# The get function returns the value of x
# The setinv function returns the inverse of the matrix and stores it as 'inv'
# The getinv function returns the inverse

## Write a short comment describing this function
# This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated, 
# cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
# The return of the getinv function is stored in inv
# inv is not null, the message "getting cached result" is printed and the inv is returneed
# If inv is null, the solve function is used to calculate the inverse of the matrix. This value is then set as the inv and returned as inv
